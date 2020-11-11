open C_literals;;
open C_semantics;;
open Position;;

module type NamingType = sig
	module Literals: LiteralsType
	module Semantics: SemanticsType
		with module Literals := Literals
	
	val filename_mapping: (string -> string) -> (string -> string) ->
		Semantics.language_mapping ->
		(Semantics.source_item list * Semantics.extra_info) StringMap.t ->
		(string * string) StringMap.t
	
	val items_per_module: Semantics.language_mapping ->
		(string * string) StringMap.t ->
		(Semantics.source_item list * Semantics.extra_info) StringMap.t ->
		Semantics.source_item list StringMap.t
	
	type name_mapping_per_module =
		(string * bool) StringMap.t * string StringMap.t * string StringMap.t *
			string StringMap.t
	
	val add:
		[> Semantics.opaque_type_var | Semantics.non_opaque_type_var
			| `defined_expression of Semantics.expression] ->
		string -> string -> name_mapping_per_module -> name_mapping_per_module
	
	val mem: [> Semantics.opaque_type_var] -> string -> name_mapping_per_module ->
		bool
	
	val find: [> Semantics.opaque_type_var] -> string -> name_mapping_per_module ->
		string
	
	type name_mapping = (string * string * name_mapping_per_module) StringMap.t
	
	val module_of: ranged_position -> name_mapping -> string
	
	val is_hidden: ranged_position -> Semantics.named_item -> name_mapping -> bool
	
	val name_mapping: long_f:(string -> string) -> short_f:(string -> string) ->
		foldcase:(string -> string) -> string StringMap.t StringMap.t ->
		(string * string) StringMap.t -> Semantics.opaque_mapping ->
		Semantics.source_item list StringMap.t -> name_mapping
	
	val add_name_mapping_for_arguments: long_f:(string -> string) ->
		short_f:(string -> string) -> anonymous_f:(int -> string) ->
		Semantics.variable list -> name_mapping ->
		name_mapping * (string * Semantics.variable) list
	
	val name_mapping_for_struct_items: long_f:(string -> string) ->
		short_f:(string -> string) -> anonymous_f:(int -> string) ->
		Semantics.struct_item list ->
		string StringMap.t * (string * Semantics.struct_item) list
end;;

module Naming
	(Literals: LiteralsType)
	(Semantics: SemanticsType
		with module Literals := Literals)
	: NamingType
		with module Literals := Literals
		with module Semantics := Semantics =
struct
	open Semantics;;
	
	let find_by_relative_path
		(relative_filename: string)
		(filename_mapping: (string * string) StringMap.t)
		: (string * string * string) option =
			(* header-filename, header-filename(relative), module-name *)
	(
		let result = ref None in
		let (_: bool) =
			StringMap.exists (fun k (rk, v) ->
				if rk = relative_filename then (
					result := Some (k, rk, v);
					true (* break *)
				) else (
					false
				)
			) filename_mapping
		in
		!result
	);;
	
	let filename_mapping
		(module_name_of_h: string -> string)
		(remove_include_dir: string -> string)
		(language_mapping: language_mapping)
		(sources: (source_item list * extra_info) StringMap.t)
		: (string * string) StringMap.t = (* header-filename -> header-filename(relative) * module-name *)
	(
		let result =
			StringMap.fold (fun k _ r ->
				let rk = remove_include_dir k in
				let h =
					let rec find_loop xs = (
						begin match xs with
						| (h1, h2) :: xr ->
							if h2 = rk then h1 else
							find_loop xr
						| [] ->
							rk
						end
					) in
					find_loop language_mapping.lm_monolithic_include
				in
				let module_name = module_name_of_h h in
				StringMap.add k (rk, module_name) r
			) sources StringMap.empty
		in
		(* for #pragma for-include making aliases into blank header *)
		List.fold_left (fun r (k, _) ->
			begin match find_by_relative_path k result with
			| Some _ ->
				r
			| None ->
				let module_name = module_name_of_h k in
				StringMap.add k (k, module_name) r
			end
		) result language_mapping.lm_include
	);;
	
	let items_per_module
		(language_mapping: language_mapping)
		(filename_mapping: (string * string) StringMap.t)
		(sources: (source_item list * extra_info) StringMap.t)
		: source_item list StringMap.t =
	(
		let rec include_loop
			(items_per_module: source_item list StringMap.t)
			: source_item list StringMap.t =
		(
			let result =
				List.fold_left (fun (items_pp: source_item list StringMap.t) (file1, file2) ->
					begin match
						find_by_relative_path file1 filename_mapping,
						find_by_relative_path file2 filename_mapping
					with
					| Some (h, _, module1), Some (source_h, _, module2) ->
						let source_items: source_item list = StringMap.find module2 items_pp in
						StringMap.update module1 (fun e ->
							let dest_items: source_item list = Option.value ~default:[] e in
							let added_dest_items =
								List.fold_right (fun item (added_dest_items: source_item list) ->
									begin match item with
									| `named (_, _, `defined_alias _, _)
									| `anonymous_alias _ ->
										added_dest_items (* does not chain *)
									| `named (_, name, _, _) as item ->
										if
											List.exists (fun (i: source_item) ->
												begin match i with
												| `named (_, n2, `defined_alias i2, _) ->
													n2 = name && i2 == item
												| _ ->
													false
												end
											) added_dest_items
										then added_dest_items else (
											let alias =
												let p = h, 0, 0, 0 in
												let ps = p, p in
												`named (ps, name, `defined_alias item, no_attributes)
											in
											alias :: added_dest_items
										)
									| `function_type _ as item ->
										if
											List.exists (fun (i: source_item) ->
												begin match i with
												| `anonymous_alias (_, i2) ->
													i2 == item
												| _ ->
													false
												end
											) added_dest_items
										then added_dest_items else (
											let alias =
												let source_p = source_h, 0, 0, 0 in
												let source_ps = source_p, source_p in
												`anonymous_alias (source_ps, item)
											in
											alias :: added_dest_items
										)
									| #anonymous_type | `include_point _ ->
										added_dest_items
									end
								) source_items dest_items
							in
							if added_dest_items != dest_items then (
								Some added_dest_items
							) else (
								e (* already included *)
							)
						) items_pp
					| _, _ ->
						items_pp (* destination/source header was not found *)
					end
				) items_per_module language_mapping.lm_include
			in
			if result != items_per_module then include_loop result else
			result
		) in
		(* separate items per filename *)
		let result_per_filename =
			StringMap.fold (fun k (items, _) r ->
				if items = [] then r else
				let rel_filename, module_name = StringMap.find k filename_mapping in
				StringMap.update rel_filename (fun e ->
					begin match e with
					| Some (_, items2) ->
						(* same filenames are possible by #pragma include_next *)
						let rec insert_loop_1 xs ys = (
							begin match xs with
							| (`include_point h_filename as x) :: xr when fst (StringMap.find h_filename filename_mapping) = rel_filename ->
								List.rev_append ys (x :: List.append items2 xr) (* items would include_next items2 *)
							| x :: xr ->
								insert_loop_1 xr (x :: ys)
							| [] ->
								List.append items items2 (* the order is not clear *)
							end
						) and insert_loop_2 xs ys = (
							begin match xs with
							| (`include_point h_filename as x) :: xr when fst (StringMap.find h_filename filename_mapping) = rel_filename ->
								List.rev_append ys (x :: List.append items xr) (* items2 would include_next items *)
							| x :: xr ->
								insert_loop_2 xr (x :: ys)
							| [] ->
								insert_loop_1 items [] (* items may include_next items2 *)
							end
						) in
						let xs = insert_loop_2 items2 [] in
						Some (module_name, xs)
					| None ->
						Some (module_name, items)
					end
				) r
			) sources StringMap.empty
		in
		(* #pragma for-monolithic_include inserting items to `include_point _ *)
		let result_per_filename =
			List.fold_left (fun result_per_filename (includer, included) ->
				begin match
					StringMap.find_opt includer result_per_filename,
					StringMap.find_opt included result_per_filename
				with
				| Some (module_name, includer_items), Some (_, included_items) ->
					let rec insert_loop xs ys = (
						begin match xs with
						| (`include_point h_filename as x) :: xr when fst (StringMap.find h_filename filename_mapping) = included ->
							let merged_items = List.rev_append ys (x :: List.append included_items xr) in
							StringMap.add includer (module_name, merged_items) (StringMap.remove included result_per_filename)
						| x :: xr ->
							insert_loop xr (x :: ys)
						| [] ->
							result_per_filename (* not found, no inserting *)
						end
					) in
					insert_loop includer_items []
				| _, _ ->
					result_per_filename
				end
			) result_per_filename language_mapping.lm_monolithic_include
		in
		(* re-map per module-name *)
		let result_per_module =
			StringMap.fold (fun _ (module_name, items) result_per_module ->
				assert (items <> []);
				StringMap.update module_name (fun e ->
					begin match e with
					| Some xs ->
						let xs = List.append items xs in
						Some xs
					| None ->
						Some items
					end
				) result_per_module
			) result_per_filename StringMap.empty
		in
		(* #pragma for-include making aliases *)
		include_loop result_per_module
	);;
	
	type name_mapping_per_module =
		(string * bool) StringMap.t * (* bool means hidden by macro *)
		string StringMap.t * (* enum *)
		string StringMap.t * (* struct *)
		string StringMap.t;; (* union *)
	
	let empty_name_mapping_per_module =
		StringMap.empty,
		StringMap.empty,
		StringMap.empty,
		StringMap.empty;;
	
	let add
		(kind: [> opaque_type_var | non_opaque_type_var | `defined_expression of expression])
		(key: string)
		(value: string)
		(map: name_mapping_per_module)
		: name_mapping_per_module =
	(
		let nmap, emap, smap, umap = map in
		begin match kind with
		| `opaque_enum | `enum _ ->
			nmap, StringMap.add key value emap, smap, umap
		| `opaque_struct | `struct_type _ ->
			nmap, emap, StringMap.add key value smap, umap
		| `opaque_union | `union _ ->
			nmap, emap, smap, StringMap.add key value umap
		| `defined_expression _ ->
			StringMap.add key (value, true) nmap, emap, smap, umap
		| _ ->
			StringMap.add key (value, false) nmap, emap, smap, umap
		end
	);;
	
	let mem
		(kind: [> opaque_type_var])
		(key: string)
		(map: name_mapping_per_module)
		: bool =
	(
		let nmap, emap, smap, umap = map in
		begin match kind with
		| `opaque_enum ->
			StringMap.mem key emap
		| `opaque_struct ->
			StringMap.mem key smap
		| `opaque_union ->
			StringMap.mem key umap
		| _ ->
			StringMap.mem key nmap
		end
	);;
	
	let find
		(kind: [> opaque_type_var])
		(key: string)
		(map: name_mapping_per_module)
		: string =
	(
		let nmap, emap, smap, umap = map in
		begin match kind with
		| `opaque_enum ->
			StringMap.find key emap
		| `opaque_struct ->
			StringMap.find key smap
		| `opaque_union ->
			StringMap.find key umap
		| _ ->
			fst (StringMap.find key nmap)
		end
	);;
	
	let is_hidden_per_module
		(item: named_item)
		(map: name_mapping_per_module)
		: bool =
	(
		let `named (_, key, kind, _) = item in
		let nmap, _, _, _ = map in
		begin match kind with
		| `extern (#function_type, _) ->
			snd (StringMap.find key nmap)
		| _ ->
			false
		end
	);;
	
	let name_mapping_per_module
		~(long_f: string -> string)
		~(short_f: string -> string)
		~(foldcase: string -> string)
		(special_map: string StringMap.t)
		(opaque_mapping: opaque_mapping)
		(items: source_item list)
		: name_mapping_per_module =
	(
		let prefix_for_esu (name: string) (kind: named_var): string = (
			begin match kind with
			| `opaque_enum | `enum _ -> "enum_" ^ name
			| `opaque_struct | `struct_type _ -> "struct_" ^ name
			| `opaque_union | `union _ -> "union_" ^ name
			| _ -> name
			end
		) in
		let opaque_is_in_same (item: non_opaque_type): bool = (
			let opaque = full_to_opaque item opaque_mapping in
			let `named (((o_filename, _, _, _), _), _, _, _) = opaque in
			let `named (((f_filename, _, _, _), _), _, _, _) = item in
			o_filename = f_filename
		) in
		(* long name phase *)
		let pair =
			List.fold_left (fun (result, rev as pair) item ->
				begin match item with
				| #anonymous_type | `include_point _ | `anonymous_alias _
				| `named (_, _, `defined_alias (`named (_, _, (`enum _ | `struct_type _ | `union _), _)), _) ->
					pair
				| `named (_, _, (`enum _ | `struct_type _ | `union _), _) as esu when opaque_is_in_same esu ->
					pair
				| `named (_, name, `defined_alias (`named (_, _, kind, _)), _)
				| `named (_, name, kind, _) ->
					let l_name =
						begin match StringMap.find_opt name special_map with
						| Some s_item ->
							s_item
						| None ->
							let s = long_f name in
							prefix_for_esu s kind
						end
					in
					let result = add kind name l_name result in
					let rev = StringMap.add (foldcase l_name) `origin rev in
					result, rev
				end
			) (empty_name_mapping_per_module, StringMap.empty) items
		in
		(* short name phase *)
		let result, _ =
			List.fold_left (fun (result, rev as pair) item ->
				begin match item with
				| #anonymous_type | `include_point _ | `anonymous_alias _
				| `named (_, _, `defined_alias (`named (_, _, (`enum _ | `struct_type _ | `union _), _)), _) ->
					pair
				| `named (_, _, (`enum _ | `struct_type _ | `union _), _) as esu when opaque_is_in_same esu ->
					pair
				| `named (_, name, `defined_alias (`named (_, _, kind, _)), _)
				| `named (_, name, kind, _) as item ->
					if StringMap.mem name special_map
						|| ( (* do not use short name for like __XXX__ *)
							let name_length = String.length name in
							name_length >= 4
							&& name.[0] = '_'
							&& name.[1] = '_'
							&& name.[name_length - 2] = '_'
							&& name.[name_length - 1] = '_')
					then pair else
					let s_name =
						let s = short_f name in
						prefix_for_esu s kind
					in
					let u = foldcase s_name in
					begin match StringMap.find_opt u rev with
					| Some e ->
						begin match e with
						| `origin | `duplicated ->
							pair
						| `short (before_name, before_kind) ->
							let before_name_length = String.length before_name in
							let name_length = String.length name in
							if before_name_length < name_length then (
								(* the previous one is shorter *)
								pair
							) else if before_name_length > name_length then (
								(* restore the previous one *)
								let l_name = long_f before_name in
								let result = add before_kind before_name l_name result in
								(* new one is shorter *)
								let result = add kind name s_name result in
								let rev = StringMap.add u (`short (name, kind)) rev in
								result, rev
							) else ( (* before_name_length = name_length *)
								let l_name = long_f before_name in
								let result = add before_kind before_name l_name result in
								let rev = StringMap.add u `duplicated rev in
								result, rev
							)
						end
					| None ->
						if is_hidden_per_module item result then (
							pair (* to use long name for macro *)
						) else (
							let result = add kind name s_name result in
							let rev = StringMap.add u (`short (name, kind)) rev in
							result, rev
						)
					end
				end
			) pair items
		in
		result
	);;
	
	type name_mapping = (string * string * name_mapping_per_module) StringMap.t;;
	(* header-filename(relative) * module-name * for-items *)
	
	let module_of
		(ps: ranged_position)
		(name_mapping: name_mapping)
		: string =
	(
		let (filename, _, _, _), _ = ps in
		let _, module_name, _ = StringMap.find filename name_mapping in
		module_name
	);;
	
	let is_hidden
		(ps: ranged_position)
		(item: named_item)
		(name_mapping: name_mapping)
		: bool =
	(
		let (filename, _, _, _), _ = ps in
		let _, _, map = StringMap.find filename name_mapping in
		is_hidden_per_module item map
	);;
	
	let name_mapping
		~(long_f: string -> string)
		~(short_f: string -> string)
		~(foldcase: string -> string)
		(special_name_mapping: string StringMap.t StringMap.t)
		(filename_mapping: (string * string) StringMap.t)
		(opaque_mapping: opaque_mapping)
		(items_per_module: source_item list StringMap.t)
		: name_mapping =
	(
		let name_mapping_per_module =
			StringMap.mapi (fun module_name items ->
				let special_map =
					Option.value ~default:StringMap.empty
						(StringMap.find_opt module_name special_name_mapping)
				in
				name_mapping_per_module ~long_f ~short_f ~foldcase special_map opaque_mapping items
			) items_per_module
		in
		StringMap.mapi (fun k _ ->
			let rel_filename, module_name = StringMap.find k filename_mapping in
			let name_mapping_per_module =
				Option.value ~default:empty_name_mapping_per_module
					(StringMap.find_opt module_name name_mapping_per_module)
			in
			rel_filename, module_name, name_mapping_per_module
		) filename_mapping
	);;
	
	let add_name_mapping_for_arguments
		~(long_f: string -> string)
		~(short_f: string -> string)
		~(anonymous_f: int -> string)
		(args: variable list)
		(name_mapping: name_mapping)
		: name_mapping * (string * variable) list =
	(
		let (_: string -> string) = long_f in (* ignore... currently, no care for confliction *)
		let rec loop index xs name_mapping rs = (
			begin match xs with
			| [] ->
				name_mapping, List.rev rs
			| x :: xr ->
				let `named (ps, name, _, _) = x in
				if name = "" then (
					let a_name = anonymous_f index in
					loop (index + 1) xr name_mapping ((a_name, x) :: rs)
				) else (
					let a_name = short_f name in
					let (filename, _, _, _), _ = ps in
					let name_mapping =
						StringMap.update filename (fun e ->
							begin match e with
							| None ->
								assert false
							| Some (rel_filename, mn, nmpm) ->
								let nmpm = add `namespace name a_name nmpm in
								Some (rel_filename, mn, nmpm)
							end
						) name_mapping
					in
					loop (index + 1) xr name_mapping ((a_name, x) :: rs)
				)
			end
		) in
		loop 1 args name_mapping []
	);;
	
	let name_mapping_for_struct_items
		~(long_f: string -> string)
		~(short_f: string -> string)
		~(anonymous_f: int -> string)
		(items: struct_item list)
		: string StringMap.t * (string * struct_item) list =
	(
		let rec loop anonymous_index xs map rs = (
			begin match xs with
			| [] ->
				map, List.rev rs
			| x :: xr ->
				let name, _, _, _ = x in
				if name = "" then (
					let an = anonymous_f anonymous_index in
					let rs = (an, x) :: rs in
					loop (anonymous_index + 1) xr map rs
				) else (
					let sn = short_f name in
					let map, rs =
						begin match Listtbl.assocs String.equal sn rs with
						| (_, (prev_name, _, _, _)) :: _ ->
							let prev_ln = long_f prev_name in
							let ln = long_f name in
							let map = StringMap.add prev_name prev_ln map in
							let map = StringMap.add name ln map in
							let rs =
								List.map (fun (item_name, item as z) ->
									if item_name <> sn then z else
									prev_ln, item
								) rs
							in
							let rs = (ln, x) :: rs in
							map, rs
						| [] ->
							let map = StringMap.add name sn map in
							let rs = (sn, x) :: rs in
							map, rs
						end
					in
					loop anonymous_index xr map rs
				)
			end
		) in
		loop 1 items StringMap.empty []
	);;
	
end;;
