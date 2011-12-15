open C_semantics;;
open Position;;
open Value;;

module Naming
	(Literals: LiteralsType)
	(Semantics: SemanticsType (Literals).S) =
struct
	open Semantics;;
	
	let find_by_relative_path
		(remove_include_dir: string -> string)
		(relative_filename: string)
		(filename_mapping: string StringMap.t)
		: string * string = (* header-filename, module-name *)
	(
		let module Local = struct exception Break of (string * string) end in
		begin try
			let (_: string StringMap.t) =
				StringMap.filter (fun k v ->
					if remove_include_dir k = relative_filename then (
						raise (Local.Break (k, v))
					) else (
						false
					)
				) filename_mapping
			in
			raise Not_found
		with Local.Break result ->
			result
		end
	);;
	
	let filename_mapping
		(module_name_of_h: (string -> string) -> string -> string)
		(remove_include_dir: string -> string)
		(language_mapping: language_mapping)
		(sources: (source_item list * extra_info) StringMap.t)
		: string StringMap.t = (* header-filename -> module-name *)
	(
		let result =
			StringMap.fold (fun k _ r ->
				let module_name = module_name_of_h remove_include_dir k in
				StringMap.add k module_name r
			) sources StringMap.empty
		in
		List.fold_left (fun r (k, _) ->
			begin try
				let (_, _: string * string) = find_by_relative_path remove_include_dir k result in
				r
			with Not_found ->
				let module_name = module_name_of_h (fun x -> x) k in
				StringMap.add k module_name r
			end
		) result language_mapping.lm_include
	);;
	
	let items_per_module
		(remove_include_dir: string -> string)
		(language_mapping: language_mapping)
		(filename_mapping: string StringMap.t)
		(sources: (source_item list * extra_info) StringMap.t)
		: source_item list StringMap.t =
	(
		let rec include_loop
			(items_per_module: source_item list StringMap.t)
			: source_item list StringMap.t =
		(
			let result =
				List.fold_left (fun (items_pp: source_item list StringMap.t) (file1, file2) ->
					begin try
						let h, module1 = find_by_relative_path remove_include_dir file1 filename_mapping in
						let _, module2 = find_by_relative_path remove_include_dir file2 filename_mapping in
						let source_items: source_item list = StringMap.find module2 items_pp in
						let dest_items: source_item list =
							begin try
								StringMap.find module1 items_pp
							with Not_found ->
								[]
							end
						in
						let added_dest_items =
							List.fold_right (fun item (added_dest_items: source_item list) ->
								begin match item with
								| `named (_, _, `defined_alias _, _) ->
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
								| #anonymous_type ->
									added_dest_items
								end
							) source_items dest_items
						in
						if added_dest_items != dest_items then (
							StringMap.add module1 added_dest_items items_pp
						) else (
							items_pp (* already included *)
						)
					with Not_found ->
						items_pp (* destination/source header was not found *)
					end
				) items_per_module language_mapping.lm_include
			in
			if result != items_per_module then include_loop result else
			result
		) in
		include_loop (
			StringMap.fold (fun k (items, _) r ->
				if items = [] then r else
				let module_name = StringMap.find k filename_mapping in
				begin try
					let xs = StringMap.find module_name r in
					let xs = List.append items xs in
					StringMap.add module_name xs r
				with Not_found ->
					StringMap.add module_name items r
				end
			) sources StringMap.empty
		)
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
				| #anonymous_type
				| `named (_, _, `defined_alias (`named (_, _, (`enum _ | `struct_type _ | `union _), _)), _) ->
					pair
				| `named (_, _, (`enum _ | `struct_type _ | `union _), _) as esu when opaque_is_in_same esu ->
					pair
				| `named (_, name, `defined_alias (`named (_, _, kind, _)), _)
				| `named (_, name, kind, _) ->
					let l_name =
						begin try
							StringMap.find name special_map
						with Not_found ->
							let s = long_f name in
							prefix_for_esu s kind
						end
					in
					let result = add kind name l_name result in
					let rev = StringMap.add (String.uppercase l_name) `origin rev in
					result, rev
				end
			) (empty_name_mapping_per_module, StringMap.empty) items
		in
		(* short name phase *)
		let result, _ =
			List.fold_left (fun (result, rev as pair) item ->
				begin match item with
				| #anonymous_type
				| `named (_, _, `defined_alias (`named (_, _, (`enum _ | `struct_type _ | `union _), _)), _) ->
					pair
				| `named (_, _, (`enum _ | `struct_type _ | `union _), _) as esu when opaque_is_in_same esu ->
					pair
				| `named (_, name, `defined_alias (`named (_, _, kind, _)), _)
				| `named (_, name, kind, _) as item ->
					if StringMap.mem name special_map then pair else
					let s_name =
						let s = short_f name in
						prefix_for_esu s kind
					in
					let u = String.uppercase s_name in
					if StringMap.mem u rev then (
						begin match StringMap.find u rev with
						| `origin | `duplicated ->
							pair
						| `short (before_name, before_kind) ->
							let l_name = long_f before_name in
							let result = add before_kind before_name l_name result in
							let rev = StringMap.add u `duplicated rev in
							result, rev
						end
					) else if is_hidden_per_module item result then (
						pair (* to use short name for macro *)
					) else (
						let result = add kind name s_name result in
						let rev = StringMap.add u (`short (name, kind)) rev in
						result, rev
					)
				end
			) pair items
		in
		result
	);;
	
	type name_mapping = (string * name_mapping_per_module) StringMap.t;;
	
	let module_of
		(ps: ranged_position)
		(name_mapping: name_mapping)
		: string =
	(
		let (filename, _, _, _), _ = ps in
		let module_name, _ = StringMap.find filename name_mapping in
		module_name
	);;
	
	let is_hidden
		(ps: ranged_position)
		(item: named_item)
		(name_mapping: name_mapping)
		: bool =
	(
		let (filename, _, _, _), _ = ps in
		let _, map = StringMap.find filename name_mapping in
		is_hidden_per_module item map
	);;
	
	let name_mapping
		~(long_f: string -> string)
		~(short_f: string -> string)
		(special_name_mapping: string StringMap.t StringMap.t)
		(filename_mapping: string StringMap.t)
		(opaque_mapping: opaque_mapping)
		(items_per_module: source_item list StringMap.t)
		: name_mapping =
	(
		let name_mapping_per_module =
			StringMap.mapi (fun module_name items ->
				let special_map =
					begin try
						StringMap.find module_name special_name_mapping
					with Not_found ->
						StringMap.empty
					end
				in
				name_mapping_per_module ~long_f ~short_f special_map opaque_mapping items
			) items_per_module
		in
		StringMap.mapi (fun k _ ->
			let module_name = StringMap.find k filename_mapping in
			let name_mapping_per_module =
				begin try
					StringMap.find module_name name_mapping_per_module
				with Not_found ->
					empty_name_mapping_per_module
				end
			in
			module_name, name_mapping_per_module
		) filename_mapping
	);;
	
	let add_name_mapping_for_arguments
		~(long_f: string -> string)
		~(short_f: string -> string)
		(args: variable list)
		(name_mapping: name_mapping)
		: name_mapping =
	(
		let name_mapping, _ =
			List.fold_left (fun (name_mapping, i) arg ->
				let `named (ps, name, _, _) = arg in
				let name_mapping =
					if name = "" then (
						name_mapping
					) else (
						let (filename, _, _, _), _ = ps in
						let mn, nmpm = StringMap.find filename name_mapping in
						let a_name =
							let a_name = short_f name in
							(* avoiding confliction with type name *)
							if a_name = name
								&& (let nmap, _, _, _ = nmpm in
								StringMap.mem (String.uppercase name) nmap)
							then (
								a_name
							) else (
								long_f name
							)
						in
						let nmpm = add `namespace name a_name nmpm in
						StringMap.add filename (mn, nmpm) name_mapping
					)
				in
				name_mapping, (i + 1)
			) (name_mapping, 1) args
		in
		name_mapping
	);;
	
	let name_mapping_for_struct_items
		~(long_f: string -> string)
		~(short_f: string -> string)
		~(anonymous_f: int -> string)
		(items: struct_item list)
		: string StringMap.t * (string * struct_item) list =
	(
		let _ = long_f in (* ignore... currently, no care for confliction *)
		let rec loop anonymous_index map rs xs = (
			begin match xs with
			| [] ->
				map, List.rev rs
			| x :: xr ->
				let name, _, _, _ = x in
				if name = "" then (
					let an = anonymous_f anonymous_index in
					let rs = (an, x) :: rs in
					loop (anonymous_index + 1) map rs xr
				) else (
					let sn = short_f name in
					let map = StringMap.add name sn map in
					let rs = (sn, x) :: rs in
					loop anonymous_index map rs xr
				)
			end
		) in
		loop 1 StringMap.empty [] items
	);;
	
end;;
