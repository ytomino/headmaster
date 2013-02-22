open Ada_format;;
open Ada_naming;;
open C_filename;;
open C_literals;;
open C_semantics;;
open C_semantics_dependency;;
open C_semantics_finding;;
open C_semantics_naming;;
open Position;;

let string_of_pp (pp: Format.formatter -> 'a -> unit) (v: 'a): string = (
	let b = Buffer.create 256 in
	let f = Format.formatter_of_buffer b in
	pp f v;
	Format.pp_print_flush f ();
	Buffer.contents b
);;

let omit_long_word max_length s = (
	if String.length s <= max_length then s else
	String.sub s 0 (max_length - 3) ^ "..."
);;

module AdaTranslator
	(Literals: LiteralsType)
	(Semantics: SemanticsType
		with module Literals := Literals) =
struct
	module Dependency = Dependency (Literals) (Semantics);;
	module Finding = Finding (Literals) (Semantics);;
	module Naming = Naming (Literals) (Semantics);;
	open Literals;;
	open Format;;
	
	(* name mapping *)
	
	let filename_without_ext_of_package_name (name: string): string = (
		if name = "" then "c" else
		let s = String.copy name in
		for i = 0 to String.length s - 1 do
			if s.[i] = '.' then s.[i] <- '-'
		done;
		"c-" ^ s
	);;
	
	let spec_filename (name: string): string = (
		filename_without_ext_of_package_name (String.lowercase name) ^ ".ads"
	);;
	
	let body_filename (name: string): string = (
		filename_without_ext_of_package_name (String.lowercase name) ^ ".adb"
	);;
	
	let filename_mapping = Naming.filename_mapping ada_package_name;;
	
	let dir_packages
		(filename_mapping: (string * string) StringMap.t)
		: string list =
	(
		let add_parent package rs = (
			begin try
				let rindex = String.rindex package '.' in
				let dir = String.sub package 0 rindex in
				if List.mem dir rs then rs else dir :: rs
			with Not_found ->
				rs
			end
		) in
		let rec loop xs = (
			let rs =
				List.fold_left (fun rs package ->
					add_parent package rs
				) xs xs
			in
			if rs == xs then rs else loop rs
		) in
		loop (StringMap.fold (fun _ (_, package_name) -> add_parent package_name) filename_mapping [])
	);;
	
	let items_per_package = Naming.items_per_module;;
	
	type name_mapping = Naming.name_mapping;;
	
	let name_mapping = Naming.name_mapping
		~long_f:(ada_name_by_substitute ~prefix:"C_" ~postfix:"")
		~short_f:(ada_name_by_short ~prefix:"C_" ~postfix:"")
		~foldcase:String.uppercase
		special_name_mapping;;
	
	let add_name_mapping_for_arguments = Naming.add_name_mapping_for_arguments
		~long_f:(ada_name_by_substitute ~prefix:"A_" ~postfix:"")
		~short_f:(ada_name_by_short ~prefix:"A_" ~postfix:"")
		~anonymous_f:ada_name_of_anonymous_argument;;
	
	let name_mapping_for_struct_items = Naming.name_mapping_for_struct_items
		~long_f:(ada_name_by_substitute ~prefix:"F_" ~postfix:"")
		~short_f:(ada_name_by_short ~prefix:"F_" ~postfix:"")
		~anonymous_f:ada_name_of_anonymous_component;;
	
	let ada_simple_name_of
		(ps: ranged_position)
		(name: string)
		(kind: [`namespace | `opaque_enum | `opaque_struct | `opaque_union])
		(name_mapping: name_mapping)
		: string =
	(
		let (filename, _, _, _), _ = ps in
		let _, _, nspp = StringMap.find filename name_mapping in
		Naming.find kind name nspp
	);;
	
	let add_package_name
		(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		?(hiding: StringSet.t = StringSet.empty)
		(package_name: string)
		(name: string)
		: string =
	(
		let rec contained sub s level = (
			assert (sub <> "");
			if s = "" then -1 else
			let s, sr = take_package_name s in
			let r = contained sub sr (level + 1) in
			if r >= 0 then r else
			if s = sub then level else
			-1
		) in
		if (current = package_name && not (StringSet.mem (String.uppercase name) hiding))
			|| package_name = "" then name else
		if StringSet.mem package_name hidden_packages
			|| (
				let p, pr = take_package_name package_name in
				pr <> "" && (
				let c = contained p current 0 in
				c > 0 || (c = 0 && fst (take_package_name pr) = p)))
		then (
			"Standard.C." ^ package_name ^ "." ^ name
		) else (
			package_name ^ "." ^ name
		)
	);;
	
	let ada_name_of
		(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		?(hiding: StringSet.t = StringSet.empty)
		(ps: ranged_position)
		(name: string)
		(kind: [`namespace | `opaque_enum | `opaque_struct | `opaque_union])
		(name_mapping: name_mapping)
		: string =
	(
		let (filename, _, _, _), _ = ps in
		begin try
			let _, package_name, nspp = StringMap.find filename name_mapping in
			let item_name = Naming.find kind name nspp in
			add_package_name current ~hidden_packages ~hiding package_name item_name
		with Not_found ->
			failwith ("ada_name_of \"" ^ name ^ "\" in \"" ^ filename ^ "\"")
		end
	);;
	
	(* types *)
	
	type anonymous_mapping = (Semantics.anonymous_type * (string * string)) list;; (* type, (package, identifier) *)
	
	let add_arguments_to_anonymous_mapping
		~(name_mapping: name_mapping)
		~(current: string)
		(args: Semantics.variable list)
		(anonymous_mapping: anonymous_mapping)
		: anonymous_mapping =
	(
		List.fold_left (fun anonymous_mapping arg ->
			let `named (ps, _, `variable (t, _), _) = arg in
			begin match Semantics.resolve_typedef t with
			| `pointer (#Semantics.anonymous_type as item) ->
				let (filename, _, _, _), _ = ps in
				let _, package_name, _ = StringMap.find filename name_mapping in
				if package_name <> current then (
					let hash = hash_name item in
					(item, (package_name, hash)) :: anonymous_mapping
				) else (
					anonymous_mapping
				)
			| _ ->
				anonymous_mapping
			end
		) anonymous_mapping args
	);;
	
	let separated_forward_opaque_to_full
		(item: Semantics.opaque_type)
		(opaque_mapping: Semantics.opaque_mapping)
		: Semantics.non_opaque_type option =
	(
		begin match Semantics.opaque_to_full item opaque_mapping with
		| Some full as result ->
			let `named (o_ps, _, _, _) = item in
			let `named (f_ps, _, _, _) = full in
			let (o_filename, _, _, _), _ = o_ps in
			let (f_filename, _, _, _), _ = f_ps in
			if o_filename = f_filename then None else
			result
		| None ->
			None
		end
	);;
	
	let separated_full_to_forward_opaque
		(item: Semantics.non_opaque_type)
		(opaque_mapping: Semantics.opaque_mapping)
		: Semantics.opaque_type option =
	(
		let opaque = Semantics.full_to_opaque item opaque_mapping in
		let `named (f_ps, _, _, _) = item in
		let `named (o_ps, _, _, _) = opaque in
		let (f_filename, _, _, _), _ = o_ps in
		let (o_filename, _, _, _), _ = f_ps in
		if f_filename = o_filename then None else
		Some opaque
	);;
	
	let using_anonymous_access_for_pointed_type (pointed_t: Semantics.all_type): Semantics.all_type option = (
		begin match Semantics.resolve_typedef pointed_t with
		| `void | `function_type _ ->
			None
		| `const pointed_t when (
			match Semantics.resolve_typedef (pointed_t :> Semantics.all_type) with
			| `void -> true
			| _ -> false)
		->
			None
		| _ ->
			Some pointed_t
		end
	);;
	
	let rec using_anonymous_access (t: Semantics.derived_type): Semantics.all_type option = (
		begin match t with
		| `pointer t ->
			using_anonymous_access_for_pointed_type t
		| `block_pointer _ ->
			None (* block pointer is mapped to System.Address *)
		| `array (_, t) ->
			using_anonymous_access_for_pointed_type (t :> Semantics.all_type)
		| `restrict (`pointer t) ->
			using_anonymous_access_for_pointed_type t
		| `volatile t ->
			begin match (t :> Semantics.all_type) with
			| #Semantics.derived_type as t ->
				using_anonymous_access t
			| _ ->
				None
			end
		| `const t ->
			begin match (t :> Semantics.all_type) with
			| #Semantics.derived_type as t ->
				using_anonymous_access t
			| _ ->
				None
			end
		end
	);;
	
	(* dependency *)
	
	let add_to_with_caluse_map
		(k : string)
		(v : with_option)
		(m : with_option StringMap.t)
		: with_option StringMap.t =
	(
		StringMap.modify (widely_with_option v) ~default:narrow_with_option k m
	);;
	
	let referencing_packages_by_language_mapping
		~(language_mapping: Semantics.language_mapping)
		~(derived_types: Semantics.derived_type list)
		(items: Semantics.all_item list)
		(m : with_option StringMap.t)
		: with_option StringMap.t =
	(
		let rec process (r: with_option StringMap.t) (x: Semantics.all_type) = (
			let r =
				begin try
					let alias = Semantics.find_mapped_type x language_mapping in
					let rindex = String.rindex alias '.' in
					let package_name = String.sub alias 0 rindex in
					add_to_with_caluse_map package_name (`none, `none, `none) r
				with Not_found -> (* find_mapped_type / String.rindex *)
					begin match x with
					| `__builtin_va_list
					| `pointer `void
					| `pointer (`volatile `void)
					| `pointer (`const `void)
					| `pointer (`const (`volatile `void)) ->
						(* va_list / void * are mapped to System.Address *)
						let package_name = "System" in
						add_to_with_caluse_map package_name (`none, `none, `none) r
					| _ ->
						r
					end
				end
			in
			Semantics.fold_derived_types (fun r x ->
				process r (x :> Semantics.all_type)
			) r x derived_types
		) in
		List.fold_left (fun r x ->
			begin match x with
			| #Semantics.predefined_type
			| #Semantics.derived_type
			| #Semantics.anonymous_type
			| `named (_, _, #Semantics.named_type_var, _) as x ->
				process r x
			| _ ->
				r
			end
		) m items
	);;
	
	let referencing_packages_from_depending
		~(language_mapping: Semantics.language_mapping)
		~(opaque_mapping: Semantics.opaque_mapping)
		~(name_mapping: name_mapping)
		~(current: string)
		(items: Semantics.all_item list)
		(m : with_option StringMap.t)
		: with_option StringMap.t =
	(
		let of_argument (t: Semantics.all_type): Semantics.named_item list = (
			let t =
				begin match t with
				| `const t -> (t :> Semantics.all_type) (* remove const *)
				| _ -> t
				end
			in
			begin match Semantics.resolve_typedef t with
			| #Semantics.derived_type as rt ->
				begin match using_anonymous_access rt with
				| Some t ->
					begin match Semantics.resolve_typedef t with
					| `named (_, _, #Semantics.opaque_type_var, _) as opaque ->
						begin match separated_forward_opaque_to_full opaque opaque_mapping with
						| Some full ->
							Dependency.of_item (full :> Semantics.all_item)
						| None ->
							Dependency.of_item (t :> Semantics.all_item)
						end
					| _ ->
						Dependency.of_item (t :> Semantics.all_item)
					end
				| None ->
					Dependency.of_item (t :> Semantics.all_item)
				end
			| _ ->
				Dependency.of_item (t :> Semantics.all_item)
			end
		) in
		let of_alias (item: Semantics.named_item): Semantics.named_item list = (
			let `named (_, _, var, _) = item in
			begin match var with
			| `extern ((#Semantics.function_type as ft), _)
			| `function_forward (_, (#Semantics.function_type as ft))
			| `function_definition (_, (#Semantics.function_type as ft), _) ->
				let `function_type prototype = ft in
				Dependency.of_prototype ~of_argument prototype
			| `defined_element_access (_, route) ->
				let result_type = Semantics.tail_type_of_element_access route in
				Dependency.of_item (result_type :> Semantics.all_item)
			| `defined_expression (_, e_type) ->
				Dependency.of_item (e_type :> Semantics.all_item)
			| _ ->
				[]
			end
		) in
		List.fold_left (fun (r: with_option StringMap.t) x ->
			begin match x with
			| `function_type _ ->
				r
			| `named (_, _, #Semantics.named_type_var, _) as t
				when Semantics.mem_mapped_type t language_mapping
			->
				r
			| `named (_, ("ptrdiff_t" | "size_t" | "wchar_t"), `typedef _, _) ->
				r
			| `named (_, _, #Semantics.opaque_type_var, _) as opaque ->
				begin match separated_forward_opaque_to_full opaque opaque_mapping with
				| Some full ->
					let `named (f_ps, _, _, _) = full in
					let package_name = "C." ^  Naming.module_of f_ps name_mapping in
					add_to_with_caluse_map package_name (`limited_with, `none, `none) r
				| None ->
					r
				end
			| (item: Semantics.all_item) ->
				List.fold_left (fun r d ->
					begin match d with
					| `named (opaque_ps, _, #Semantics.opaque_type_var, _) as opaque
						when separated_forward_opaque_to_full opaque opaque_mapping <> None
					->
						let full =
							match Semantics.opaque_to_full opaque opaque_mapping with
							| Some full -> full
							| None -> assert false (* does not come here *)
						in
						let `named (full_ps, _, _, _) = full in
						let package_name = Naming.module_of full_ps name_mapping in
						if package_name = current then (
							(* reference (pointer to) opaqure from full *)
							let package_name = Naming.module_of opaque_ps name_mapping in
							let package_name = "C." ^ package_name in
							add_to_with_caluse_map package_name (`none, `none, `none) r
						) else (
							(* reference full from opaque or other headers *)
							let package_name = "C." ^ package_name in
							add_to_with_caluse_map package_name (`limited_with, `none, `none) r
						)
					| `named (full_ps, _, #Semantics.non_opaque_type_var, _) as full
						when separated_full_to_forward_opaque full opaque_mapping <> None
					->
						let package_name = Naming.module_of full_ps name_mapping in
						if package_name = current then r else
						(* reference full from opaque headers or other headers via opaque type *)
						let package_name = "C." ^ package_name in
						add_to_with_caluse_map package_name (`limited_with, `none, `none) r
					| _ ->
						let `named (ps, _, _, _) = d in
						let package_name = Naming.module_of ps name_mapping in
						if package_name = current || package_name = "" then r else
						let package_name = "C." ^ package_name in
						add_to_with_caluse_map package_name (`none, `none, `none) r
					end
				) r (Dependency.dependents ~of_alias ~of_argument item)
			end
		) m items
	);;
	
	let referencing_packages
		~(language_mapping: Semantics.language_mapping)
		~(derived_types: Semantics.derived_type list)
		~(opaque_mapping: Semantics.opaque_mapping)
		~(name_mapping: name_mapping)
		~(casts: (Semantics.all_type * Semantics.all_type) list)
		~(current: string)
		(items: Semantics.all_item list)
		: with_clause list =
	(
		let rs = StringMap.empty in
		let rs = referencing_packages_by_language_mapping ~language_mapping ~derived_types items rs in
		let rs = referencing_packages_from_depending ~language_mapping ~name_mapping ~opaque_mapping ~current items rs in
		let rs =
			if casts <> [] then (
				add_to_with_caluse_map "Ada.Unchecked_Conversion" (`none, `none, `none) rs
			) else (
				rs
			)
		in
		List.rev (StringMap.fold (fun k v r -> (k, v) :: r) rs [])
	);;
	
	(* body requirement *)
	
	let body_required_for_single_item (item: Semantics.source_item): bool = (
		begin match item with
		| `named (_, _, `function_definition (sc, _, _), _) ->
			begin match sc with
			| `extern_inline -> false (* use extern version *)
			| `static | `none -> true (* static be placed into body *)
			end
		| `named (_, _, `defined_element_access _, _) ->
			true
		| `named (_, _, `defined_generic_expression _, _)
		| `named (_, _, `defined_generic_statement _, _) ->
			false (* unimplemented *)
		| `named (_, _, `defined_expression expr, _) ->
			not (Semantics.is_static_expression expr)
			&& (match expr with `ref_object _, _ -> false | _ -> true)
		| _ ->
			false
		end
	);;
	
	let body_required (items: Semantics.source_item list): bool = (
		List.exists body_required_for_single_item items
	);;
	
	(* expression /statment *)
	
	let prototype_for_element_access
		(ps: ranged_position)
		(t: Semantics.struct_or_union_type)
		(route: Semantics.struct_item list)
		: Semantics.prototype =
	(
		let result_type = Semantics.tail_type_of_element_access route in
		let args = [
			`named (ps, "Object", `variable ((t :> Semantics.all_type), None), Semantics.no_attributes)]
		in
		`cdecl, args, `none, result_type
	);;
	
	(* pretty printer *)
	
	let pp_notification (ff: formatter) (version: string): unit = (
		fprintf ff "--  This file is translated by \"headmaster\" %s.@." version;
		fprintf ff "--  The original C header\'s license should be applied to this file.@.";
		fprintf ff "--  All conditional-directives are expanded for the exclusive use of your@.";
		fprintf ff "--    environment, it is not recommended to commit this file to any repository.@.";
		fprintf ff "-------------------------------------------------------------------------------@."
	);;
	
	let pp_predefined_type_name
		(ff: formatter)
		(item: Semantics.predefined_type)
		: unit =
	(
		begin match item with
		| `void ->
			pp_print_string ff "void";
		| `bool ->
			pp_print_string ff "bool";
		| #int_prec as p ->
			pp_print_string ff (ada_name_of_int_prec p)
		| #float_prec as p ->
			pp_print_string ff (ada_name_of_float_prec p)
		| `decimal32 ->
			pp_print_string ff "decimal32"
		| `decimal64 ->
			pp_print_string ff "decimal64"
		| `decimal128 ->
			pp_print_string ff "decimal128"
		| `imaginary e ->
			fprintf ff "%s_imaginary" (ada_name_of_float_prec e)
		| `complex e ->
			fprintf ff "%s_complex" (ada_name_of_float_prec e)
		| `char ->
			pp_print_string ff "char"
		| `wchar ->
			pp_print_string ff "wchar_t"
		| `__builtin_va_list ->
			pp_print_string ff "builtin_va_list"
		end
	);;
	
	type where = [
		| `name (* for declaration or a part of the name of derived type *)
		| `subtype (* for normal using *)
		| `argument (* inline to anonymous access *)
		| `extern (* always constrainted *)
		| `rename];; (* always unconstrainted *)
	
	let rec pp_derived_type_name
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		?(hiding: StringSet.t = StringSet.empty)
		~(where: where)
		(item: Semantics.derived_type)
		: unit =
	(
		let pp_pointer_type_name ff ~mappings ~where ~restrict t = (
			let resolved_t = Semantics.resolve_typedef t in
			if where = `argument && using_anonymous_access_for_pointed_type resolved_t <> None then (
				(* expanding to anonymous access type *)
				begin match resolved_t with
				| `function_type _ ->
					(* pragma Convention does not reach to anonymous access to subprogram... *)
					assert false (* does not come here *)
				| `const t ->
					fprintf ff "access constant ";
					pp_type_name ff ~mappings ~current ~hidden_packages ~hiding ~where:`subtype (t :> Semantics.all_type)
				| _ ->
					fprintf ff "access ";
					pp_type_name ff ~mappings ~current ~hidden_packages ~hiding ~where:`subtype t
				end
			) else (
				let postfix = if restrict then "_restrict_ptr" else "_ptr" in
				begin match t with
				| `function_type _ as t ->
					let (package_name, unique_key) =
						let _, _, anonymous_mapping = mappings in
						try List.assq t anonymous_mapping with
						| Not_found -> failwith "pp_derived_type_name/pp_pointer_type_name"
					in
					pp_print_string ff (add_package_name current package_name ("access_" ^ unique_key))
				| `const t ->
					pp_type_name ff ~mappings ~current ~hidden_packages ~where:`name (t :> Semantics.all_type);
					fprintf ff "_const%s" postfix
				| _ ->
					pp_type_name ff ~mappings ~current ~hidden_packages ~where:`name t;
					pp_print_string ff postfix
				end
			)
		) in
		begin match item with
		| `pointer t ->
			pp_pointer_type_name ff ~mappings ~where ~restrict:false t
		| `block_pointer _ ->
			fprintf ff "System.Address"
		| `array (n, t) ->
			begin match where with
			| `argument ->
				assert false; (* does not come here *)
			| `extern | `name | `subtype | `rename as where ->
				let base_name = string_of_pp
					(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name) (t :> Semantics.all_type)
				in
				begin match where with
				| `extern | `name | `subtype as where ->
					begin match n with
					| Some n ->
						let n = Integer.to_int n in
						begin match where with
						| `subtype | `extern ->
							fprintf ff "%s_array" base_name;
							fprintf ff " (0 .. %d)" (n - 1)
						| `name ->
							fprintf ff "%s_array" (strip_package_name base_name);
							pp_print_int ff n
						end
					| None ->
						fprintf ff "%s_array" base_name;
						begin match where with
						| `extern ->
							fprintf ff " (size_t)"
						| `subtype | `name ->
							()
						end
					end
				| `rename ->
					fprintf ff "%s_array" base_name
				end
			end
		| `restrict (`pointer t) ->
			pp_pointer_type_name ff ~mappings ~where ~restrict:true t
		| `volatile base_type ->
			pp_type_name ff ~mappings ~current ~hidden_packages ~where:`name (base_type :> Semantics.all_type);
			fprintf ff "_volatile"
		| `const t ->
			pp_type_name ff ~mappings ~current ~hidden_packages ~where (t :> Semantics.all_type)
		end
	) and pp_anonymous_type_name
		(ff: formatter)
		~(mappings: name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		(item: Semantics.anonymous_type)
		: unit =
	(
		let ps, postfix =
			begin match item with
			| `anonymous (ps, `enum _) ->
				ps, "enum_"
			| `anonymous (ps, `struct_type _) ->
				ps, "struct_"
			| `anonymous (ps, `union _) ->
				ps, "union_"
			| `function_type _ ->
				assert false (* does not come here *)
			end
		in
		let unique_key =
			begin try
				let _, anonymous_mapping = mappings in
				snd (List.assq item anonymous_mapping)
			with Not_found ->
				hash_name item
			end
		in
		let (filename, _, _, _), _ = ps in
		let _, package_name, _ =
			let name_mapping, _ = mappings in
			try StringMap.find filename name_mapping with
			| Not_found -> failwith "pp_anonymous_type_name"
		in
		pp_print_string ff (add_package_name ~hidden_packages current package_name (postfix ^ unique_key))
	) and pp_type_name
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		?(hiding: StringSet.t = StringSet.empty)
		~(where: where)
		(item: Semantics.all_type)
		: unit =
	(
		let opaque_mapping, name_mapping, anonymous_mapping = mappings in
		begin match Semantics.resolve_typedef item with
		| #Semantics.derived_type as item
			when where = `argument && using_anonymous_access item <> None
		->
			pp_derived_type_name ff ~mappings ~current ~hidden_packages ~hiding ~where item
		| `named (_, _, #Semantics.opaque_type_var, _) as opaque
			when where <> `name && separated_forward_opaque_to_full opaque opaque_mapping <> None
		->
			begin match Semantics.opaque_to_full opaque opaque_mapping with
			| Some full ->
				pp_type_name ff ~mappings ~current ~hidden_packages ~hiding ~where (full :> Semantics.all_type)
			| None ->
				assert false (* does not come here *)
			end
		| _ ->
			begin match item with
			| #Semantics.predefined_type as item ->
				pp_predefined_type_name ff item
			| #Semantics.anonymous_type as item ->
				let mappings = name_mapping, anonymous_mapping in
				pp_anonymous_type_name ff ~mappings ~current ~hidden_packages item
			| #Semantics.derived_type as item ->
				pp_derived_type_name ff ~mappings ~current ~hidden_packages ~where item
			| `named (ps, name, (`opaque_enum | `enum _), _) ->
				let name = ada_name_of current ~hidden_packages ~hiding ps name `opaque_enum name_mapping in
				pp_print_string ff name
			| `named (ps, name, (`opaque_struct | `struct_type _), _) ->
				let name = ada_name_of current ~hidden_packages ~hiding ps name `opaque_struct name_mapping in
				pp_print_string ff name
			| `named (ps, name, (`opaque_union | `union _), _) ->
				let name = ada_name_of current ~hidden_packages ~hiding ps name `opaque_union name_mapping in
				pp_print_string ff name
			| `named (ps, ("ptrdiff_t" | "size_t" | "wchar_t" as name), _, _)
				when (let (filename, _, _, _), _ = ps in is_special_filename filename)
			->
				if StringSet.mem name hiding then pp_print_string ff "Standard.C.";
				pp_print_string ff name
			| `named (ps, name, _, _) ->
				let name = ada_name_of current ~hidden_packages ~hiding ps name `namespace name_mapping in
				pp_print_string ff name
			end
		end
	) and pp_prototype
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		~(name: string option)
		(prototype: Semantics.prototype)
		: name_mapping =
	(
		let _, args, _, ret = prototype in
		let _, name_mapping, _ = mappings in
		let name_mapping, args = add_name_mapping_for_arguments args name_mapping in
		let hiding = set_of_fst args in
		let pp_args ff args = (
			if args <> [] then (
				pp_print_string ff " (";
				let (_: int) =
					List.fold_left (fun num (arg_name, arg) ->
						if num >= 2 then fprintf ff ";@ " else pp_print_break ff 0 0;
						let `named (_, _, `variable (arg_t, _), _) = arg in
						pp_open_box ff indent;
						pp_print_string ff arg_name;
						pp_print_string ff " :";
						pp_print_space ff ();
						pp_type_name ff ~mappings ~current ~hidden_packages ~hiding ~where:`argument arg_t;
						pp_close_box ff ();
						num + 1
					) 1 args
				in
				pp_print_string ff ")"
			)
		) in
		begin match ret with
		| `void ->
			pp_print_string ff "procedure";
			begin match name with
			| Some name -> fprintf ff " %s" name;
			| None -> ()
			end;
			pp_args ff args
		| _ ->
			pp_print_string ff "function";
			begin match name with
			| Some name -> fprintf ff " %s" name;
			| None -> ()
			end;
			pp_args ff args;
			fprintf ff "@ return ";
			pp_type_name ff ~mappings ~current ~hidden_packages ~hiding ~where:`name ret
		end;
		name_mapping
	);;
	
	let pp_predefined_type
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		(item: Semantics.predefined_type * int)
		: unit =
	(
		let t, _ = item in
		begin try
			let alias = Semantics.find_mapped_type (t :> Semantics.all_type) language_mapping in
			pp_subtype ff (string_of_pp pp_predefined_type_name t) pp_print_string alias
		with Not_found ->
			let t, size = item in
			let name = string_of_pp pp_predefined_type_name t in
			begin match t with
			| `void ->
				fprintf ff "@ --  type void (<>) is limited private;"
			| `bool ->
				pp_type ff name pp_derived_type_definition
					pp_print_string "Boolean";
				pp_pragma_convention ff `cdecl name
			| #int_prec as p ->
				let body =
					begin match p with
					| `signed_char -> "new Short_Short_Integer"
					| `unsigned_char -> "mod 2 ** signed_char'Size"
					| `signed_short -> "new Short_Integer"
					| `unsigned_short -> "mod 2 ** signed_short'Size"
					| `signed_int -> "new Integer"
					| `unsigned_int -> "mod 2 ** signed_int'Size"
					| `signed_long -> "new Long_Integer"
					| `unsigned_long -> "mod 2 ** signed_long'Size"
					| `signed_long_long -> "new Long_Long_Integer"
					| `unsigned_long_long -> "mod 2 ** signed_long_long'Size"
					end
				in
				fprintf ff "@ type %s is %s;" name body;
				pp_pragma_convention ff `cdecl name
			| #float_prec as p ->
				let body =
					begin match p with
					| `float -> "Standard.Float"
					| `double -> "Long_Float"
					| `long_double -> "Long_Long_Float"
					end
				in
				pp_type ff name pp_derived_type_definition pp_print_string body;
				pp_pragma_convention ff `cdecl name
			| `decimal32 ->
				fprintf ff "@ --  type decimal32 is ..."
			| `decimal64 ->
				fprintf ff "@ --  type decimal64 is ..."
			| `decimal128 ->
				fprintf ff "@ --  type decimal128 is ..."
			| `imaginary e ->
				let e_name = ada_name_of_float_prec e in
				pp_type ff name pp_derived_type_definition pp_print_string e_name;
				pp_pragma_convention ff `cdecl name
			| `complex e ->
				let e_name = ada_name_of_float_prec e in
				pp_type ff name pp_record_definition [
					(fun ff () -> fprintf ff "@ Re, Im : %s'Base;" e_name)];
				pp_pragma_complex_representation ff name;
				pp_pragma_convention ff `cdecl name
			| `char ->
				pp_type ff name pp_derived_type_definition pp_print_string "Character"
				(* dirty hack: pragma Convention set word-size alignment...so omitted... *)
			| `wchar ->
				let body =
					match size with
					| 2 -> "Wide_Character"
					| 4 -> "Wide_Wide_Character"
					| _ -> assert false
				in
				pp_type ff name pp_derived_type_definition pp_print_string body
				(* dirty hack: pragma Convention set word-size alignment...so omitted... *)
			| `__builtin_va_list ->
				pp_subtype ff name pp_print_string "System.Address"
			end
		end
	);;
	
	let pp_primitives_for_predefined_type
		(ff: formatter)
		(item: Semantics.predefined_type * int)
		: unit =
	(
		let t, _ = item in
		begin match t with
		| #int_prec as p ->
			let name = ada_name_of_int_prec p in
			pp_print_space ff ();
			pp_open_box ff indent;
			fprintf ff "function Shift_Left (Left : %s; Right : Natural)@ return %s;" name name;
			pp_close_box ff ();
			begin match p with
			| #signed_int_prec ->
				pp_print_space ff ();
				pp_open_box ff indent;
				fprintf ff "function Shift_Right_Arithmetic (Left : %s; Right : Natural)@ return %s;" name name;
				pp_close_box ff ()
			| #unsigned_int_prec ->
				pp_print_space ff ();
				pp_open_box ff indent;
				fprintf ff "function Shift_Right (Left : %s; Right : Natural)@ return %s;" name name;
				pp_close_box ff ()
			end
		| _ ->
			()
		end
	);;
	
	let pp_newtype_of_predefind_type
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		(name: string)
		(t: Semantics.predefined_type)
		: unit =
	(
		begin try
			let _, alias =
				List.find (fun (x, _) ->
					begin match x with
					| `named (_, "size_t", _, _) -> true
					| _ -> false
					end
				) language_mapping.Semantics.lm_type
			in
			pp_subtype ff name pp_print_string alias
		with Not_found ->
			pp_type ff name pp_derived_type_definition pp_predefined_type_name t
		end
	);;
	
	let pp_derived_type
		(ff: formatter)
		~(mappings: Semantics.language_mapping * Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		(typedef: Semantics.typedef_type option)
		(item: Semantics.derived_type)
		: unit =
	(
		begin try
			let language_mapping, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let alias = Semantics.find_mapped_type (item :> Semantics.all_type) language_mapping in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			pp_subtype ff
				(string_of_pp
					(fun ff -> pp_derived_type_name ff ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name)
					item)
				pp_print_string alias
		with Not_found ->
			let pp_pointer_type ff ~mappings name ~restrict t = (
				begin match t with
				| `function_type prototype
				| `named (_, _, `typedef (`function_type prototype), _) ->
					ignore (pp_type ff name pp_access_definition `none
						(pp_prototype ~mappings ~current ?hidden_packages:None ~name:None) prototype);
					let conv, _, _, _ = prototype in
					pp_pragma_convention ff conv name
				| `void
				| `volatile `void
				| `const `void
				| `const (`volatile `void) ->
					pp_subtype ff name pp_print_string "System.Address"
				| _ ->
					begin match t with
					| `const t ->
						pp_type ff name pp_access_definition `constant
							(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) (t :> Semantics.all_type)
					| _ ->
						pp_type ff name pp_access_definition `all
							(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t
					end;
					pp_for_type_storage_size ff name 0;
					if not restrict then (
						pp_pragma_no_strict_aliasing ff name
					);
					pp_pragma_convention ff `cdecl name
				end
			) in
			let pp_sized_array ff ~mappings name n base_type = (
				let n = Integer.to_int n in
				pp_subtype ff name
					begin fun ff () ->
						fprintf ff "%a (0 .. %d)"
							(pp_derived_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name) (`array (None, base_type))
							(n - 1)
					end ()
			) in
			begin match typedef with
			| None ->
				begin match item with
				| `pointer t ->
					let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, anonymous_mapping in
					let name = string_of_pp (pp_derived_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name) item in
					pp_pointer_type ff ~mappings name ~restrict:false t
				| `block_pointer _ ->
					() (* block pointer is mapped to System.Address *)
				| `array (n, base_type) ->
					let language_mapping, opaque_mapping, name_mapping, anonymous_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, anonymous_mapping in
					let name = string_of_pp (pp_derived_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name) item in
					begin match n with
					| None ->
						begin try
							let alias = Semantics.find_mapped_type_of_unconstrained_array base_type language_mapping in
							pp_subtype ff name pp_print_string alias
						with Not_found ->
							pp_type ff name pp_array_definition
								(fun ff () -> pp_print_string ff "size_t range <>")
								`aliased
								(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype)
								(base_type :> Semantics.all_type);
							pp_pragma_convention ff `cdecl name
						end
					| Some n ->
						pp_sized_array ff ~mappings name n base_type
					end
				| `restrict (`pointer t) ->
					let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, anonymous_mapping in
					let name = string_of_pp (pp_derived_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name) item in
					pp_pointer_type ff ~mappings name ~restrict:true t
				| `volatile `void ->
					fprintf ff "@ --  type void_volatile is new void;"
				| `volatile base_type ->
					let language_mapping, opaque_mapping, name_mapping, anonymous_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, anonymous_mapping in
					let name = string_of_pp (pp_derived_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name) item in
					begin try
						let alias = Semantics.find_mapped_type (item :> Semantics.all_type) language_mapping in
						pp_subtype ff name pp_print_string alias
					with Not_found ->
						let resolved_base_type = Semantics.resolve_typedef (base_type :> Semantics.all_type) in
						pp_type ff name pp_derived_type_definition
							(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype)
							(base_type :> Semantics.all_type);
						begin match resolved_base_type with
						| #Semantics.predefined_type ->
							()
						| _ ->
							pp_pragma_convention ff `cdecl name
						end;
						pp_pragma_volatile ff name
					end
				| `const _ ->
					() (* only "access constant" form *)
				end
			| Some typedef ->
				let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				let name = string_of_pp (pp_derived_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name) item in
				begin match item with
				| `array (Some n, t) ->
					pp_sized_array ff ~mappings name n t
				| `const _ ->
					() (* only "access constant" form *)
				| _ ->
					(* ptrdiff_t, size_t, wchar_t in typedef.h *)
					let is_typedef_of_language_typedef (t: Semantics.typedef_type): bool = (
						begin match t with
						| `named (_, ("ptrdiff_t" | "size_t" | "wchar_t" as n1), `typedef (`named (_, n2, _, _)), _) when n1 = n2 ->
							true
						| _ ->
							false
						end
					) in
					let is_language_typedef =
						let is_typedef_of_language_typedef' (t: Semantics.all_type): bool = (
							begin match t with
							| `named (_, _, `typedef _, _) as t ->
								is_typedef_of_language_typedef t
							| _ ->
								false
							end
						) in
						Semantics.is_derived_type is_typedef_of_language_typedef' item
					in
					if is_language_typedef then (
						let re_t = Finding.expand_typedef is_typedef_of_language_typedef (item :> Semantics.all_type) in
						pp_subtype ff name (
							begin fun ff ->
								pp_print_string ff "Standard.C.";
								pp_type_name ff ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name
							end)
							re_t
					) else (
						pp_subtype ff name
							(pp_derived_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name)
							(match Finding.expand_typedef ((==) typedef) (item :> Semantics.all_type) with
								| #Semantics.derived_type as t -> t
								| _ -> assert false) (* does not come here *)
					)
				end
			end
		end
	);;
	
	let pp_unchecked_conversion
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		(t1, t2: Semantics.all_type * Semantics.all_type)
		: unit =
	(
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "function Cast is@ new Ada.Unchecked_Conversion (@,";
		pp_type_name ff ~mappings ~current ~where:`rename t1; (* use unconstrained *)
		fprintf ff ",@ ";
		pp_type_name ff ~mappings ~current ~where:`name t2;
		fprintf ff ");";
		pp_close_box ff ()
	);;
	
	let pp_derived_types_for_the_type
		(ff: formatter)
		~(mappings: Semantics.language_mapping * Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(casts: (Semantics.all_type * Semantics.all_type) list)
		~(sized_arrays: Semantics.all_type list)
		~(current: string)
		?(special: bool = false)
		(base_type: Semantics.all_type)
		(derived_types: Semantics.derived_type list)
		(done_list: Semantics.derived_type list)
		: Semantics.derived_type list =
	(
		Finding.recursive_fold_derived_types ~including_typedef:true (fun done_list dt ->
			let is_output =
				List.for_all
					(fun x -> not (Finding.compare_types_by_structure (dt :> Semantics.all_type) (x :> Semantics.all_type)))
					done_list
				&& (match dt with
				| `array (Some _, _) -> List.memq (dt :> Semantics.all_type) sized_arrays
				| _ -> true)
			in
			if is_output then (
				(* type declaration *)
				begin match base_type with
				| `named (_, _, `typedef _, _) as typedef_t when not special ->
					pp_derived_type ff ~mappings ~current (Some typedef_t) dt
				| _ ->
					pp_derived_type ff ~mappings ~current None dt
				end;
				(* cast *)
				List.iter (fun (x, y as pair) ->
					if x == (dt :> Semantics.all_type) || y == (dt :> Semantics.all_type) then (
						let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						pp_unchecked_conversion ff ~mappings ~current pair
					)
				) casts;
				(* add to done list *)
				dt :: done_list
			) else (
				done_list
			)
		) done_list base_type derived_types
	);;
	
	let pp_enum
		(ff: formatter)
		~(mappings: name_mapping * anonymous_mapping)
		~(current: string)
		(name: string)
		(t: Semantics.full_enum_type)
		: unit =
	(
		let items =
			begin match t with
			| `anonymous (_, `enum items) ->
				items
			| `named (_, _, `enum items, _) ->
				items
			end
		in
		(* split elements has same representation *)
		let items, duplicated =
			List.fold_left (fun (items, duplicated) x ->
				let `named (_, _, `enum_element x_value, _) = x in
				if
					List.exists (fun y ->
						let `named (_, _, `enum_element y_value, _) = y in
						y_value = x_value
					) items
				then (
					items, x :: duplicated
				) else (
					x :: items, duplicated
				)
			) ([], []) items
		in
		(* sort by representation *)
		let items =
			List.sort (fun x y ->
				let `named (_, _, `enum_element x_value, _) = x in
				let `named (_, _, `enum_element y_value, _) = y in
				Integer.compare x_value y_value
			) items
		in
		(* source order *)
		let duplicated = List.rev duplicated in
		(* printing type *)
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "type %s is (@," name;
		let rec loop index xs = (
			begin match xs with
			| `named (item_ps, item_name, _, _) :: xr ->
				if index > 0 then fprintf ff ",@ ";
				let item_name =
					let name_mapping, _ = mappings in
					ada_name_of current item_ps item_name `namespace name_mapping
				in
				pp_print_string ff item_name;
				loop (index + 1) xr
			| _ ->
				()
			end
		) in
		loop 0 items;
		fprintf ff ");";
		pp_close_box ff ();
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "for %s use (@," name;
		let rec loop index xs = (
			begin match xs with
			| `named (item_ps, item_name, `enum_element repr, _) :: xr ->
				if index > 0 then fprintf ff ",@ ";
				let item_name =
					let name_mapping, _ = mappings in
					ada_name_of current item_ps item_name `namespace name_mapping
				in
				fprintf ff "%s => %s" item_name (Integer.to_based_string ~base:10 repr);
				loop (index + 1) xr
			| _ ->
				()
			end
		) in
		loop 0 items;
		fprintf ff ");";
		pp_close_box ff ();
		pp_pragma_convention ff `cdecl name;
		(* printing duplicated *)
		List.iter (fun x ->
			let name_mapping, anonymous_mapping = mappings in
			let `named (item_ps, item_name, `enum_element x_value, _) = x in
			let item_name =
				ada_name_of current item_ps item_name `namespace name_mapping
			in
			let prototype = `cdecl, [], `none, (t :> Semantics.all_type) in
			pp_print_space ff ();
			pp_open_box ff indent;
			let mappings = Semantics.empty_opaque_mapping, name_mapping, anonymous_mapping in
			ignore (pp_prototype ff ~mappings ~current ~name:(Some item_name) prototype);
			let same_repr_item_name =
				let y =
					List.find (fun y ->
						let `named (_, _, `enum_element y_value, _) = y in
						y_value = x_value
					) items
				in
				let `named (item_ps, item_name, _, _) = y in
				ada_name_of current item_ps item_name `namespace name_mapping
			in
			fprintf ff "@ renames %s;" same_repr_item_name;
			pp_close_box ff ()
		) duplicated
	);;
	
	let make_pp_struct_items
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		(fields: (string * Semantics.struct_item) list)
		: (formatter -> unit -> unit) list =
	(
		List.map (fun (field_name, (_, field_type, field_bits_info, _)) (ff: formatter) () ->
			pp_print_space ff ();
			pp_open_box ff indent;
			pp_print_string ff field_name;
			pp_print_string ff " :";
			pp_print_space ff ();
			begin match field_bits_info with
			| Some (_, _, true) ->
				()
			| Some (_, _, false) | None ->
				pp_print_string ff "aliased "
			end;
			let hiding = set_of_fst fields in
			pp_type_name ff ~mappings ~current ~hidden_packages ~hiding ~where:`subtype field_type;
			begin match field_bits_info with
			| Some (_, field_bits, true) ->
				begin match Semantics.resolve_typedef field_type with
				| #signed_int_prec ->
					fprintf ff " range %s .. %s"
						(Integer.to_based_string ~base:10 (Integer.neg (Integer.shift_left Integer.one (field_bits - 1))))
						(Integer.to_based_string ~base:10 (Integer.sub (Integer.shift_left Integer.one (field_bits - 1)) Integer.one))
				| #unsigned_int_prec ->
					fprintf ff " range 0 .. %s"
						(Integer.to_based_string ~base:10 (Integer.sub (Integer.shift_left Integer.one field_bits) Integer.one))
				| _ ->
					assert false (* bit field has non-integer type *)
				end
			| Some (_, _, false) | None ->
				()
			end;
			pp_print_char ff ';';
			pp_close_box ff ()
		) fields
	);;
	
	let pp_struct
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		(name: string)
		(items: Semantics.struct_item list)
		(attributes: Semantics.attributes)
		: unit =
	(
		let _, fields = name_mapping_for_struct_items items in
		pp_type ff name pp_record_definition
			(make_pp_struct_items ~mappings ~current ~hidden_packages fields);
		if Semantics.is_bitfield items then (
			pp_print_space ff ();
			pp_open_vbox ff indent;
			fprintf ff "for %s use record" name;
			List.iter (fun (field_name, (_, _, field_bits_info, _)) ->
				begin match field_bits_info with
				| Some (field_pos, field_bits, _) ->
					fprintf ff "@ %s at %d range %d .. %d;" field_name
						0
						field_pos
						(field_pos + field_bits - 1)
				| None ->
					assert false (* does not come here *)
				end
			) fields;
			pp_close_box ff ();
			fprintf ff "@ end record;"
		);
		begin match attributes.Semantics.at_aligned with
		| `default ->
			()
		| `explicit_aligned ->
			pp_for_type_alignment ff name pp_print_string "Standard'Maximum_Alignment" (* ??? *)
		| `aligned n ->
			pp_for_type_alignment ff name pp_print_int n
		| `packed ->
			pp_pragma_pack ff name
		end;
		pp_pragma_convention ff `c_pass_by_copy name
	);;
	
	let pp_union
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		(name: string)
		(items: Semantics.struct_item list)
		: unit =
	(
		let has_bitfield = ref false in
		let field_map, _ = name_mapping_for_struct_items items in
		pp_type ff name
			~pp_discriminants:[
				fun ff () -> fprintf ff "Unchecked_Tag : unsigned_int := 0"]
			pp_record_definition
			[fun ff () ->
				pp_print_space ff ();
				pp_open_vbox ff indent;
				fprintf ff "case Unchecked_Tag is";
				let rec loop index xs = (
					begin match xs with
					| (item_name, item_type, _, _) :: xr ->
						pp_print_space ff ();
						pp_open_vbox ff indent;
						if xr <> [] then (
							fprintf ff "when %d =>" index
						) else (
							fprintf ff "when others =>"
						);
						if item_name = "" then (
							(* anonymous struct in union *)
							begin match item_type with
							| `anonymous (_, (`struct_type (_, items)))
							| `named (_, _, (`struct_type (_, items)), _) ->
								let _, fields = name_mapping_for_struct_items items in
								List.iter (fun pp_item ->
									pp_item ff ()
								) (make_pp_struct_items ~mappings ~current fields);
								(* bitfield ? *)
								has_bitfield := !has_bitfield || Semantics.is_bitfield items
							| _ ->
								assert false (* does not come here *)
							end
						) else (
							assert (StringMap.mem item_name field_map);
							let item_name = StringMap.find item_name field_map in
							fprintf ff "@ %s : %a;" item_name
								(pp_type_name ~mappings ~current ~hidden_packages ?hiding:None ~where:`subtype) item_type
						);
						pp_close_box ff ();
						loop (index + 1) xr
					| [] ->
						()
					end
				) in
				loop 0 items;
				pp_close_box ff ();
				fprintf ff "@ end case;"];
		pp_pragma_unchecked_union ff name;
		pp_pragma_convention ff `c_pass_by_copy name;
		if !has_bitfield then (
			fprintf ff "@ --  attention: %s has bit-field member!" name;
		);
	);;
	
	let pp_anonymous_type
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		(item: Semantics.anonymous_type)
		: unit =
	(
		let _, name_mapping, anonymous_mapping = mappings in
		let _, unique_key = List.assq item anonymous_mapping in
		begin match item with
		| `anonymous (_, `enum _) as t ->
			let name = "enum_" ^ unique_key in
			let mappings = name_mapping, anonymous_mapping in
			pp_enum ff ~mappings ~current name t
		| `anonymous (_, (`struct_type (alignment, items))) ->
			let name = "struct_" ^ unique_key in
			let attrs = {Semantics.no_attributes with Semantics.at_aligned = alignment} in
			pp_struct ff ~mappings ~current ~hidden_packages name items attrs
		| `anonymous (_, `union items) ->
			let name = "union_" ^ unique_key in
			pp_union ff ~mappings ~current ~hidden_packages name items
		| `function_type _ ->
			() (* only subprogram or access to subprogram *)
		end
	);;
	
	let pp_typedef_without_language_mapping
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		~(where: [`typedef | `macro])
		(ps: ranged_position)
		(name: string)
		(t: Semantics.all_type)
		: unit =
	(
		let opaque_mapping, name_mapping, anonymous_mapping = mappings in
		begin match Semantics.resolve_typedef t with
		| `named (_, _, #Semantics.opaque_type_var, _) as resolved_t
			when separated_forward_opaque_to_full resolved_t opaque_mapping <> None
		->
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			let target_type_name =
				omit_long_word
					(59 - String.length name)
					(string_of_pp (pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t)
			in
			fprintf ff "@ --  subtype %s is %s;" name target_type_name
		| `void ->
			fprintf ff "@ --  subtype %s is void (%s)" name
				(match where with `typedef -> "typedef" | `macro -> "macro")
		| _ ->
			begin match t with
			| `function_type _ ->
				fprintf ff "@ --  subtype %s is ... (function type)" name
			| _ ->
				let name = ada_name_of current ps name `namespace name_mapping in
				begin match name with
				| "ptrdiff_t" ->
					pp_subtype ff name pp_print_string "Standard.C.ptrdiff_t"
				| "size_t" ->
					pp_subtype ff name pp_print_string "Standard.C.size_t"
				| "wchar_t" ->
					pp_subtype ff name pp_print_string "Standard.C.wchar_t"
				| "bool" ->
					pp_subtype ff name pp_print_string "Standard.C.bool"
				| _ ->
					let mappings = opaque_mapping, name_mapping, anonymous_mapping in
					pp_subtype ff name
						(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t
				end
			end
		end
	);;
	
	let pp_typedef
		(ff: formatter)
		~(mappings: Semantics.language_mapping * Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		(item: Semantics.typedef_type)
		: unit =
	(
		begin try
			let language_mapping, _, _, _ = mappings in
			let alias = Semantics.find_mapped_type (item :> Semantics.all_type) language_mapping in
			let `named (_, name, _, _) = item in
			pp_subtype ff name pp_print_string alias
		with Not_found ->
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			let `named (ps, name, `typedef t, _) = item in
			pp_typedef_without_language_mapping ff ~mappings ~current ~where:`typedef ps name t
		end
	);;
	
	let pp_named_type
		(ff: formatter)
		~(mappings: Semantics.language_mapping * Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		(item: Semantics.named_type)
		: unit =
	(
		begin try
			let language_mapping, _, _, _ = mappings in
			let alias = Semantics.find_mapped_type (item :> Semantics.all_type) language_mapping in
			let `named (_, name, _, _) = item in
			pp_subtype ff name pp_print_string alias
		with Not_found ->
			begin match item with
			| `named (ps, name, (`opaque_enum | `opaque_struct | `opaque_union as kind), _) as item ->
				let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let name = ada_name_of current ps name kind name_mapping in
				begin match Semantics.opaque_to_full item opaque_mapping with
				| None ->
					(* as opaque *)
					pp_type ff name ~pp_discriminants:[fun ff () -> pp_print_string ff "<>"]
						pp_private_type_declaration `limited
				| Some full ->
					let `named (full_ps, _, _, _) = full in
					let module_having_full = Naming.module_of full_ps name_mapping in
					if module_having_full <> current then (
						(* use full type in another source *)
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						let target_type_name =
							omit_long_word
								(59 - String.length name)
								(string_of_pp
									(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype)
									(full :> Semantics.all_type))
						in
						fprintf ff "@ --  subtype %s is %s;" name target_type_name
					) else (
						(* as forward *)
						pp_incomplete_type ff name
					)
				end
			| `named (ps, name, `enum _, _) as t ->
				let _, _, name_mapping, anonymous_mapping = mappings in
				let name = ada_name_of current ps name `opaque_enum name_mapping in
				let mappings = name_mapping, anonymous_mapping in
				pp_enum ff ~mappings ~current name t
			| `named (ps, name, (`struct_type (_, items)), attrs) ->
				let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let name = ada_name_of current ps name `opaque_struct name_mapping in
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				pp_struct ff ~mappings ~current ~hidden_packages name items attrs
			| `named (ps, name, `union items, _) ->
				let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let name = ada_name_of current ps name `opaque_union name_mapping in
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				pp_union ff ~mappings ~current ~hidden_packages name items
			| `named (_, _, `typedef _, _) as t ->
				pp_typedef ff ~mappings ~current t
			| `named (_, _, `generic_type, _) ->
				assert false (* does not come here *)
			end
		end
	);;
	
	let pp_char_literal (ff: formatter) (c: char): unit = (
		begin match c with
		| '\x00' .. '\x1f' | '\x80' .. '\xff' ->
			fprintf ff "char'Val (%d)" (int_of_char c)
		| _ ->
			fprintf ff "\'%c\'" c
		end
	);;
	
	let pp_wchar_literal (ff: formatter) (c: WideString.elm): unit = (
		fprintf ff "wchar_t'Val (%ld)" c
	);;
	
	let rec pp_expression
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping)
		~(current: string)
		~(outside: outside_precedence)
		(expr: Semantics.expression)
		: unit =
	(
		let pp_relation_op ff op left right = (
			let paren = parenthesis_required ~outside ~inside:`relation in
			if paren then pp_open_paren ff ();
			pp_expression ff ~mappings ~current ~outside:`relation left;
			pp_print_space ff ();
			pp_print_string ff op;
			pp_print_char ff ' ';
			pp_expression ff ~mappings ~current ~outside:`relation right;
			if paren then pp_close_paren ff ()
		) in
		let pp_bit_op ff inside left right t = (
			let need_to_cast expr = (
				match Semantics.integer_of_expression expr with
				| Some (_, n) -> Integer.compare n Integer.zero < 0
				| None -> true
			) in
			let is_signed, unsigned_of_signed =
				begin match Semantics.resolve_typedef t with
				| #unsigned_int_prec as p -> false, p
				| #signed_int_prec as p -> (need_to_cast left || need_to_cast right), Semantics.unsigned_of_signed p
				| _ -> assert false
				end
			in
			if is_signed then (
				let opaque_mapping, name_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, [] in
				pp_type_name ff ~mappings ~current ~where:`name t;
				pp_print_char ff ' '
			);
			let paren = parenthesis_required ~outside ~inside:(inside :> precedence) in
			if is_signed || paren then pp_open_paren ff ();
			if is_signed && need_to_cast left then (
				pp_print_string ff (ada_name_of_int_prec unsigned_of_signed);
				pp_print_string ff "'Mod (";
				pp_expression ff ~mappings ~current ~outside:`lowest left;
				pp_print_char ff ')'
			) else (
				pp_expression ff ~mappings ~current ~outside:(inside :> outside_precedence) left
			);
			pp_print_space ff ();
			pp_print_string ff (
				begin match inside with
				| `logical_and -> "and"
				| `logical_or -> "or"
				| `logical_xor -> "xor"
				end);
			pp_print_char ff ' ';
			if is_signed && need_to_cast right then (
				pp_print_string ff (ada_name_of_int_prec unsigned_of_signed);
				pp_print_string ff "'Mod (";
				pp_expression ff ~mappings ~current ~outside:`lowest right;
				pp_print_char ff ')'
			) else (
				pp_expression ff ~mappings ~current ~outside:(inside :> outside_precedence) right
			);
			if is_signed || paren then pp_close_paren ff ()
		) in
		begin match expr with
		| `int_literal (_, value), _ ->
			pp_print_string ff (Integer.to_based_string ~base:10 value)
		| `float_literal (_, value), _ ->
			let _, e = Real.frexp value in
			let e16 = e / 4 in
			let m = Real.scale value ~base:16 ~exponent:(- e16) in
			fprintf ff "16#%s#e%+d" (Real.to_based_string ~base:16 m) e16
		| `imaginary_literal (_, value), _ ->
			let _, e = Real.frexp value in
			let e16 = e / 4 in
			let m = Real.scale value ~base:16 ~exponent:(- e16) in
			fprintf ff "16#%s#e%+d" (Real.to_based_string ~base:16 m) e16;
		| `char_literal value, _ ->
			pp_char_literal ff value
		| `chars_literal value, _ ->
			let zvalue = value ^ "\x00" in
			pp_string_literal ff pp_char_literal 0 zvalue
		| `wchar_literal value, _ ->
			pp_wchar_literal ff value
		| `wchars_literal value, _ ->
			let zvalue =
				let length = WideString.length value in
				let z = Array.make (length + 1) 0l in
				for i = 0 to length - 1 do
					z.(i) <- WideString.get value i
				done;
				z.(length) <- 0l;
				WideString.of_array z
			in
			pp_array_literal ff pp_wchar_literal 0
				WideString.length WideString.get zvalue
		| `objc_string_literal _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `enumerator (`named (ps, name, _, _)), _ ->
			let name =
				let _, name_mapping = mappings in
				ada_name_of current ps name `namespace name_mapping
			in
			pp_print_string ff name
		| `ref_object ((`named (ps, name, _, _)), _), _ ->
			let name =
				let _, name_mapping = mappings in
				ada_name_of current ps name `namespace name_mapping
			in
			pp_print_string ff name
		| `ref_function (`named (ps, name, _, _)), _ ->
			let name =
				let _, name_mapping = mappings in
				ada_name_of current ps name `namespace name_mapping
			in
			pp_print_string ff name
		| `__FILE__, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `__LINE__, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `__func__, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `statement _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Extension_Statement_%s" hash
		| `function_call (func, args), _ ->
			pp_expression ff ~mappings ~current ~outside:`primary func;
			if args <> [] then (
				pp_print_string ff " (";
				let (_: int) =
					List.fold_left (fun num arg ->
						if num >= 2 then fprintf ff ",@ " else pp_print_break ff 0 0;
						pp_expression ff ~mappings ~current ~outside:`lowest arg;
						num + 1
					) 1 args
				in
				pp_print_string ff ")"
			)
		| `va_arg _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `element_access ((_, t as expr), (field_name, _, _, _)), _ ->
			(* remove const *)
			let t =
				begin match Semantics.resolve_typedef t with
				| `const t -> Semantics.resolve_typedef (t :> Semantics.all_type)
				| _ as t -> t
				end
			in
			(* fields *)
			let items =
				begin match t with
				| `anonymous (_, `struct_type (_, items))
				| `anonymous (_, `union items)
				| `named (_, _, `struct_type (_, items), _)
				| `named (_, _, `union items, _) ->
					items
				| _ ->
					assert false (* does not come here *)
				end
			in
			let field_map, _ = name_mapping_for_struct_items items in
			begin match expr with
			| `dereference expr, _ (* omit .all *)
			| expr ->
				fprintf ff "%a.%s"
					(pp_expression ~mappings ~current ~outside:`primary) expr
					(StringMap.find field_name field_map)
			end
		| `dereference expr, _ ->
			begin match expr with
			| `add ((_, `array _ as a), index), _
			| `add (index, (_, `array _ as a)), _ ->
				pp_expression ff ~mappings ~current ~outside:`primary a;
				pp_print_string ff " (size_t (";
				pp_expression ff ~mappings ~current ~outside:`lowest index;
				pp_print_string ff "))"
			| `add _, t | `sub _, t ->
				(* ex: int_ptr (p + 1).all *)
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					pp_type_name ff ~mappings ~current ~where:`name t
				end;
				fprintf ff " %a.all"
					(pp_expression ~mappings ~current ~outside:`primary) expr
			| _ ->
				fprintf ff "%a.all"
					(pp_expression ~mappings ~current ~outside:`primary) expr
			end
		| `post_increment _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Assign_%s" hash
		| `post_decrement _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `compound (exprs, zero), t ->
			begin match Semantics.resolve_typedef t with
			| `array _ ->
				fprintf ff "(@,";
				let rec loop n es = (
					begin match es with
					| [] ->
						()
					| e :: er ->
						if n > 0 then fprintf ff ",@ ";
						pp_expression ff ~mappings ~current ~outside:`lowest e;
						loop (n + 1) er
					end;
				) in
				loop 0 exprs;
				begin match zero with
				| Some zero ->
					fprintf ff ",@ others => ";
					pp_expression ff ~mappings ~current ~outside:`lowest zero
				| None ->
					()
				end;
				fprintf ff ")";
			| `anonymous (_, `struct_type (_, items))
			| `named (_, _, `struct_type (_, items), _) ->
				let _, fields = name_mapping_for_struct_items items in
				fprintf ff "(@,";
				let rec loop first fs es = (
					begin match fs, es with
					| [], [] ->
						()
					| (field_name, _) :: fr, e :: er ->
						if not first then fprintf ff ",@ ";
						fprintf ff "%s => " field_name;
						pp_expression ff ~mappings ~current ~outside:`lowest e;
						loop false fr er
					| _ ->
						assert false
					end
				) in
				loop true fields exprs;
				fprintf ff ")";
			| `anonymous (_, `union items)
			| `named (_, _, `union items, _) ->
				let _, fields = name_mapping_for_struct_items items in
				fprintf ff "(@,";
				fprintf ff "Unchecked_Tag => 0";
				let process fs es = (
					begin match fs, es with
					| [], [] ->
						()
					| (field_name, _) :: _, e :: _ ->
						fprintf ff ",@ ";
						fprintf ff "%s => " field_name;
						pp_expression ff ~mappings ~current ~outside:`lowest e
					| _ ->
						assert false
					end
				) in
				process fields exprs;
				fprintf ff ")";
			| _ ->
				assert false (* does not come here ??? *)
			end
		| `increment _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `decrement _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Assign_%s" hash
		| `address expr, _ ->
			pp_expression ff ~mappings ~current ~outside:`primary expr;
			pp_print_string ff "'Access"
		| `neg _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `bit_not expr, _ ->
			let paren = parenthesis_required ~outside ~inside:`factor in
			if paren then pp_open_paren ff ();
			pp_print_string ff "not ";
			pp_expression ff ~mappings ~current ~outside:`factor expr;
			if paren then pp_close_paren ff ()
		| `not expr, _ ->
			let paren = parenthesis_required ~outside ~inside:`factor in
			if paren then pp_open_paren ff ();
			pp_print_string ff "not ";
			pp_expression ff ~mappings ~current ~outside:`factor expr;
			if paren then pp_close_paren ff ()
		| `sizeof_formal_type _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `real _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `imag _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `cast (_, t1 as expr), t2
		| `explicit_conv (_, t1 as expr), t2
		| `implicit_conv (_, t1 as expr), t2 ->
			let resolved_t1 = Semantics.resolve_typedef t1 in
			let resolved_t2 = Semantics.resolve_typedef t2 in
			begin match expr, resolved_t1, resolved_t2 with
			| (`float_literal _, _), #real_prec, #real_prec ->
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					pp_type_name ff ~mappings ~current ~where:`name t2
				end;
				fprintf ff "'(%a)"
					(pp_expression ~mappings ~current ~outside:`lowest) expr
			| _, #real_prec, `complex _ ->
				fprintf ff "(Re => %a,@ Im => 0.0)"
					(pp_expression ~mappings ~current ~outside:`lowest) expr
			| _, `imaginary _, `complex _ ->
				fprintf ff "(Re => 0.0,@ Im => %a)"
					(pp_expression ~mappings ~current ~outside:`lowest) expr
			| _, `bool, #int_prec ->
				fprintf ff "Boolean'Pos (%a)"
					(pp_expression ~mappings ~current ~outside:`lowest) expr
			| _, `char, #int_prec ->
				fprintf ff "char'Pos (%a)"
					(pp_expression ~mappings ~current ~outside:`lowest) expr
			| (`int_literal _, _), #int_prec, #int_prec ->
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					pp_type_name ff ~mappings ~current ~where:`name t2
				end;
				fprintf ff "'(%a)"
					(pp_expression ~mappings ~current ~outside:`lowest) expr
			| _, (#int_prec | #real_prec), (#int_prec | #real_prec) ->
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					pp_type_name ff ~mappings ~current ~where:`name t2
				end;
				pp_print_string ff " (";
				pp_print_break ff 0 0;
				pp_expression ff ~mappings ~current ~outside:`lowest expr;
				pp_print_char ff ')'
			| _, #int_prec, `bool ->
				let paren = parenthesis_required ~outside ~inside:`relation in
				if paren then pp_open_paren ff ();
				pp_expression ff ~mappings ~current ~outside:`relation expr;
				pp_print_space ff ();
				pp_print_string ff "/= 0";
				if paren then pp_close_paren ff ()
			| _, #int_prec, `char ->
				fprintf ff "char'Val (%a)"
					(pp_expression ~mappings ~current ~outside:`lowest) expr
			| _, #int_prec, `named (_, _, `enum _, _) ->
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					pp_type_name ff ~mappings ~current ~where:`name t2
				end;
				fprintf ff "'Enum_Val (%a)"
					(pp_expression ~mappings ~current ~outside:`lowest) expr
			| _, #int_prec, (`pointer `void) when Semantics.is_static_expression expr -> (* pointer literal to void *)
				begin match Semantics.integer_of_expression expr with
				| Some (_, value) ->
					fprintf ff "System'To_Address (%s)"
						(Integer.to_based_string ~base:10 value)
				| None ->
					assert false (* does not come here *)
				end
			| (`int_literal (_, value), _), #int_prec, (`pointer _) (* pointer literal to not void *)
				when Integer.compare value Integer.zero = 0
			->
				pp_print_string ff "null"
			| (`chars_literal _, _), `array (_, `char), (`pointer `char | `pointer (`const `char)) ->
				let hash = hash_name expr in
				fprintf ff "const_%s (0)'Access" hash
			| _, `pointer _, `bool ->
				let paren = parenthesis_required ~outside ~inside:`relation in
				if paren then pp_open_paren ff ();
				pp_expression ff ~mappings ~current ~outside:`relation expr;
				pp_print_space ff ();
				pp_print_string ff "/= System.Null_Address";
				if paren then pp_close_paren ff ()
			| _, _, `pointer _ | _, `pointer _, _ ->
				let opaque_mapping, name_mapping = mappings in
				let mappings_for_typename = opaque_mapping, name_mapping, [] in
				if outside <> `primary then (
					pp_type_name ff ~mappings:mappings_for_typename ~current ~where:`name t2;
					pp_print_string ff "'(";
				);
				pp_print_string ff "Cast (";
				begin match expr with
				| `int_literal _, _ ->
					pp_type_name ff ~mappings:mappings_for_typename ~current ~where:`name t1;
					pp_print_string ff "'(";
					pp_expression ff ~mappings ~current ~outside:`lowest expr;
					pp_print_char ff ')';
				| _ ->
					pp_expression ff ~mappings ~current ~outside:`lowest expr
				end;
				pp_print_string ff ")";
				if outside <> `primary then (
					pp_print_string ff ")"
				)
			| _ ->
				fprintf ff "@ **** unimplemented. ****\n";
				assert false
			end
		| `mul (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`term in
			if paren then pp_open_paren ff ();
			pp_expression ff ~mappings ~current ~outside:`term left;
			pp_print_space ff ();
			pp_print_string ff "* ";
			pp_expression ff ~mappings ~current ~outside:`term right;
			if paren then pp_close_paren ff ()
		| `div (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`term in
			if paren then pp_open_paren ff ();
			pp_expression ff ~mappings ~current ~outside:`term left;
			pp_print_space ff ();
			pp_print_string ff "/ ";
			pp_expression ff ~mappings ~current ~outside:`term_right right;
			if paren then pp_close_paren ff ()
		| `rem (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`term in
			if paren then pp_open_paren ff ();
			pp_expression ff ~mappings ~current ~outside:`term left;
			pp_print_space ff ();
			pp_print_string ff "rem ";
			pp_expression ff ~mappings ~current ~outside:`term_right right;
			if paren then pp_close_paren ff ()
		| `add (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`simple in
			if paren then pp_open_paren ff ();
			pp_expression ff ~mappings ~current ~outside:`simple left;
			pp_print_space ff ();
			pp_print_string ff "+ ";
			pp_expression ff ~mappings ~current ~outside:`simple right;
			if paren then pp_close_paren ff ()
		| `sub (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`simple in
			if paren then pp_open_paren ff ();
			pp_expression ff ~mappings ~current ~outside:`simple left;
			pp_print_space ff ();
			pp_print_string ff "- ";
			pp_expression ff ~mappings ~current ~outside:`simple_right right;
			if paren then pp_close_paren ff ()
		| `l_shift (left, right), t ->
			begin
				let opaque_mapping, name_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, [] in
				pp_type_name ff ~mappings ~current ~where:`name t
			end;
			pp_print_string ff "'(Shift_Left (";
			pp_print_break ff 0 0;
			pp_expression ff ~mappings ~current ~outside:`lowest left;
			pp_print_string ff ",";
			pp_print_space ff ();
			let static_shift_bits = Semantics.is_static_expression right in
			if not static_shift_bits then pp_print_string ff "Natural (";
			pp_expression ff ~mappings ~current ~outside:`lowest right;
			if not static_shift_bits then pp_print_char ff ')';
			pp_print_string ff "))"
		| `r_shift (left, right), t ->
			begin
				let opaque_mapping, name_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, [] in
				pp_type_name ff ~mappings ~current ~where:`name t
			end;
			begin match Semantics.resolve_typedef t with
			| #signed_int_prec ->
				fprintf ff "'(Shift_Right_Arithmetic ("
			| #unsigned_int_prec ->
				fprintf ff "'(Shift_Right ("
			| _ ->
				assert false (* error! *)
			end;
			pp_print_break ff 0 0;
			pp_expression ff ~mappings ~current ~outside:`lowest left;
			pp_print_string ff ",";
			pp_print_space ff ();
			let static_shift_bits = Semantics.is_static_expression right in
			if not static_shift_bits then pp_print_string ff "Natural (";
			pp_expression ff ~mappings ~current ~outside:`lowest right;
			if not static_shift_bits then pp_print_char ff ')';
			pp_print_string ff "))"
		| `lt (left, right), _ ->
			pp_relation_op ff "<" left right
		| `gt (left, right), _ ->
			pp_relation_op ff ">" left right
		| `le (left, right), _ ->
			pp_relation_op ff "<=" left right
		| `ge (left, right), _ ->
			pp_relation_op ff ">=" left right
		| `eq (left, right), _ ->
			pp_relation_op ff "=" left right
		| `ne (left, right), _ ->
			pp_relation_op ff "/=" left right
		| `uo _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `bit_and (left, right), t ->
			pp_bit_op ff `logical_and left right t
		| `bit_xor (left, right), t ->
			pp_bit_op ff `logical_xor left right t
		| `bit_or (left, right), t ->
			pp_bit_op ff `logical_or left right t
		| `and_then (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`logical_and_then in
			if paren then pp_open_paren ff ();
			pp_expression ff ~mappings ~current ~outside:`logical_and_then left;
			pp_print_space ff ();
			pp_print_string ff "and then ";
			pp_expression ff ~mappings ~current ~outside:`logical_and_then right;
			if paren then pp_close_paren ff ()
		| `or_else (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`logical_or_else in
			if paren then pp_open_paren ff ();
			pp_expression ff ~mappings ~current ~outside:`logical_or_else left;
			pp_print_space ff ();
			pp_print_string ff "or else ";
			pp_expression ff ~mappings ~current ~outside:`logical_or_else right;
			if paren then pp_close_paren ff ()
		| `cond _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Cond_%s" hash
		| `assign _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Assign_%s" hash
		| `comma _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		end
	);;
	
	let pp_assignment_expression
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping)
		~(current: string)
		~(pp_left: formatter -> outside:outside_precedence -> Semantics.expression -> unit)
		(expr: Semantics.assignment_expression)
		: unit =
	(
		pp_print_space ff ();
		pp_open_box ff indent;
		let `assign (left, op, right), _ = expr in
		fprintf ff "%a := " (pp_left ~outside:`lowest) left;
		begin match op with
		| `assign ->
			pp_expression ff ~mappings ~current ~outside:`lowest right;
			pp_print_string ff ";"
		| `mul_assign ->
			fprintf ff "%a * %a;"
				(pp_left ~outside:`term) left
				(pp_expression ~mappings ~current ~outside:`term) right
		| `div_assign ->
			fprintf ff "%a / %a;"
				(pp_left ~outside:`term_right) left
				(pp_expression ~mappings ~current ~outside:`term_right) right
		| `rem_assign ->
			fprintf ff "%a rem %a;"
				(pp_left ~outside:`term_right) left
				(pp_expression ~mappings ~current ~outside:`term_right) right
		| `add_assign ->
			fprintf ff "%a + %a;"
				(pp_left ~outside:`simple) left
				(pp_expression ~mappings ~current ~outside:`simple) right
		| `sub_assign ->
			fprintf ff "%a - %a;"
				(pp_left ~outside:`simple) left
				(pp_expression ~mappings ~current ~outside:`simple_right) right
		| `l_shift_assign ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `r_shift_assign ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `and_assign ->
			fprintf ff "%a and %a;"
				(pp_left ~outside:`logical_and) left
				(pp_expression ~mappings ~current ~outside:`logical_and) right
		| `or_assign ->
			fprintf ff "%a or %a;"
				(pp_left ~outside:`logical_or) left
				(pp_expression ~mappings ~current ~outside:`logical_or) right
		| `xor_assign ->
			fprintf ff "%a xor %a;"
				(pp_left ~outside:`logical_xor) left
				(pp_expression ~mappings ~current ~outside:`logical_xor) right
		end;
		pp_close_box ff ()
	);;
	
	let pp_incdec
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping)
		~(current: string)
		(op: char)
		(t: Semantics.all_type)
		: unit =
	(
		pp_print_space ff ();
		pp_open_box ff indent;
		if Semantics.is_pointer t then (
			let opaque_mapping, name_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, [] in
			fprintf ff "Left := Cast (Cast (Left)@ %c %a'Size / Standard'Storage_Unit);" op
				(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t
		) else (
			fprintf ff "Left := Left %c 1;" op
		);
		pp_close_box ff ()
	);;
	
	let rec pp_expression_in_statement
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping)
		~(current: string)
		(expr: Semantics.expression)
		~(pp_statement: formatter -> unit -> unit)
		: unit =
	(
		let chars_literals = Finding.find_all_chars_literal_as_pointer_in_expression [] expr in
		let assignment_expressions = Finding.find_all_assignment_in_expression [] expr in
		let conditional_expressions = Finding.find_all_conditional_in_expression [] expr in
		let statement_expressions = Finding.find_all_extension_statement_expression_in_expression [] expr in
		if chars_literals <> []
			|| assignment_expressions <> []
			|| conditional_expressions <> []
			|| statement_expressions <> []
		then (
			pp_print_space ff ();
			pp_open_vbox ff indent;
			pp_print_string ff "declare";
			(* literals *)
			List.iter (fun (literal, _ as expr) ->
				pp_print_space ff ();
				let hash = hash_name expr in
				pp_open_vbox ff indent;
				begin match literal with
				| `chars_literal value ->
					pp_open_box ff indent;
					fprintf ff "const_%s : constant char_array := " hash;
					pp_string_literal ff pp_char_literal 0 (value ^ "\x00");
					fprintf ff ";";
					pp_close_box ff ()
				| _ ->
					fprintf ff "@ **** unimplemented. ****\n";
					assert false
				end;
				pp_close_box ff ();
			) chars_literals;
			(* specs of inner functions *)
			List.iter (fun (_, t as expr) ->
				let hash = hash_name expr in
				let opaque_mapping, name_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, [] in
				fprintf ff "@ function Assign_%s return %a;" hash
					(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t
			) assignment_expressions;
			List.iter (fun (_, t as expr) ->
				let hash = hash_name expr in
				let opaque_mapping, name_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, [] in
				fprintf ff "@ function Cond_%s return %a;" hash
					(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t
			) conditional_expressions;
			List.iter (fun (_, t as expr) ->
				let hash = hash_name expr in
				pp_print_space ff ();
				pp_open_box ff indent;
				let opaque_mapping, name_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, [] in
				fprintf ff "function Extension_Statement_%s@ return %a;" hash
					(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t;
				pp_close_box ff ()
			) statement_expressions;
			(* bodies of inner functions *)
			List.iter (fun (_, t as expr) ->
				pp_print_space ff ();
				let hash = hash_name expr in
				pp_open_vbox ff indent;
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					fprintf ff "function Assign_%s return %a is" hash
						(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t
				end;
				begin match expr with
				| `assign (ebody, _, _), _
				| `increment ebody, _
				| `decrement ebody, _ as expr ->
					fprintf ff "@ Left : ";
					begin
						let opaque_mapping, name_mapping = mappings in
						let mappings = opaque_mapping, name_mapping, [] in
						pp_type_name ff ~mappings ~current ~where:`subtype t
					end;
					fprintf ff " renames %a;"
						(pp_expression ~mappings ~current ~outside:`lowest) ebody;
					pp_close_box ff ();
					pp_begin ff ();
					begin match expr with
					| `assign _, _ as expr ->
						pp_assignment_expression ff ~mappings ~current
							~pp_left:(fun ff ~outside _ -> ignore outside; fprintf ff "Left") expr
					| `increment _, t ->
						pp_incdec ff ~mappings ~current '+' t
					| `decrement _, t ->
						pp_incdec ff ~mappings ~current '-' t
					end;
					pp_return ff (Some (fun ff () -> pp_print_string ff "Left"))
				| `post_increment ebody, _
				| `post_decrement ebody, _ as expr ->
					pp_print_space ff ();
					pp_print_string ff "Left : ";
					begin
						let opaque_mapping, name_mapping = mappings in
						let mappings = opaque_mapping, name_mapping, [] in
						pp_type_name ff ~mappings ~current ~where:`subtype t
					end;
					pp_print_string ff " renames ";
					pp_expression ff ~mappings ~current ~outside:`lowest ebody;
					pp_print_char ff ';';
					pp_print_space ff ();
					pp_print_string ff "Previous : constant ";
					begin
						let opaque_mapping, name_mapping = mappings in
						let mappings = opaque_mapping, name_mapping, [] in
						pp_type_name ff ~mappings ~current ~where:`subtype t
					end;
					pp_print_string ff " := Left;";
					pp_close_box ff ();
					pp_begin ff ();
					begin match expr with
					| `post_increment _, t ->
						pp_incdec ff ~mappings ~current '+' t
					| `post_decrement _, t ->
						pp_incdec ff ~mappings ~current '-' t
					end;
					pp_return ff (Some (fun ff () -> pp_print_string ff "Previous"))
				end;
				pp_end ff ~label:("Assign_" ^ hash) ()
			) assignment_expressions;
			List.iter (fun (`cond (cond, true_case, false_case), t as expr) ->
				pp_print_space ff ();
				let hash = hash_name expr in
				pp_open_vbox ff indent;
				fprintf ff "function Cond_%s return " hash;
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					pp_type_name ff ~mappings ~current ~where:`subtype t
				end;
				pp_print_string ff " is";
				pp_close_box ff ();
				pp_begin ff ();
				pp_if ff
					~pp_cond:(fun ff () -> pp_expression ff ~mappings ~current ~outside:`lowest cond)
					~pp_true_case: (
						begin fun ff () ->
							pp_return ff (Some (fun ff () ->
								pp_expression ff ~mappings ~current ~outside:`lowest true_case))
						end)
					~pp_false_case: (Some (
						begin fun ff () ->
							pp_return ff (Some (fun ff () ->
								pp_expression ff ~mappings ~current ~outside:`lowest false_case))
						end));
				pp_end ff ~label:("Cond_" ^ hash) ()
			) conditional_expressions;
			List.iter (fun (`statement stmts, t as expr) ->
				pp_print_space ff ();
				let hash = hash_name expr in
				pp_open_vbox ff indent;
				fprintf ff "function Extension_Statement_%s@ return " hash;
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					pp_type_name ff ~mappings ~current ~where:`subtype t
				end;
				pp_print_string ff " is";
				pp_close_box ff ();
				pp_begin ff ();
				begin
					let opaque_mapping, name_mapping = mappings in
					let mappings = opaque_mapping, name_mapping, [] in
					pp_statement_list ff ~mappings ~current
						~in_expression:true ~null_statement:true stmts
				end;
				pp_end ff ~label:("Extension_Statement_" ^ hash) ()
			) statement_expressions;
			(* end of local declarations *)
			pp_close_box ff ();
			pp_begin ff ();
			pp_statement ff ();
			pp_end ff ()
		) else (
			pp_statement ff ()
		);
	) and pp_statement
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(in_expression: bool = false)
		(stmt: Semantics.statement)
		: unit =
	(
		begin match stmt with
		| `asm (volatile, template, out_args, in_args, destructive) ->
			pp_print_space ff ();
			pp_open_box ff indent;
			fprintf ff "Asm (";
			pp_string_literal ff pp_character_literal 1 template;
			if out_args <> [] then (
				let handle (n, expr) = (
					pp_type_name ff ~mappings ~current ~where:`name (snd expr);
					let opaque_mapping, name_mapping, _ = mappings in
					let mappings = opaque_mapping, name_mapping in
					fprintf ff "\'Asm_Output (\"%s\", %a)" n
						(pp_expression ~mappings ~current ~outside:`lowest) expr
				) in
				fprintf ff ",@ ";
				pp_open_box ff indent;
				fprintf ff "Outputs =>@ ";
				begin match out_args with
				| x :: [] ->
					handle x
				| _ ->
					fprintf ff "(";
					let (_: int) =
						List.fold_left (fun i x ->
							if i > 0 then fprintf ff "@ ,";
							handle x;
							i + 1
						) 0 in_args
					in
					fprintf ff ")"
				end;
				pp_close_box ff ()
			);
			if in_args <> [] then (
				let handle (n, expr) = (
					pp_type_name ff ~mappings ~current ~where:`name (snd expr);
					let opaque_mapping, name_mapping, _ = mappings in
					let mappings = opaque_mapping, name_mapping in
					fprintf ff "\'Asm_Input (\"%s\", %a)" n
						(pp_expression ~mappings ~current ~outside:`lowest) expr
				) in
				fprintf ff ",@ Inputs => ";
				begin match in_args with
				| x :: [] ->
					handle x
				| _ ->
					pp_print_char ff '(';
					let (_: int) =
						List.fold_left (fun i x ->
							if i > 0 then fprintf ff "@ ,";
							handle x;
							i + 1
						) 0 in_args
					in
					pp_print_char ff ')'
				end
			);
			if destructive <> [] then (
				fprintf ff ",@ ";
				pp_open_box ff indent;
				fprintf ff "Clobber =>@ ";
				pp_print_char ff '\"';
				let (_: int) =
					List.fold_left (fun i x ->
						if i > 0 then pp_print_string ff " ,";
						pp_print_string ff x;
						i + 1
					) 0 destructive
				in
				pp_print_char ff '\"';
				pp_close_box ff ()
			);
			if volatile = `volatile
				|| in_args = [] (* suppressing warning by gnat *)
			then (
				fprintf ff ",@ Volatile => True"
			);
			pp_print_string ff ");";
			pp_close_box ff ()
		| `local (items, stmts) ->
			pp_print_space ff ();
			pp_open_vbox ff indent;
			pp_print_string ff "declare";
			let opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let (name_mapping, _: name_mapping * anonymous_mapping) =
				List.fold_left (fun (name_mapping, anonymous_mapping) item ->
					(* source item *)
					begin match item with
					| #Semantics.anonymous_type as item ->
						let hash = hash_name item in
						let anonymous_mapping = (item, (current, hash)) :: anonymous_mapping in
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						pp_anonymous_type ff ~mappings ~current item;
						name_mapping, anonymous_mapping
					| `named (ps, name, _, _) as item ->
						(* name mapping *)
						let name_mapping =
							try
								(* should make mapping function in c_semantics_naming.ml... *)
								let ada_name = ada_name_by_substitute ~prefix:"L_" ~postfix:"" name in
								let (filename, _, _, _), _ = ps in
								let rel_filename, package_name, map = StringMap.find filename name_mapping in
								let map = Naming.add `namespace name ada_name map in
								StringMap.add filename (rel_filename, package_name, map) name_mapping
							with Not_found -> assert false
						in
						(* un-modified variable to constant *)
						let item =
							match item with
							| `named (ps, name, `variable (#Semantics.not_const_type as t, init), attr) as var
								when not (List.exists (Finding.lvalue_referenced_in_statement var) stmts)
							->
								`named (ps, name, `variable (`const t, init), attr)
							| _ ->
								item
						in
						(* output *)
						let mappings = Semantics.no_language_mapping, opaque_mapping, name_mapping, anonymous_mapping in
						pp_named ff ~mappings ~enum_of_element:StringMap.empty ~current item;
						name_mapping, anonymous_mapping
					| `include_point _ | `anonymous_alias _ ->
						name_mapping, anonymous_mapping
					end
				) (name_mapping, []) items
			in
			pp_close_box ff ();
			pp_begin ff ();
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			pp_statement_list ff ~mappings ~current
				~in_expression ~null_statement:true stmts;
			pp_end ff ()
		| `compound _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `expression expr ->
			if in_expression then (
				pp_return ff (Some (fun ff () ->
					let opaque_mapping, name_mapping, _ = mappings in
					let mappings = opaque_mapping, name_mapping in
					pp_expression ff ~mappings ~current ~outside:`lowest expr))
			) else (
				begin match expr with
				| `assign (_, `assign, _), _ as expr ->
					let opaque_mapping, name_mapping, _ = mappings in
					let mappings = opaque_mapping, name_mapping in
					pp_assignment_expression ff ~mappings ~current
						~pp_left:(pp_expression ~mappings ~current) expr
				| #Semantics.any_assignment_expression_var, t as expr ->
					begin match expr with
					| `assign (ebody, _, _), _
					| `increment ebody, _
					| `decrement ebody, _
					| `post_increment ebody, _
					| `post_decrement ebody, _ as expr ->
						pp_print_space ff ();
						pp_open_vbox ff indent;
						pp_print_string ff "declare";
						fprintf ff "@ Left : %a renames "
							(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t;
						begin
							let opaque_mapping, name_mapping, _ = mappings in
							let mappings = opaque_mapping, name_mapping in
							pp_expression ff ~mappings ~current ~outside:`lowest ebody
						end;
						pp_print_char ff ';';
						pp_close_box ff ();
						pp_begin ff ();
						begin match expr with
						| `assign (_, _, _), _ as expr ->
							let opaque_mapping, name_mapping, _ = mappings in
							let mappings = opaque_mapping, name_mapping in
							pp_assignment_expression ff ~mappings ~current
								~pp_left:(fun ff ~outside _ -> ignore outside; fprintf ff "Left") expr
						| `increment _, _
						| `post_increment _, _ ->
							let opaque_mapping, name_mapping, _ = mappings in
							let mappings = opaque_mapping, name_mapping in
							pp_incdec ff ~mappings ~current '+' t
						| `decrement _, _
						| `post_decrement _, _ ->
							let opaque_mapping, name_mapping, _ = mappings in
							let mappings = opaque_mapping, name_mapping in
							pp_incdec ff ~mappings ~current '-' t
						| _ ->
							assert false
						end;
						pp_end ff ()
					end
				| _ ->
					pp_print_space ff ();
					pp_open_box ff indent;
					let opaque_mapping, name_mapping, _ = mappings in
					let mappings = opaque_mapping, name_mapping in
					pp_expression ff ~mappings ~current ~outside:`lowest expr;
					pp_print_char ff ';';
					pp_close_box ff ()
				end
			)
		| `va_start _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `va_end _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `va_copy _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `label _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `if_statement (cond, true_case, false_case) ->
			let opaque_mapping, name_mapping, _ = mappings in
			let mappings_for_expr = opaque_mapping, name_mapping in
			pp_expression_in_statement ff ~mappings:mappings_for_expr ~current cond
				~pp_statement:(
					begin fun ff () ->
						pp_if ff
							~pp_cond:(fun ff () -> pp_expression ff ~mappings:mappings_for_expr ~current ~outside:`lowest cond)
							~pp_true_case:
								begin fun ff () ->
									pp_statement_list ff ~mappings ~current
										~null_statement:true true_case
								end
							~pp_false_case:(if false_case = [] then None else Some (
								begin fun ff () ->
									pp_statement_list ff ~mappings ~current
										~null_statement:true false_case
								end))
					end)
		| `while_loop (cond, stmts) ->
			let opaque_mapping, name_mapping, _ = mappings in
			let mappings_for_expr = opaque_mapping, name_mapping in
			pp_loop ff
				~pp_cond:
					(Some (pp_while (fun ff () ->
						pp_expression ff ~mappings:mappings_for_expr ~current ~outside:`lowest cond)))
				~pp_loop:
					begin fun ff () ->
						pp_statement_list ff ~mappings ~current
							~null_statement:true stmts
					end
		| `do_loop (stmts, cond) ->
			let opaque_mapping, name_mapping, _ = mappings in
			let mappings_for_expr = opaque_mapping, name_mapping in
			pp_loop ff
				~pp_cond:None
				~pp_loop:
					begin fun ff () ->
						pp_statement_list ff ~mappings ~current
							~null_statement:true stmts;
						pp_exit ff ~pp_when:(Some (fun ff () ->
							pp_print_string ff "not";
							pp_print_space ff ();
							pp_expression ff ~mappings:mappings_for_expr ~current ~outside:`factor cond))
					end
		| `for_loop _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `goto _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `break _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `return (expr, _) ->
			begin match expr with
			| Some expr ->
				let opaque_mapping, name_mapping, _ = mappings in
				let mappings = opaque_mapping, name_mapping in
				pp_expression_in_statement ff ~mappings ~current expr
					~pp_statement:(
						begin fun ff () ->
							pp_return ff (Some (fun ff () ->
								pp_expression ff ~mappings ~current ~outside:`lowest expr))
						end)
			| None ->
				pp_return ff None
			end
		end
	) and pp_statement_list
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		?(in_expression: bool = false)
		~(null_statement : bool)
		(stmts: Semantics.statement list)
		: unit =
	(
		if stmts = [] && null_statement then (
			pp_null_statement ff ();
		) else (
			let rec loop xs = (
				begin match xs with
				| stmt :: [] when in_expression ->
					pp_statement ff ~mappings ~current ~in_expression:true stmt
				| stmt :: xr ->
					pp_statement ff ~mappings ~current stmt;
					loop xr
				| [] ->
					()
				end
			) in
			loop stmts
		)
	) and pp_alias
		(ff: formatter)
		~(mappings: Semantics.language_mapping * Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(enum_of_element: Semantics.full_enum_type StringMap.t)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		(name: string)
		(source_item: Semantics.named_item)
		: unit =
	(
		let pp_subprogram_alias source_name prototype = (
			pp_print_space ff ();
			let _, args, _, ret = prototype in
			begin match ret with
			| `pointer (`function_type _) -> (* anonymous function pointer *)
				let source_name = omit_long_word (46 - String.length name) source_name in
				fprintf ff "--  %s renames %s (anonymous type)" name source_name
			| _ ->
				pp_open_box ff indent;
				let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let anonymous_mapping = add_arguments_to_anonymous_mapping ~name_mapping ~current args anonymous_mapping in
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				ignore (pp_prototype ff ~mappings ~current ~hidden_packages ~name:(Some name) prototype);
				fprintf ff "@ renames %s;" source_name;
				pp_close_box ff ()
			end
		) in
		let pp_subprogram_overload_alias source_name (source_item: Semantics.function_item) = (
			begin try
				let language_mapping, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				let overload = List.assq source_item language_mapping.Semantics.lm_overload in
				List.iter (fun prototype ->
					pp_print_space ff ();
					pp_open_box ff indent;
					ignore (pp_prototype ff ~mappings ~current ~name:(Some name) prototype);
					fprintf ff "@ renames %s;" source_name;
					pp_close_box ff ()
				) overload
			with Not_found ->
				()
			end
		) in
		let source_name =
			let `named (source_ps, source_name, _, _) = source_item in
			let kind =
				begin match source_item with
				| `named (_, _, `opaque_enum, _) -> `opaque_enum
				| `named (_, _, `opaque_struct, _) -> `opaque_struct
				| `named (_, _, `opaque_union, _) -> `opaque_union
				| _ -> `namespace
				end
			in
			let _, _, name_mapping, _ = mappings in
			ada_name_of current ~hidden_packages source_ps source_name kind name_mapping
		in
		begin match source_item with
		| `named (source_ps, source_name, `enum_element _, _) ->
			pp_print_space ff ();
			pp_open_box ff indent;
			let t = StringMap.find source_name enum_of_element in
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			fprintf ff "function %s return %a@ renames %s;"
				name
				(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`name) (t :> Semantics.all_type)
				(ada_name_of current source_ps source_name `namespace name_mapping);
			pp_close_box ff ()
		| `named (_, _, (`typedef (`function_type _)) , _) ->
			fprintf ff "@ --  subtype %s is ... (function type)" name
		| `named (_, _, #Semantics.named_type_var, _) as source_item ->
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			pp_subtype ff name
				(pp_type_name ~mappings ~current ~hidden_packages ?hiding:None ~where:`subtype) source_item
		| `named (_, _, `extern _, _) as source_item ->
			begin match source_item with
			| `named (_, _, `extern ((`function_type prototype), _), _) as source_item ->
				pp_subprogram_alias source_name prototype;
				pp_subprogram_overload_alias source_name source_item
			| `named (_, _, `extern (t, _), _) ->
				pp_print_space ff ();
				pp_open_box ff indent;
				let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				fprintf ff "%s : %a@ renames %s;"
					name
					(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`rename) t
					source_name;
				pp_close_box ff ()
			end
		| `named (_, _, `variable _, _) ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `function_forward (_, (`function_type _)), _) ->
			fprintf ff "@ --  function %s renames ..." name
		| `named (_, _, `function_definition (_, (`function_type prototype), _), _) as source_item ->
			pp_subprogram_alias source_name prototype;
			pp_subprogram_overload_alias source_name source_item
		| `named (_, _, `defined_operator _, _) ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_attributes, _) ->
			fprintf ff "@ --  %s renames __attribute__((...)) (macro)" name
		| `named (_, _, `defined_storage_class _, _) ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_type_specifier _, _) ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_type_qualifier _, _) ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_typedef source_item, _) ->
			begin match Semantics.resolve_typedef source_item with
			| `void ->
				fprintf ff "@ --  %s renames void (macro)" name
			| `volatile `void ->
				fprintf ff "@ --  %s renames void volatile (macro)" name
			| `function_type _ ->
				fprintf ff "@ --  %s renames ... (function type)" name
			| _ ->
				let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				pp_subtype ff name
					(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) source_item
			end
		| `named (_, _, `defined_opaque_type source_item, _) ->
			let tag, kind =
				match source_item with
				| `named (_, tag, `opaque_enum, _) -> tag, "enum"
				| `named (_, tag, `opaque_struct, _) -> tag, "struct"
				| `named (_, tag, `opaque_union, _) -> tag, "union"
			in
			fprintf ff "@ --  %s renames %s %s" name kind tag
		| `named (source_ps, _, `defined_element_access (t, route), _) ->
			let prototype = prototype_for_element_access source_ps t route in
			pp_subprogram_alias source_name prototype
		| `named (_, _, `defined_expression expr, _) ->
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			if Semantics.is_static_expression expr then (
				begin match expr with
				| `int_literal _, _
				| `float_literal _, _ ->
					pp_universal_constant_object ff name
						(fun ff () -> pp_print_string ff source_name)
				| `cast (_, t1 as e1), t2 when List.length (Finding.find_all_cast_in_source_item [] (source_item :> Semantics.source_item)) = 1 ->
					(* calling Unchecked_Conversion is not static expression, use trick *)
					pp_print_space ff ();
					pp_open_box ff indent;
					fprintf ff "function %s (@,Value : " name;
					begin
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						pp_type_name ff ~mappings ~current ~where:`rename t1 (* unconstrained, but not using anonymous access *)
					end;
					fprintf ff " := ";
					begin
						let mappings = opaque_mapping, name_mapping in
						pp_expression ff ~mappings ~current ~outside:`lowest e1
					end;
					fprintf ff ")@ return ";
					begin
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						pp_type_name ff ~mappings ~current ~hidden_packages ~where:`subtype t2
					end;
					fprintf ff "@ renames %s;" source_name;
					pp_close_box ff ()
				| _, t ->
					pp_print_space ff ();
					pp_open_box ff indent;
					fprintf ff "%s : %a@ renames %s;" name
						(pp_type_name ~mappings ~current ~hidden_packages ?hiding:None ~where:`subtype) t
						source_name;
					pp_close_box ff ()
				end
			) else (
				pp_print_space ff ();
				pp_open_box ff indent;
				let prototype = `cdecl, [], `none, snd expr in (* calling-convention be ignored *)
				ignore (pp_prototype ff ~mappings  ~current ~name:(Some name) prototype);
				fprintf ff " renames %s;" source_name;
				pp_close_box ff ()
			)
		| `named (_, _, `defined_generic_expression _, _) ->
			let source_name = omit_long_word (46 - String.length name) source_name in
			fprintf ff "@ --  %s renames %s (function macro)" name source_name
		| `named (_, _, `defined_generic_statement _, _) ->
			let source_name = omit_long_word (46 - String.length name) source_name in
			fprintf ff "@ --  %s renames %s (function macro)" name source_name
		| `named (_, _, `defined_any message, _) ->
			let source_name = omit_long_word (60 - String.length name - String.length message) source_name in
			fprintf ff "@ --  %s renames %s (%s)" name source_name message
		| `named (_, _, `defined_alias _, _) ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `generic_value _, _) ->
			assert false (* does not come here *)
		end
	) and pp_named
		(ff: formatter)
		~(mappings: Semantics.language_mapping * Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(enum_of_element: Semantics.full_enum_type StringMap.t)
		~(current: string)
		?(hidden_packages: StringSet.t = StringSet.empty)
		(item: Semantics.named_item)
		: unit =
	(
		begin match item with
		| `named (_, _, `enum_element _, _) ->
			() (* it shell be declared with enum-type *)
		| `named (_, _, #Semantics.named_type_var, _) as item ->
			pp_named_type ff ~mappings ~current ~hidden_packages item
		| `named (ps, name, `extern (t, alias), attrs) as item ->
			let language_mapping, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			if Naming.is_hidden ps item name_mapping then (
				fprintf ff "@ --  function %s ... (hidden by macro)" name
			) else if (
				match t with
				| `named (_, _, #Semantics.opaque_type_var, _) as t when Semantics.is_opaque t opaque_mapping -> true
				| _ -> false)
			then (
				fprintf ff "@ --  extern %s (opaque type)" name
			) else (
				let ada_name = ada_name_of current ps name `namespace name_mapping in
				begin match item with
				| `named (_, _, `extern ((`function_type prototype), _), _) as item ->
					let mappings = opaque_mapping, name_mapping, anonymous_mapping in
					pp_print_space ff ();
					pp_open_box ff indent;
					ignore (pp_prototype ff ~mappings ~current ~name:(Some ada_name) prototype);
					pp_print_string ff ";";
					pp_close_box ff ();
					begin try
						let overload = List.assq item language_mapping.Semantics.lm_overload in
						List.iter (fun prototype ->
							pp_print_space ff ();
							pp_open_box ff indent;
							ignore (pp_prototype ff ~mappings ~current ~name:(Some ada_name) prototype);
							pp_print_string ff ";";
							pp_close_box ff ()
						) overload
					with Not_found ->
						()
					end;
					if attrs.Semantics.at_noreturn then pp_pragma_noreturn ff ada_name
				| _ ->
					pp_print_space ff ();
					pp_open_box ff indent;
					pp_print_string ff ada_name;
					pp_print_string ff " :";
					pp_print_space ff ();
					pp_print_string ff "aliased";
					pp_print_space ff ();
					let mappings = opaque_mapping, name_mapping, anonymous_mapping in
					pp_type_name ff ~mappings ~current ~where:`extern t;
					pp_print_char ff ';';
					pp_close_box ff ();
				end;
				let c_name =
					begin match alias with
					| `alias c_name ->
						if c_name.[0] = '_' then String.sub c_name 1 (String.length c_name - 1) else
						c_name
					| `none ->
						name
					end
				in
				pp_pragma_import ff attrs.Semantics.at_conventions ada_name c_name;
				if attrs.Semantics.at_weak_link = `weak then (
					pp_pragma_weak_external ff ada_name
				)
			)
		| `named (ps, name, `variable (t, init), _) ->
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let ada_name = ada_name_of current ps name `namespace name_mapping in
			pp_print_space ff ();
			pp_open_box ff indent;
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			fprintf ff "%s : %s%a" ada_name
				(match t with `const _ -> "constant " | _ -> "")
				(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) t;
			begin match init with
			| Some init ->
				pp_print_string ff " := ";
				let mappings = opaque_mapping, name_mapping in
				pp_expression ff ~mappings ~current ~outside:`lowest init
			| None ->
				()
			end;
			pp_print_char ff ';';
			pp_close_box ff ()
		| `named (ps, name, `function_forward (k, (`function_type prototype)), _) ->
			begin match k with
			| `builtin ->
				let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				let ada_name = ada_name_of current ps name `namespace name_mapping in
				pp_print_space ff ();
				pp_open_box ff indent;
				ignore (pp_prototype ff ~mappings ~current ~name:(Some ada_name) prototype);
				pp_print_string ff ";";
				pp_close_box ff ();
				pp_pragma_import ff `intrinsic ada_name name
			| `static ->
				(* function will be translated from `function_definition *)
				fprintf ff "@ --  function %s ..." name
			end
		| `named (ps, name, `function_definition (storage_class, (`function_type prototype), _), attrs) ->
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			let ada_name = ada_name_of current ps name `namespace name_mapping in
			pp_print_space ff ();
			pp_open_box ff indent;
			ignore (pp_prototype ff ~mappings ~current ~name:(Some ada_name) prototype);
			pp_print_string ff ";";
			pp_close_box ff ();
			begin match storage_class with
			| `extern_inline ->
				(* use extern, ignore body *)
				pp_pragma_import ff attrs.Semantics.at_conventions ada_name name
			| `static ->
				pp_pragma_convention ff attrs.Semantics.at_conventions ada_name;
				begin match attrs.Semantics.at_inline with
				| `inline ->
					pp_pragma_inline ff ada_name
				| `always_inline ->
					pp_pragma_inline ff ~always:true ada_name
				| `noinline | `none ->
					()
				end
			| `none ->
				fprintf ff "@ **** %s / unimplemented. ****\n" name;
				assert false
			end
		| `named (_, name, `defined_operator op, _) ->
			begin match op with
			| `and_assign | `or_assign | `xor_assign as op ->
				let op_s =
					begin match op with
					| `and_assign -> "&="
					| `or_assign -> "|="
					| `xor_assign -> "^="
					end
				in
				fprintf ff "@ --  procedure %s (Left : in out T; Right : T) renames \"%s\"" name op_s
			| `ampersand | `and_then | `caret | `exclamation
			| `ne | `or_else | `tilde | `vertical as op ->
				let op_s =
					begin match op with
					| `ampersand -> "&"
					| `and_then -> "&&"
					| `caret -> "^"
					| `exclamation -> "!"
					| `ne -> "!="
					| `or_else -> "||"
					| `tilde -> "~"
					| `vertical -> "|"
					end
				in
				fprintf ff "@ --  function %s (Left, Right : T) return T renames \"%s\"" name op_s
			end
		| `named (ps, name, `defined_attributes, attrs) ->
			if attrs = {Semantics.no_attributes with Semantics.at_conventions = attrs.Semantics.at_conventions} then (
				let _, _, name_mapping, _ = mappings in
				let ada_name = ada_name_of current ps name `namespace name_mapping in
				let conv = attrs.Semantics.at_conventions in
				begin match conv with
				| `fastcall | `thiscall ->
					fprintf ff "@ --  pragma Convention_Identifier (%s, %s);"
						name (convention_identifier conv)
				| _ ->
					pp_pragma_convention_identifier ff ada_name conv
				end
			) else (
				(* available.h (darwin9) has very long identifiers *)
				let name = omit_long_word 60 name in
				fprintf ff "@ --  %s (attribute)" name
			)
		| `named (_, name, `defined_storage_class storage_class, _) ->
			begin match storage_class with
			| `auto ->
				fprintf ff "@ --  %s (alias of static)" name
			| `extern ->
				fprintf ff "@ --  %s (alias of extern)" name
			| `register ->
				fprintf ff "@ --  %s (alias of register)" name
			| `static ->
				fprintf ff "@ --  %s (alias of static)" name
			| `typedef ->
				fprintf ff "@ --  %s (alias of typedef)" name
			end
		| `named (_, name, `defined_type_specifier ts, _) ->
			begin match ts with
			| `complex ->
				fprintf ff "@ --  %s (alias of _Complex)" name
			| `imaginary ->
				fprintf ff "@ --  %s (alias of _Imaginary)" name
			end
		| `named (_, name, `defined_type_qualifier q, _) ->
			begin match q with
			| `const ->
				fprintf ff "@ --  %s (alias of const)" name
			| `restrict ->
				fprintf ff "@ --  %s (alias of restrict)" name
			| `volatile ->
				fprintf ff "@ --  %s (alias of volatile)" name
			end
		| `named (ps, name, `defined_typedef t, _) ->
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			pp_typedef_without_language_mapping ff ~mappings ~current ~where:`macro ps name t
		| `named (_, name, `defined_opaque_type t, _) ->
			let target_type_name =
				match t with
				| `named (_, tag, `opaque_enum, _) -> "enum " ^ tag
				| `named (_, tag, `opaque_struct, _) -> "struct " ^ tag
				| `named (_, tag, `opaque_union, _) -> "union " ^ tag
			in
			let target_type_name =
				omit_long_word (59 - String.length name) target_type_name
			in
			fprintf ff "@ --  subtype %s is %s;" name target_type_name
		| `named (ps, name, `defined_element_access (t, route), _) ->
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_space ff ();
			pp_open_box ff indent;
			let prototype = prototype_for_element_access ps t route in
			ignore (pp_prototype ff ~mappings ~current ~name:(Some name) prototype);
			pp_print_string ff ";";
			pp_close_box ff ();
			pp_pragma_inline ff ~always:true name
		| `named (ps, name, `defined_expression expr, _) ->
			let _, opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let name = ada_name_of current ps name `namespace name_mapping in
			if Semantics.is_static_expression expr then (
				begin match expr with
				| `ref_function (func: Semantics.function_item), _ ->
					pp_alias ff ~mappings ~enum_of_element ~current ~hidden_packages name (func :> Semantics.named_item)
				| `int_literal _, _ | `float_literal _, _ ->
					let mappings = opaque_mapping, name_mapping in
					pp_universal_constant_object ff name
						(fun ff () -> pp_expression ff ~mappings ~current ~outside:`lowest expr)
				| `cast (_, t1 as e1), t2 when List.length (Finding.find_all_cast_in_source_item [] (item :> Semantics.source_item)) = 1 ->
					(* calling Unchecked_Conversion is not static expression, use trick *)
					pp_print_space ff ();
					pp_open_box ff indent;
					fprintf ff "function %s (@,Value : " name;
					begin
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						pp_type_name ff ~mappings ~current ~where:`rename t1 (* unconstrained, but not using anonymous access *)
					end;
					fprintf ff " := ";
					begin
						let mappings = opaque_mapping, name_mapping in
						pp_expression ff ~mappings ~current ~outside:`lowest e1
					end;
					fprintf ff ")@ return ";
					begin
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						pp_type_name ff ~mappings ~current ~where:`subtype t2
					end;
					fprintf ff "@ renames Cast;";
					pp_close_box ff ()
				| _, t ->
					pp_print_space ff ();
					pp_open_box ff indent;
					fprintf ff "%s :@ constant@ " name;
					begin
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						pp_type_name ff ~mappings ~current ~hidden_packages ~where:`subtype t
					end;
					fprintf ff " :=@ ";
					begin
						let mappings = opaque_mapping, name_mapping in
						pp_expression ff ~mappings ~current ~outside:`lowest expr
					end;
					pp_print_char ff ';';
					pp_close_box ff ()
				end
			) else (
				begin match expr with
				| `ref_object (obj, _), _ ->
					pp_alias ff ~mappings ~enum_of_element ~current ~hidden_packages name (obj :> Semantics.named_item)
				| _ ->
					pp_print_space ff ();
					pp_open_box ff indent;
					let prototype = `cdecl, [], `none, snd expr in (* calling-convention be ignored *)
					let mappings = opaque_mapping, name_mapping, anonymous_mapping in
					ignore (pp_prototype ff ~mappings ~current ~name:(Some name) prototype);
					pp_print_string ff ";";
					pp_close_box ff ();
					pp_pragma_inline ff ~always:true name
				end
			)
		| `named (_, name, `defined_generic_expression _, _) ->
			fprintf ff "@ --  %s (function macro)" name
		| `named (_, name, `defined_generic_statement _, _) ->
			fprintf ff "@ --  %s (function macro)" name
		| `named (_, name, `defined_any message, _) ->
			let name = omit_long_word (69 - String.length message) name in
			fprintf ff "@ --  %s (%s)" name message
		| `named (ps, name, `defined_alias source_item, _) ->
			begin match source_item with
			| `named (_, _, `enum _, _)
			| `named (_, _, `struct_type _, _)
			| `named (_, _, `union _, _) ->
				()
			| _ ->
				let kind =
					begin match source_item with
					| `named (_, _, `opaque_enum, _) -> `opaque_enum
					| `named (_, _, `opaque_struct, _) -> `opaque_struct
					| `named (_, _, `opaque_union, _) -> `opaque_union
					| _ -> `namespace
					end
				in
				let _, _, name_mapping, _ = mappings in
				let name = ada_name_of current ps name kind name_mapping in
				pp_alias ff ~mappings ~enum_of_element ~current ~hidden_packages name source_item
			end
		| `named (_, _, `generic_value _, _) ->
			assert false (* does not come here *)
		end
	);;
	
	let pp_named_body
		(ff: formatter)
		~(mappings: Semantics.opaque_mapping * name_mapping * anonymous_mapping)
		~(current: string)
		(item: Semantics.named_item)
		: unit =
	(
		begin match item with
		| `named (ps, name, `function_definition (storage_class, (`function_type prototype), stmts), _) ->
			begin match storage_class with
			| `extern_inline ->
				()
			| `static ->
				let opaque_mapping, name_mapping, anonymous_mapping = mappings in
				let ada_name = ada_name_of current ps name `namespace name_mapping in
				let _, args, _, _ = prototype in
				let modified_arguments =
					List.filter (fun arg ->
						List.exists (Finding.lvalue_referenced_in_statement arg) stmts
					) args
				in
				let has_local = modified_arguments <> [] in
				pp_print_space ff ();
				if has_local then pp_open_vbox ff indent;
				if has_local then pp_open_box ff 0;
				pp_open_box ff indent;
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				let name_mapping = pp_prototype ff ~mappings ~current ~name:(Some ada_name) prototype in
				(* start of local declarations *)
				if has_local then pp_close_box ff ();
				fprintf ff "@ is";
				pp_close_box ff ();
				(* modified arguments to local variable *)
				let name_mapping =
					List.fold_left (fun name_mapping arg ->
						pp_print_space ff ();
						let `named (ps, arg_name, `variable (arg_t, _), _) = arg in
						let ada_name = ada_simple_name_of ps arg_name `namespace name_mapping in
						let mutable_name = "Mutable_" ^ ada_name in
						let mappings = opaque_mapping, name_mapping, anonymous_mapping in
						fprintf ff "%s : %a := %s;"
							mutable_name
							(pp_type_name ~mappings ~current ?hidden_packages:None ?hiding:None ~where:`subtype) arg_t
							ada_name;
						let (filename, _, _, _), _ = ps in
						let rel_filename, package_name, map = StringMap.find filename name_mapping in
						let map = Naming.add `namespace arg_name mutable_name map in
						StringMap.add filename (rel_filename, package_name, map) name_mapping
					) name_mapping modified_arguments
				in
				(* end of local declarations *)
				if has_local then pp_close_box ff ();
				pp_begin ff ();
				let mappings = opaque_mapping, name_mapping, anonymous_mapping in
				pp_statement_list ff ~mappings ~current
					~null_statement:true stmts;
				pp_end ff ~label:ada_name ()
			| `none ->
				fprintf ff "@ **** %s / unimplemented. ****\n" name;
				assert false
			end
		| `named (ps, name, `defined_element_access (t, route), _) ->
			let opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_space ff ();
			pp_open_box ff indent;
			let prototype = prototype_for_element_access ps t route in
			ignore (pp_prototype ff ~mappings ~current ~name:(Some name) prototype);
			fprintf ff " is";
			pp_close_box ff ();
			pp_begin ff ();
			pp_return ff (Some (
				begin fun ff () ->
					pp_print_string ff "Object";
					let (_: Semantics.all_type) =
						List.fold_left (fun (t: Semantics.all_type) field ->
							let items =
								begin match t with
								| `anonymous (_, `struct_type (_, items))
								| `anonymous (_, `union items)
								| `named (_, _, `struct_type (_, items), _)
								| `named (_, _, `union items, _) ->
									items
								| _ ->
									assert false (* does not come here *)
								end
							in
							let field_map, _ = name_mapping_for_struct_items items in
							let name, field_t, _, _ = field in
							fprintf ff ".%s" (StringMap.find name field_map);
							field_t
						) (t :> Semantics.all_type) route
					in
					()
				end));
			pp_end ff ~label:name ()
		| `named (_, name, `defined_generic_expression _, _) ->
			fprintf ff "@ **** %s / unimplemented. ****\n" name;
			assert false
		| `named (_, name, `defined_generic_statement _, _) ->
			fprintf ff "@ **** %s / unimplemented. ****\n" name;
			assert false
		| `named (ps, name, `defined_expression expr, _) ->
			assert (not (Semantics.is_static_expression expr));
			let opaque_mapping, name_mapping, anonymous_mapping = mappings in
			let name = ada_name_of current ps name `namespace name_mapping in
			let prototype = `cdecl, [], `none, snd expr in (* calling-convention be ignored *)
			pp_print_space ff ();
			pp_open_box ff indent;
			let mappings = opaque_mapping, name_mapping, anonymous_mapping in
			let name_mapping = pp_prototype ff ~mappings ~current ~name:(Some name) prototype in
			fprintf ff "@ is";
			pp_close_box ff ();
			pp_begin ff ();
			let mappings = opaque_mapping, name_mapping in
			pp_expression_in_statement ff ~mappings ~current expr
				~pp_statement:(
					begin fun ff () ->
						pp_return ff (Some (fun ff () ->
							pp_expression ff ~mappings ~current ~outside:`lowest expr))
					end);
			pp_end ff ~label:name ()
		| _ ->
			assert false (* does not come here *)
		end
	);;
	
	let pp_include_point
		(ff: formatter)
		(name_mapping: name_mapping)
		(header_filename: string)
		: unit =
	(
		let rel_filename, _, _ = StringMap.find header_filename name_mapping in
		fprintf ff "@ --  #include <%s>" rel_filename
	);;
	
	let pp_dir_package_spec
		(ff: formatter)
		~(name: string)
		: unit =
	(
		pp_package_spec
			ff
			~with_packages:[]
			~name:("C." ^ name)
			~kind:`preelaborate
			~pp_contents:(fun (_: formatter) () -> ())
			~pp_private:None
	);;
	
	let pp_translated_package_spec
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		~(predefined_types: (Semantics.predefined_type * int) list * Semantics.typedef_type list)
		~(derived_types: Semantics.derived_type list)
		~(enum_of_element: Semantics.full_enum_type StringMap.t)
		~(opaque_mapping: Semantics.opaque_mapping)
		~(name_mapping: name_mapping)
		~(name: string)
		(items: Semantics.source_item list)
		: unit =
	(
		let ptrdiff_t = Semantics.find_ptrdiff_t predefined_types in
		let items_having_bodies = List.filter (
			begin fun (item: Semantics.source_item) ->
				begin match item with
				| `named (_, _, `function_definition (`extern_inline, _, _), _) (* use extern, ignore body *)
				| `named (_, _, `defined_generic_expression _, _) (* unimplemented for translating function macro *)
				| `named (_, _, `defined_generic_statement _, _) ->
					false
				| _ ->
					true
				end
			end)
			items
		in
		(* used casts *)
		let casts = List.fold_left Finding.find_all_cast_in_source_item [] items_having_bodies in
		let casts = List.fold_left (Finding.find_all_pointer_arithmetic_in_source_item ptrdiff_t) casts items_having_bodies in
		(* used sized arrays *)
		let used_sized_arrays = List.fold_left Finding.find_all_sized_array_in_source_item [] items in
		let all_sized_arrays = List.fold_left Finding.find_all_sized_array_in_derived_type used_sized_arrays derived_types in
		(* with clauses *)
		let with_packages =
			let items = (items :> Semantics.all_item list) in
			let items =
				if name <> "" then items else (
					let items = List.rev_append (fst (List.split (fst predefined_types)) :> Semantics.all_item list) items in
					let items = List.rev_append (snd predefined_types :> Semantics.all_item list) items in
					items
				)
			in
			referencing_packages ~language_mapping ~name_mapping ~derived_types ~opaque_mapping ~casts ~current:name items
		in
		let hidden_packages = hidden_packages with_packages ~current:name in
		(* opaque types *)
		let has_private_part =
			List.exists (fun item ->
				begin match item with
				| `named (_, _, (`opaque_enum | `opaque_struct | `opaque_union), _) as item ->
					Semantics.is_opaque item opaque_mapping
				| _ ->
					false
				end
			) items
		in
		(* finally, output *)
		let current = name in
		pp_package_spec
			ff
			~with_packages
			~name:(if current = "" then "C" else "C." ^ current)
			~kind:`preelaborate
			~pp_contents:(fun ff () ->
				if current = "" then (
					fprintf ff "@ use type System.Address;";
					(* predefined types *)
					List.iter (fun item ->
						pp_predefined_type ff ~language_mapping item
					) (fst predefined_types);
					(* special-typedefs (size_t is required before any array types) *)
					List.iter (fun item ->
						begin match item with
						| `named (_, current, `typedef (#Semantics.predefined_type as t), _) ->
							pp_newtype_of_predefind_type ff ~language_mapping current t
						| _ ->
							assert false (* does not come here *)
						end
					) (snd predefined_types);
					(* derived types of predefined types *)
					List.iter (fun (item, _) ->
						ignore (pp_derived_types_for_the_type ff ~mappings:(language_mapping, opaque_mapping, name_mapping, [])
							~casts ~sized_arrays:all_sized_arrays
							~current (item :> Semantics.all_type) derived_types [])
					) (fst predefined_types);
					(* derived types of special typedefs *)
					List.iter (fun item ->
						begin match item with
						| `named (_, current, `typedef #Semantics.predefined_type, _) as item ->
							ignore (pp_derived_types_for_the_type ff ~mappings:(language_mapping, opaque_mapping, name_mapping, [])
								~casts ~sized_arrays:all_sized_arrays
								~current ~special:true (item :> Semantics.all_type) derived_types [])
						| _ ->
							assert false (* does not come here *)
						end
					) (snd predefined_types);
					(* primitives for predefined type (after volatile types) *)
					List.iter (fun item ->
						pp_primitives_for_predefined_type ff item
					) (fst predefined_types);
					pp_pragma_import ff `intrinsic "Shift_Left" "Shift_Left";
					pp_pragma_import ff `intrinsic "Shift_Right" "Shift_Right";
					pp_pragma_import ff `intrinsic "Shift_Right_Arithmetic" "Shift_Right_Arithmetic"
				) else (
					(* casts between types belong to another packages *)
					let belongs_to (t: Semantics.all_type) (items: Semantics.source_item list): bool = (
						begin match t with
						| #Semantics.derived_type as t ->
							List.exists (fun item ->
								begin match item with
								| #Semantics.anonymous_type
								| `named (_, _, #Semantics.named_type_var, _) as item ->
									Semantics.is_derived_type (fun x -> x == item) t
								| _ ->
									false
								end
							) items
						| _ ->
							List.exists (fun item ->
								begin match item with
								| #Semantics.anonymous_type
								| `named (_, _, #Semantics.named_type_var, _) as item ->
									item == t
								| _ ->
									false
								end
							) items
						end
					) in
					List.iter (fun (x, y as pair) ->
						if not (belongs_to x items) && not (belongs_to y items) then (
							pp_unchecked_conversion ff ~mappings:(opaque_mapping, name_mapping, []) ~current pair
						)
					) casts;
					(* sized arrays of types in another packages *)
					List.iter (fun sized_array ->
						begin match sized_array with
						| `array (Some _, t) as sized_array ->
							begin match t with
							| `named (ps, _, _, _) ->
								let (filename, _, _, _), _ = ps in
								let _, package_name, _ = StringMap.find filename name_mapping in
								if package_name <> current then (
									pp_derived_type ff ~mappings:(language_mapping, opaque_mapping, name_mapping, []) ~current
										None (sized_array :> Semantics.derived_type)
								)
							| _ ->
								()
							end
						| _ ->
							assert false (* does not come here *)
						end
					) used_sized_arrays
				);
				(* items *)
				let rec extern_exists_before
					(item: Semantics.function_definition_item)
					(xs: Semantics.source_item list)
					: bool =
				(
					let `named (_, current, _, _) = item in
					begin match xs with
					| `named (_, xname, `extern ((`function_type _), _), _) :: _ when xname = current ->
						true
					| x :: _ when x == (item :> Semantics.source_item) ->
						false
					| _ :: xr ->
						extern_exists_before item xr
					| [] ->
						false
					end
				) in
				ignore (
					List.fold_left (fun (anonymous_mapping, done_list) item ->
						(* source item *)
						let anonymous_mapping =
							begin match item with
							| #Semantics.anonymous_type as item ->
								let hash = hash_name item in
								let anonymous_mapping = (item, (current, hash)) :: anonymous_mapping in
								pp_anonymous_type ff ~mappings:(opaque_mapping, name_mapping, anonymous_mapping)
									~current ~hidden_packages item;
								anonymous_mapping
							| `named (_, _, `function_definition (`extern_inline, _, _), _) as item when extern_exists_before item items ->
								anonymous_mapping
							| `named _ as item ->
								pp_named ff ~mappings:(language_mapping, opaque_mapping, name_mapping, anonymous_mapping) ~enum_of_element
									~current ~hidden_packages item;
								anonymous_mapping
							| `anonymous_alias (source_ps, item) ->
								let hash = hash_name item in
								let (filename, _, _, _), _ = source_ps in
								let _, package_name, _ = StringMap.find filename name_mapping in
								let anonymous_mapping = ((item :> Semantics.anonymous_type), (package_name, hash)) :: anonymous_mapping in
								anonymous_mapping
							| `include_point included ->
								pp_include_point ff name_mapping included;
								anonymous_mapping
							end
						in
						(* casts and derived types *)
						let done_list =
							begin match item with
							| #Semantics.anonymous_type
							| `named (_, _, #Semantics.named_type_var, _) as t ->
								(* output casts *)
								List.iter (fun (x, y as pair) ->
									if x == (t :> Semantics.all_type) || y == (t :> Semantics.all_type) then (
										pp_unchecked_conversion ff ~mappings:(opaque_mapping, name_mapping, anonymous_mapping) ~current pair
									)
								) casts;
								(* output derived types *)
								pp_derived_types_for_the_type ff ~mappings:(language_mapping, opaque_mapping, name_mapping, anonymous_mapping)
									~casts ~sized_arrays:all_sized_arrays
									~current (t :> Semantics.all_type) derived_types done_list
							| _ ->
								done_list
							end
						in
						anonymous_mapping, done_list
					) ([], []) items
				)
			)
			~pp_private:(
				if has_private_part then Some (
					begin fun ff () ->
						List.iter (fun item ->
							begin match item with
							| `named (ps, t_name, (`opaque_enum | `opaque_struct | `opaque_union as kind), _) as item
								when Semantics.is_opaque item opaque_mapping
							->
								let t_name = ada_name_of current ps t_name kind name_mapping in
								pp_type ff t_name pp_record_definition []
							| _ ->
								()
							end
						) items
					end
				) else None
			)
	);;
	
	let pp_translated_package_body
		(ff: formatter)
		~(opaque_mapping: Semantics.opaque_mapping)
		~(name_mapping: name_mapping)
		~(name: string)
		(items: Semantics.source_item list)
		: unit =
	(
		let has_asm =
			List.exists (fun item ->
				body_required_for_single_item item && Finding.asm_exists_in_source_item item
			) items
		in
		let with_packages =
			if has_asm then (
				["System.Machine_Code", (`none, `none, `use)]
			) else (
				[]
			)
		in
		let current = name in
		pp_package_body
			ff
			~with_packages
			~name:("C." ^ current)
			~pp_contents:(fun ff () ->
				ignore (
					List.fold_left (fun anonymous_mapping item ->
						begin match item with
						| #Semantics.anonymous_type as item ->
							let hash = hash_name item in
							(item, (current, hash)) :: anonymous_mapping
						| `named _ as item when body_required_for_single_item item ->
							pp_named_body ff ~mappings:(opaque_mapping, name_mapping, anonymous_mapping) ~current:current item;
							anonymous_mapping
						| _ ->
							anonymous_mapping
						end
					) [] items
				)
			)
	);;
	
end;;
