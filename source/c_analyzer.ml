open C_filename;;
open C_literals;;
open C_semantics;;
open C_semantics_build_decl;;
open C_semantics_build_expr;;
open C_semantics_build_type;;
open C_syntax;;
open C_syntax_traversing;;
open Position;;

let list_remove (a: 'a) (xs: 'a list): 'a list = (
	let rec loop a ys xs orig_xs = (
		begin match xs with
		| x :: xr ->
			if x == a then (
				List.rev_append ys xr
			) else (
				loop a (x :: ys) xr orig_xs
			)
		| [] ->
			orig_xs
		end
	) in
	loop a [] xs xs
);;

module Analyzer
	(Literals: LiteralsType)
	(Syntax: SyntaxType
		with module Literals := Literals)
	(Semantics: SemanticsType
		with module Literals := Literals) =
struct
	module Traversing = Traversing (Literals) (Syntax);;
	module Typing = Typing (Literals) (Semantics);;
	module Declaring = Declaring (Literals) (Semantics) (Typing);;
	module Expressing = Expressing (Literals) (Semantics) (Typing);;
	open Literals;;
	open Semantics;;
	
	(* error messages *)
	
	let is_undeclared (s: string): string =
		s ^ " is undeclared.";;
	let is_not_a_struct_or_union (s: string): string =
		s ^ " is not a struct or union.";;
	let unalignable_struct_item (s: string): string =
		"the type of " ^ s ^ " can not be aligned.";;
	let not_had_bit_width_int (size: int): string =
		"this environment does not have " ^ string_of_int (size * 8) ^ " bit-integer.";;
	let inapplicable_attribute_mode: string =
		"attribute \"__mode__\" can not be applied to not int type.";
	
	(* in *)
	
	type 'a p = 'a Syntax.p;;
	type 'a e = 'a Syntax.e;;
	type 'a opt = 'a Syntax.opt;;
	
	(* namespace *)
	
	let opaque_mapping (namespace: namespace): opaque_mapping = (
		let to_opaque_mapping (type a) (type b)
			(opaques: a StringMap.t)
			(fulls: b StringMap.t)
			: (a * b) StringMap.t =
		(
			let add_to_opaque_mapping
				(opaques: a StringMap.t)
				(tag: string)
				(full: b)
				(r: (a * b) StringMap.t)
				: (a * b) StringMap.t =
			(
				StringMap.add tag (StringMap.find tag opaques, full) r
			) in
			StringMap.fold (add_to_opaque_mapping opaques) fulls StringMap.empty
		) in
		to_opaque_mapping namespace.ns_opaque_enum namespace.ns_enum,
		to_opaque_mapping namespace.ns_opaque_struct namespace.ns_struct,
		to_opaque_mapping namespace.ns_opaque_union namespace.ns_union
	);;
	
	(* detect type from specifiers *)
	
	type type_specifier_set = {
		ts_void: int;
		ts_char: int;
		ts_short: int;
		ts_int: int;
		ts_long: int;
		ts_float: int;
		ts_double: int;
		ts_signed: int;
		ts_unsigned: int;
		ts_bool: int;
		ts_imaginary: int;
		ts_complex: int;
		ts_int64: int;
		ts_builtin_va_list: int};;
	
	let no_type_specifier_set = {
		ts_void = 0;
		ts_char = 0;
		ts_short = 0;
		ts_int = 0;
		ts_long = 0;
		ts_float = 0;
		ts_double = 0;
		ts_signed = 0;
		ts_unsigned = 0;
		ts_bool = 0;
		ts_imaginary = 0;
		ts_complex = 0;
		ts_int64 = 0;
		ts_builtin_va_list = 0};;
	
	let get_type_by_specifier_set
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(ps: ranged_position)
		(bit_width_mode: bit_width_mode option)
		(set, named: type_specifier_set * all_type option)
		: all_type =
	(
		let invalid = "invalid combination of type-specifier(s)." in
		begin match named with
		| Some named ->
			if set <> no_type_specifier_set then (
				error ps invalid
			);
			if bit_width_mode <> None then (
				error ps "attribute \"__mode__\" was used for not int type."
			);
			named
		| None ->
			let t =
				if set = {no_type_specifier_set with ts_void = 1} then (
					`void
				) else if set = {no_type_specifier_set with ts_bool = 1} then (
					`bool
				) else if set = {no_type_specifier_set with ts_char = 1} then (
					`char
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_char = 1} then (
					`signed_char
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_char = 1} then (
					`unsigned_char
				) else if set = {no_type_specifier_set with ts_short = 1} then (
					`signed_short
				) else if set = {no_type_specifier_set with ts_short = 1; ts_int = 1} then (
					`signed_short
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_short = 1} then (
					`signed_short
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_short = 1; ts_int = 1} then (
					`signed_short
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_short = 1} then (
					`unsigned_short
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_short = 1; ts_int = 1} then (
					`unsigned_short
				) else if set = {no_type_specifier_set with ts_int = 1} then (
					`signed_int
				) else if set = {no_type_specifier_set with ts_signed = 1} then (
					`signed_int
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_int = 1} then (
					`signed_int
				) else if set = {no_type_specifier_set with ts_unsigned = 1} then (
					`unsigned_int
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_int = 1} then (
					`unsigned_int
				) else if set = {no_type_specifier_set with ts_long = 1} then (
					`signed_long
				) else if set = {no_type_specifier_set with ts_long = 1; ts_int = 1} then (
					`signed_long
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_long = 1} then (
					`signed_long
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_long = 1; ts_int = 1} then (
					`signed_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_long = 1} then (
					`unsigned_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_long = 1; ts_int = 1} then (
					`unsigned_long
				) else if set = {no_type_specifier_set with ts_long = 2} then (
					`signed_long_long
				) else if set = {no_type_specifier_set with ts_long = 2; ts_int = 1} then (
					`signed_long_long
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_long = 2} then (
					`signed_long_long
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_long = 2; ts_int = 1} then (
					`signed_long_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_long = 2} then (
					`unsigned_long_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_long = 2; ts_int = 1} then (
					`unsigned_long_long
				) else if set = {no_type_specifier_set with ts_float = 1} then (
					`float
				) else if set = {no_type_specifier_set with ts_double = 1} then (
					`double
				) else if set = {no_type_specifier_set with ts_long = 1; ts_double = 1} then (
					`long_double
				) else if set = {no_type_specifier_set with ts_float = 1; ts_imaginary = 1} then (
					`imaginary `float
				) else if set = {no_type_specifier_set with ts_double = 1; ts_imaginary = 1} then (
					`imaginary `double
				) else if set = {no_type_specifier_set with ts_long = 1; ts_double = 1; ts_imaginary = 1} then (
					`imaginary `long_double
				) else if set = {no_type_specifier_set with ts_float = 1; ts_complex = 1} then (
					`complex `float
				) else if set = {no_type_specifier_set with ts_double = 1; ts_complex = 1} then (
					`complex `double
				) else if set = {no_type_specifier_set with ts_long = 1; ts_double = 1; ts_complex = 1} then (
					`complex `long_double
				) else if set = {no_type_specifier_set with ts_int64 = 1} then (
					let t, err = Typing.select_bit_width_int predefined_types 8 `signed_long `signed_long_long in
					begin match err with
					| `none -> ()
					| `not_had size -> error ps (not_had_bit_width_int size)
					end;
					t
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_int64 = 1} then (
					let t, err = Typing.select_bit_width_int predefined_types 8 `signed_long `signed_long_long in
					begin match err with
					| `none -> ()
					| `not_had size -> error ps (not_had_bit_width_int size)
					end;
					t
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_int64 = 1} then (
					let t, err = Typing.select_bit_width_int predefined_types 8 `unsigned_long `unsigned_long_long in
					begin match err with
					| `none -> ()
					| `not_had size -> error ps (not_had_bit_width_int size)
					end;
					t
				) else if set = {no_type_specifier_set with ts_builtin_va_list = 1} then (
					`__builtin_va_list
				) else (
					error ps invalid;
					`signed_int
				)
			in
			begin match bit_width_mode with
			| Some bit_width_mode ->
				let t, err = Typing.apply_bit_width_mode predefined_types bit_width_mode (find_predefined_type t predefined_types) in
				begin match err with
				| `none -> ()
				| `not_had size -> error ps (not_had_bit_width_int size)
				| `not_int -> error ps inapplicable_attribute_mode
				end;
				t
			| None ->
				find_predefined_type t predefined_types
			end
		end
	);;
	
	type type_qualifier_set = {
		tq_const: bool;
		tq_restrict: bool;
		tq_volatile: bool};;
	
	let no_type_qualifier_set = {
		tq_const = false;
		tq_restrict = false;
		tq_volatile = false};;
	
	let get_type_by_qualifier_set
		(error: ranged_position -> string -> unit)
		(derived_types: derived_types)
		(ps: ranged_position)
		(t: all_type)
		(qualifiers: type_qualifier_set)
		: derived_types * all_type =
	(
		let t, derived_types =
			if qualifiers.tq_const then (
				Typing.find_const_type t derived_types
			) else (
				t, derived_types
			)
		in
		let t, derived_types =
			if qualifiers.tq_restrict then (
				begin match t with
				| `pointer _ as t ->
					Typing.find_restrict_type t derived_types
				| _ ->
					error ps "\"restrict\" could not be applied to not pointer type!";
					t, derived_types
				end
			) else (
				t, derived_types
			)
		in
		let t, derived_types =
			if qualifiers.tq_volatile then (
				Typing.find_volatile_type t derived_types
			) else (
				t, derived_types
			)
		in
		derived_types, t
	);;
	
	(* analyzing *)
	
	let rec handle_pragma
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(info: extra_info)
		(alignment_stack: alignment list)
		(mapping_options: mapping_options)
		(x: Syntax.pragma p)
		: derived_types * extra_info * alignment list * mapping_options =
	(
		let ps, (_, directive) = x in
		begin match directive with
		| `some directive ->
			begin match snd directive with
			| `gcc (_, gcc_directive) ->
				begin match gcc_directive with
				| `some gcc_directive ->
					begin match snd gcc_directive with
					| `fenv ->
						let info = {info with ei_fenv = true} in
						derived_types, info, alignment_stack, mapping_options
					| `poison _ ->
						error (fst gcc_directive) "unimplemented!";
						assert false
					| `visibility _ ->
						(* ignore #pragma visibility *)
						derived_types, info, alignment_stack, mapping_options
					| `system_header ->
						let info = {info with ei_system_header = true} in
						derived_types, info, alignment_stack, mapping_options
					end
				| `error ->
					derived_types, info, alignment_stack, mapping_options
				end
			| `instance (_, sql, decl) ->
				begin match sql, decl with
				| `some sql, `some decl ->
					let derived_types, n2, s2, base_type = handle_specifier_qualifier_list error predefined_types derived_types namespace [] (List.hd alignment_stack) sql in
					let derived_types, n2, s2, decl = handle_declarator error predefined_types derived_types n2 s2 base_type no_attributes decl in
					begin match decl with
					| Some (_, name, t, attrs) ->
						if n2 != namespace || (
							match s2 with
							| [] -> false
							| st :: [] when (st :> all_item) == (t :> all_item) -> false
							| _ -> true)
						then (
							error (fst directive) "new type-specifier was found in pragma."
						);
						if attrs <> no_attributes then (
							error (fst directive) "attributes are ignored in pragma instance."
						);
						let mapping_options = {mapping_options with mo_instances =
							StringMap.modify (fun rs ->
								let error =
									List.exists (fun i ->
										begin match t, i with
										| `function_type p1, `function_type p2 ->
											Typing.prototype_ABI_compatibility ~dest:p1 ~source:p2 = `just (* same prototype *)
										| _ ->
											error (fst directive) "this type could not be overloaded.";
											true
										end
									) rs
								in
								if error then rs else t :: rs
							) ~default:[] name mapping_options.mo_instances}
						in
						derived_types, info, alignment_stack, mapping_options
					| None ->
						derived_types, info, alignment_stack, mapping_options
					end
				| _ ->
					derived_types, info, alignment_stack, mapping_options
				end
			| `pack (_, _, arg, _) ->
				let push (n: Integer.t) alignment_stack = (
					begin match Integer.to_int n with
					| 1 ->
						`packed :: alignment_stack
					| n ->
						`aligned n :: alignment_stack
					end
				) in
				let pop alignment_stack = (
					begin match alignment_stack with
					| _ :: [] | [] ->
						error ps ("too many #pragma pack(pop).");
						alignment_stack
					| _ :: xr ->
						xr
					end
				) in
				begin match arg with
				| `some arg ->
					begin match snd arg with
					| `push (_, _, n) ->
						begin match n with
						| `some n ->
							let alignment_stack = push (snd n) alignment_stack in
							derived_types, info, alignment_stack, mapping_options
						| `error ->
							derived_types, info, alignment_stack, mapping_options
						end
					| `pop ->
						let alignment_stack = pop alignment_stack in
						derived_types, info, alignment_stack, mapping_options
					| `set n ->
						let alignment_stack = push n (List.tl alignment_stack) in
						derived_types, info, alignment_stack, mapping_options
					end
				| `none ->
					(* set default *)
					let alignment_stack = `default :: (List.tl alignment_stack) in
					derived_types, info, alignment_stack, mapping_options
				end
			| `language_mapping (_, lang, mapping) ->
				begin match lang, mapping with
				| `some lang, `some mapping ->
					let lang = String.uppercase (snd lang) in
					begin match snd mapping with
					| `type_mapping (_, typename, _, repr) ->
						begin match typename, repr with
						| `some typename, `some (_, `chars_literal repr) ->
							let derived_types, s2, t = handle_type_name error predefined_types derived_types namespace [] typename in
							if s2 <> [] then (
								error (fst typename) "new type-specifier was found in pragma."
							);
							let mapping_options = {mapping_options with mo_language_mappings =
								StringMap.modify (fun x ->
									{x with lm_type = (t, repr) :: x.lm_type}
								) ~default:no_language_mapping lang mapping_options.mo_language_mappings}
							in
							derived_types, info, alignment_stack, mapping_options
						| _ ->
							derived_types, info, alignment_stack, mapping_options
						end
					| `overload (_, sql, decl) ->
						begin match sql, decl with
						| `some sql, `some decl ->
							let derived_types, n2, s2, base_type = handle_specifier_qualifier_list error predefined_types derived_types namespace [] (List.hd alignment_stack) sql in
							let derived_types, n2, s2, decl = handle_declarator error predefined_types derived_types n2 s2 base_type no_attributes decl in
							begin match decl with
							| Some (_, name, t, attrs) ->
								if n2 != namespace || (
									match s2 with
									| s :: [] when (s :> all_item) == (t :> all_item) -> false
									| _ -> true)
								then (
									error (fst mapping) "new type-specifier was found in pragma."
								);
								if attrs <> no_attributes then (
									error (fst mapping) "attributes are ignored in pragma overload."
								);
								begin match t with
								| `function_type prototype ->
									begin try
										begin match StringMap.find name namespace.ns_namespace with
										| `named (_, _, `extern ((#function_type as original_t), _), _)
										| `named (_, _, `function_forward (_, original_t), _)
										| `named (_, _, `function_definition (_, original_t, _), _) as func ->
											let `function_type original_prototype = original_t in
											begin match Typing.prototype_ABI_compatibility ~dest:prototype ~source:original_prototype with
											| `compatible ->
												let mapping_options = {mapping_options with mo_language_mappings =
													StringMap.modify (fun x ->
														let overload =
															begin try
																let e = List.assq func x.lm_overload in
																let e = prototype :: e in
																(func, e) :: List.remove_assq func x.lm_overload
															with Not_found ->
																(func, [prototype]) :: x.lm_overload
															end
														in
														{x with lm_overload = overload}
													) ~default:no_language_mapping lang mapping_options.mo_language_mappings}
												in
												derived_types, info, alignment_stack, mapping_options
											| `error ->
												error (fst mapping) "this overload has no ABI compatibility with original function.";
												derived_types, info, alignment_stack, mapping_options
											| `just ->
												error (fst mapping) "this overload has same prototype of original function.";
												derived_types, info, alignment_stack, mapping_options
											end
										| _ ->
											error (fst mapping) ("\"" ^ name ^ "\" is not function.");
											derived_types, info, alignment_stack, mapping_options
										end
									with Not_found ->
										error (fst mapping) ("overloading \"" ^ name ^ "\" is undeclared.");
										derived_types, info, alignment_stack, mapping_options
									end
								| _ ->
									error (fst mapping) ("overloading \"" ^ name ^ "\" is not function.");
									derived_types, info, alignment_stack, mapping_options
								end
							| None ->
								derived_types, info, alignment_stack, mapping_options
							end
						| _ ->
							derived_types, info, alignment_stack, mapping_options
						end
					| `includes ((_, `chars_literal file1), _, file2) ->
						begin match file2 with
						| `some (_, `chars_literal file2) ->
							let mapping_options = {mapping_options with mo_language_mappings =
								StringMap.modify (fun x ->
									{x with lm_include = (file1, file2) :: x.lm_include}
								) ~default:no_language_mapping lang mapping_options.mo_language_mappings}
							in
							derived_types, info, alignment_stack, mapping_options
						| `error ->
							derived_types, info, alignment_stack, mapping_options
						end
					| `monolithic_include ((_, `chars_literal file1), _, file2) ->
						begin match file2 with
						| `some (_, `chars_literal file2) ->
							let mapping_options = {mapping_options with mo_language_mappings =
								StringMap.modify (fun x ->
									{x with lm_monolithic_include = (file1, file2) :: x.lm_monolithic_include}
								) ~default:no_language_mapping lang mapping_options.mo_language_mappings}
							in
							derived_types, info, alignment_stack, mapping_options
						| `error ->
							derived_types, info, alignment_stack, mapping_options
						end
					end
				| _ ->
					derived_types, info, alignment_stack, mapping_options
				end
			end
		| `error ->
			derived_types, info, alignment_stack, mapping_options
		end
	) and handle_attribute
		(error: ranged_position -> string -> unit)
		(attributes: attributes)
		(x: Syntax.attribute p)
		: attributes =
	(
		let _, (_, _, _, list, _, _) = x in
		begin match list with
		| `some list ->
			Traversing.fold_ail (handle_attribute_item error) attributes list
		| `error ->
			attributes
		end
	) and handle_attribute_item
		(error: ranged_position -> string -> unit)
		(attributes: attributes)
		(x: Syntax.attribute_item p)
		: attributes =
	(
		let int_of_expr (expr: Syntax.assignment_expression p) = (
			begin match snd expr with
			| `int_literal (_, value) ->
				Some (Integer.to_int value)
			| _ ->
				error (fst expr) "complex expression in __attributes__(...) is not supported.";
				None
			end
		) in
		let int_list_of_expr_list (list: Syntax.argument_expression_list p) = (
			Traversing.fold_right_ael (fun ae rs ->
				begin match int_of_expr ae with
				| Some n ->
					n :: rs
				| None ->
					rs
				end)
				list
				[]
		) in
		begin match snd x with
		| `aligned (_, param) ->
			begin match param with
			| `some (_, (_, n, _)) ->
				begin match n with
				| `some n ->
					begin match int_of_expr n with
					| Some n ->
						{attributes with at_aligned = `aligned n}
					| None ->
						attributes
					end
				| `error ->
					attributes
				end
			| `none ->
				{attributes with at_aligned = `explicit_aligned}
			end
		| `alloc_size (_, _, list, _) ->
			begin match list with
			| `some list ->
				{attributes with at_alloc_size = int_list_of_expr_list list}
			| `error ->
				attributes
			end
		| `always_inline _ ->
			{attributes with at_inline = `always_inline}
		| `artificial ->
			{attributes with at_artificial = true}
		| `blocks (_, _, arg, _) ->
			begin match arg with
			| `some (_, `BYREF) ->
				{attributes with at_blocks = `byref}
			| `error ->
				attributes
			end
		| `cdecl _ ->
			{attributes with at_conventions = `cdecl}
		| `const _ ->
			{attributes with at_const = true}
		| `deprecated _ ->
			{attributes with at_deprecated = true}
		| `dllimport _ ->
			{attributes with at_dllimport = true}
		| `dllexport _ ->
			{attributes with at_dllexport = true}
		| `fastcall ->
			{attributes with at_conventions = `fastcall}
		| `format (_, _, f, _, m, _, n, _) ->
			begin match f, m, n with
			| `some (_, `ident funcname), `some m, `some n ->
				begin match int_of_expr m, int_of_expr n with
				| Some m, Some n ->
					{attributes with at_format = `like (funcname, m, n)}
				| _ ->
					attributes
				end
			| _ ->
				attributes
			end
		| `format_arg (_, _, n, _) ->
			begin match n with
			| `some n ->
				begin match int_of_expr n with
				| Some n ->
					{attributes with at_format = `arg n}
				| None ->
					attributes
				end
			| `error ->
				attributes
			end
		| `inline _ ->
			if attributes.at_inline = `always_inline then attributes else
			{attributes with at_inline = `inline}
		| `leaf ->
			{attributes with at_leaf = true}
		| `malloc ->
			{attributes with at_malloc = true}
		| `mode (_, _, mode, _) ->
			begin match mode with
			| `some mode ->
				{attributes with at_mode = Some (snd mode)}
			| `error ->
				attributes
			end
		| `noinline _ ->
			{attributes with at_inline = `noinline}
		| `nonnull (_, _, list, _) ->
			begin match list with
			| `some list ->
				{attributes with at_nonnull = int_list_of_expr_list list}
			| `error ->
				attributes
			end
		| `noreturn _ ->
			{attributes with at_noreturn = true}
		| `nothrow ->
			{attributes with at_nothrow = true}
		| `objc_gc (_, _, arg, _) ->
			begin match arg with
			| `some (_, `WEAK) ->
				{attributes with at_objc_gc = `weak}
			| `error ->
				attributes
			end
		| `optimize (_, _, arg, _) ->
			begin match arg with
			| `some (_, `chars_literal arg) ->
				{attributes with at_optimize = Some arg}
			| `error ->
				attributes
			end
		| `packed _ ->
			{attributes with at_aligned = `packed}
		| `pure ->
			{attributes with at_pure = true}
		| `regparm (_, _, arg, _) ->
			begin match arg with
			| `some expr ->
				begin match int_of_expr expr with
				| Some _ as r ->
					{attributes with at_regparm = r}
				| None ->
					attributes
				end
			| `error ->
				attributes
			end
		| `returns_twice ->
			{attributes with at_returns_twice = true}
		| `sentinel ->
			{attributes with at_sentinel = true}
		| `selectany ->
			{attributes with at_selectany = true}
		| `stdcall ->
			{attributes with at_conventions = `stdcall}
		| `thiscall ->
			{attributes with at_conventions = `thiscall}
		| `transparent_union ->
			{attributes with at_transparent_union = true}
		| `unavailable ->
			{attributes with at_unavailable = true}
		| `unused _ ->
			{attributes with at_used = `unused}
		| `used ->
			{attributes with at_used = `used}
		| `visibility _ ->
			attributes (* ignore __attribute__((visibility(...))) *)
		| `warn_unused_result ->
			{attributes with at_warn_unused_result = true}
		| `weak ->
			{attributes with at_weak_link = `weak}
		| `weak_import ->
			{attributes with at_weak_link = `weak_import}
		end
	) and handle_expression
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(request: [`lvalue | `rvalue])
		(x: Syntax.expression p)
		: derived_types * source_item list * expression option =
	(
		let handle_field
			(dereferencing: bool)
			(var: Syntax.expression p)
			(var_request: [`lvalue | `rvalue])
			(field: Syntax.identifier e)
			: derived_types * source_item list * expression option =
		(
			let derived_types, source, var = handle_expression error predefined_types derived_types namespace source var_request var in
			begin match var, field with
			| Some (`ref_object ((`named (_, _, `generic_value _, _)), _), _), _ ->
				error (fst x) "currently, macro argument could not be inferenced as struct or union.";
				derived_types, source, None
			| Some var, `some (_, `ident field_name) ->
				let t = snd var in
				(* dereferencing *)
				let resolved_t, var = (
					if dereferencing then (
						begin match dereference t with
						| Some target_t ->
							let target_t = resolve_typedef target_t in
							target_t, (`dereference var, target_t)
						| None ->
							error (fst x) "this expression is not a pointer type.";
							t, var
						end
					) else (
						(resolve_typedef t), var
					)
				) in
				(* remove const *)
				let resolved_t =
					begin match resolved_t with
					| `const t -> resolve_typedef (t :> all_type)
					| _ -> resolved_t
					end
				in
				(* element access *)
				begin match resolved_t with
				| `anonymous (_, `struct_type (_, items))
				| `anonymous (_, `union items)
				| `named (_, _, `struct_type (_, items), _)
				| `named (_, _, `union items, _) ->
					begin match find_field field_name items with
					| Some (_, field_t, _, _ as field) ->
						derived_types, source, Some (`element_access (var, field), field_t)
					| None ->
						error (fst x) ("field \"" ^ field_name ^ "\" does not exist.");
						derived_types, source, None
					end
				| _ ->
					error (fst x) (is_not_a_struct_or_union "this expression");
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		) in
		let handle_sizeof derived_types source t: derived_types * source_item list * expression option = (
			let size_t = find_size_t predefined_types in
			begin match t with
			| `named (_, formal_type, `generic_type, _) ->
				derived_types, source, Some (`sizeof_formal_type formal_type, size_t)
			| _ ->
				begin match Typing.sizeof t predefined_types with
				| Some sizeof_value ->
					derived_types, source, Some (`int_literal (`unsigned_int, Integer.of_int sizeof_value), size_t)
				| None ->
					error (fst x) "sizeof(opaque) is invalid.";
					derived_types, source, None
				end
			end
		) in
		let handle_unary (f: derived_types -> expression -> derived_types * expression option) request (right: Syntax.expression e): derived_types * source_item list * expression option = (
			begin match right with
			| `some right ->
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source request right in
				begin match right with
				| Some right ->
					let derived_types, result = f derived_types right in
					derived_types, source, result
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		) in
		let handle_unary_folding (f: derived_types -> expression -> derived_types * expression option) (int_f: Integer.t -> Integer.t) (right: Syntax.expression e): derived_types * source_item list * expression option = (
			handle_unary (fun derived_types right ->
				begin match integer_of_expression right with
				| Some (prec, right) ->
					let result = int_f right in
					derived_types, Some (`int_literal (prec, result), find_predefined_type prec predefined_types)
				| None ->
					f derived_types right
				end
			) `rvalue right
		) in
		let handle_unary_bool (f: expression -> expression_var option) (int_f: Integer.t -> bool) (right: Syntax.expression e): derived_types * source_item list * expression option = (
			handle_unary (fun derived_types right ->
				let bool_type = find_predefined_type `bool predefined_types in
				begin match integer_of_expression right with
				| Some (_, right) ->
					let result = if int_f right then Integer.one else Integer.zero in
					derived_types, Some (`int_literal (`unsigned_char, result), bool_type)
				| None ->
					begin match f right with
					| Some x -> derived_types, Some (x, bool_type)
					| None -> derived_types, None
					end
				end
			) `rvalue right
		) in
		let handle_binary (f: derived_types -> expression -> expression -> derived_types * expression option) (left: Syntax.expression p) (right: Syntax.expression e): derived_types * source_item list * expression option = (
			begin match right with
			| `some right ->
				let derived_types, source, left = handle_expression error predefined_types derived_types namespace source `rvalue left in
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source `rvalue right in
				begin match left, right with
				| Some left, Some right ->
					let derived_types, result = f derived_types left right in
					derived_types, source, result
				| _ ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		) in
		let handle_binary_folding op (f: expression -> expression -> expression_var option) (int_f: Integer.t -> Integer.t -> Integer.t) (left: Syntax.expression p) (right: Syntax.expression e): derived_types * source_item list * expression option = (
			handle_binary (fun derived_types left right ->
				begin match (integer_of_expression left), (integer_of_expression right) with
				| Some (prec1, left), Some (prec2, right) ->
					let prec =
						begin match op with
						| `bit | `conditional when prec1 = prec2 ->
							prec1
						| _ ->
							Expressing.int_prec prec1 prec2
						end
					in
					let result = int_f left right in
					derived_types, Some (`int_literal (prec, result), find_predefined_type prec predefined_types)
				| _ ->
					begin match Expressing.result_type_of op (snd left) (snd right) predefined_types derived_types with
					| Some result_type, derived_types ->
						begin match f (Expressing.implicit_conv result_type left) (Expressing.implicit_conv result_type right) with
						| Some x ->
							derived_types, Some (x, result_type)
						| None ->
							derived_types, None
						end
					| None, derived_types ->
						error (fst x) "type mismatch for binary-operator.";
						derived_types, None
					end
				end
			) left right
		) in
		let handle_compare (f: expression -> expression -> expression_var option) (int_f: Integer.t -> Integer.t -> bool) (left: Syntax.expression p) (right: Syntax.expression e): derived_types * source_item list * expression option = (
			handle_binary (fun derived_types left right ->
				let bool_type = find_predefined_type `bool predefined_types in
				begin match (integer_of_expression left), (integer_of_expression right) with
				| Some (_, left), Some (_, right) ->
					let result = if int_f left right then Integer.one else Integer.zero in
					derived_types, Some (`int_literal (`unsigned_char, result), bool_type)
				| _ ->
					begin match f left right with
					| Some x -> derived_types, Some (x, bool_type)
					| None -> derived_types, None
					end
				end
			) left right
		) in
		let handle_shift (f: expression -> expression -> expression_var option) (int_f: Integer.t -> int -> Integer.t) (left: Syntax.expression p) (right: Syntax.expression e): derived_types * source_item list * expression option = (
			handle_binary (fun derived_types left right ->
				begin match (integer_of_expression left), (integer_of_expression right) with
				| Some (prec, left), Some (_, right) ->
					let result = int_f left (Integer.to_int right) in
					derived_types, Some (`int_literal (prec, result), find_predefined_type prec predefined_types)
				| _ ->
					begin match f left right with
					| Some x -> derived_types, Some (x, snd left)
					| None -> derived_types, None
					end
				end
			) left right
		) in
		begin match snd x with
		| `int_literal (prec, _) as v ->
			derived_types, source, Some (v, find_predefined_type prec predefined_types)
		| `float_literal (prec, _) as v ->
			derived_types, source, Some (v, find_predefined_type prec predefined_types)
		| `imaginary_literal (prec, _) as v ->
			derived_types, source, Some (v, find_predefined_type (`imaginary prec) predefined_types)
		| `char_literal _ as v ->
			derived_types, source, Some (v, find_predefined_type `char predefined_types)
		| `chars_literal s as v ->
			let base_type = find_predefined_type `char predefined_types in
			let length = Some (Integer.of_int (String.length s + 1)) in
			let t, derived_types = Typing.find_array_type length base_type derived_types in
			derived_types, source, Some (v, t)
		| `wchar_literal _ as v ->
			derived_types, source, Some (v, find_wchar_t predefined_types)
		| `wchars_literal s as v ->
			let base_type = find_wchar_t predefined_types in
			let length = Some (Integer.of_int (WideString.length s + 1)) in
			let t, derived_types = Typing.find_array_type length base_type derived_types in
			derived_types, source, Some (v, t)
		| `objc_string_literal _ ->
			error (fst x) "unimplemented!";
			assert false
		| `ident name ->
			begin try
				begin match StringMap.find name namespace.ns_namespace with
				| `named (_, _, `enum_element _, _) as item ->
					let t = Typing.find_enum_by_element item predefined_types namespace in
					derived_types, source, Some (`enumerator item, t)
				| `named (_, _, `function_forward (_, t), _) as item ->
					derived_types, source, Some (`ref_function item, (t :> all_type))
				| `named (_, _, `function_definition (_, t, _), _) as item ->
					derived_types, source, Some (`ref_function item, (t :> all_type))
				| `named (_, _, `extern ((#function_type as t), _), _) as item ->
					derived_types, source, Some (`ref_function item, t)
				| `named (_, _, `extern (t, _), _) as item ->
					derived_types, source, Some (`ref_object (item, request), t)
				| `named (_, _, `variable (t, _), _) as item ->
					derived_types, source, Some (`ref_object (item, request), t)
				| `named (_, _, `generic_value t, _) as item ->
					derived_types, source, Some (`ref_object (item, request), t)
				| `named (_, _, #named_type_var, _) ->
					error (fst x) ("a type name (" ^ name ^ ")is used as variable.");
					derived_types, source, None
				| _ -> (* `defined_* *)
					error (fst x) "unimplemented!";
					assert false
				end
			with Not_found ->
				if name = "__func__" || name = "__PRETTY_FUNCTION__" then (
					let base_type = find_predefined_type `char predefined_types in
					let t, derived_types = Typing.find_array_type None base_type derived_types in
					derived_types, source, Some (`__func__, t)
				) else (
					error (fst x) (is_undeclared name);
					derived_types, source, None
				)
			end
		| `__FILE__ _ ->
			let base_type = find_predefined_type `char predefined_types in
			let t, derived_types = Typing.find_array_type None base_type derived_types in
			derived_types, source, Some (`__FILE__, t)
		| `__LINE__ _ ->
			derived_types, source, Some (`__LINE__, find_predefined_type `signed_int predefined_types)
		| `paren (_, e, _) ->
			begin match e with
			| `some e ->
				handle_expression error predefined_types derived_types namespace source request e
			| `error ->
				derived_types, source, None
			end
		| `statement (_, compound, _) ->
			let derived_types, source, compound = handle_compound_statement error predefined_types derived_types namespace source `default compound in
			let rec loop xs = (
				begin match xs with
				| stmt :: [] ->
					begin match stmt with
					| `expression (_, t) ->
						t
					| `local (_, stmts) ->
						loop stmts
					| _ ->
						error (fst x) "last of statement expression is not an expression.";
						find_predefined_type `void predefined_types (* should it be error? *)
					end
				| _ :: xr ->
					loop xr
				| [] ->
					error (fst x) "statement expression is empty.";
					find_predefined_type `void predefined_types
				end
			) in
			let t = loop compound in
			derived_types, source, Some (`statement compound, t)
		| `array_access (left, _, right, _) ->
			begin match right with
			| `some right ->
				let derived_types, source, left = handle_expression error predefined_types derived_types namespace source `rvalue left in
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source `rvalue right in
				begin match left, right with
				| Some left, Some right ->
					(* treat a[i] as *(a + i) *)
					begin match Expressing.result_type_of `add (snd left) (snd right) predefined_types derived_types with
					| Some added_type, derived_types ->
						begin match dereference added_type with
						| Some t ->
							derived_types, source, Some (`dereference (`add (left, right), added_type), t)
						| None ->
							error (fst x) "type mismatch to dereference of array-access.";
							derived_types, source, None
						end
					| None, derived_types ->
						error (fst x) "type mismatch to add of array-access.";
						derived_types, source, None
					end
				| _ ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `function_call (f, _, args, _) ->
			let derived_types, source, f = handle_expression error predefined_types derived_types namespace source `rvalue f in
			begin match f with
			| Some f ->
				let derived_types, source, args =
					Traversing.opt Traversing.fold_ael (fun (derived_types, source, rs) arg ->
						begin match rs with
						| Some rs ->
							let derived_types, source, arg = handle_expression error predefined_types derived_types namespace source `rvalue arg in
							begin match arg with
							| Some arg ->
								derived_types, source, Some (arg :: rs)
							| None ->
								derived_types, source, None
							end
						| None ->
							derived_types, source, None
						end
					) (derived_types, source, Some []) args
				in
				begin match args with
				| Some args ->
					let args = List.rev args in
					(* type check *)
					let function_type = resolve_typedef (snd f) in
					begin match function_type with
					| `function_type (_, args_of_prototype, _, result_type) ->
						if List.length args <> List.length args_of_prototype then (
							error (fst x) "a number of arguments was mismatch.";
							derived_types, source, None
						) else (
							let args =
								List.fold_left2 (fun rs farg rarg ->
									let `named (_, _, `variable (farg_t, _), _) = farg in
									(Expressing.implicit_conv farg_t rarg) :: rs
								) [] args_of_prototype args
							in
							let args = List.rev args in
							derived_types, source, Some (`function_call (f, args), result_type)
						)
					| _ ->
						error (fst x) "this is not function.";
						derived_types, source, None
					end
				| None ->
					derived_types, source, None
				end
			| None ->
				derived_types, source, None
			end
		| `__builtin_constant_p _ ->
			(* currently, always false *)
			let value = Some (
				`int_literal (`unsigned_char, Integer.zero),
				find_predefined_type `bool predefined_types)
			in
			derived_types, source, value
		| `__builtin_expect (_, _, expr, _, _, _) ->
			(* __builtin_expect(expr, c) -> expr *)
			begin match expr with
			| `some expr ->
				handle_expression error predefined_types derived_types namespace source request expr
			| `error ->
				derived_types, source, None
			end
		| `__builtin_object_size _ ->
			(* currently, always max value of size_t *)
			let size_t = find_size_t predefined_types in
			let sizeof_size_t = Typing.sizeof size_t predefined_types in
			begin match sizeof_size_t with
			| Some sizeof_size_t ->
				let max_sizeof = Integer.sub (Integer.shift_left Integer.one (sizeof_size_t * 8)) Integer.one in
				begin match resolve_typedef size_t with
				| #int_prec as t ->
					let value = Some (`int_literal (t, max_sizeof), size_t) in
					derived_types, source, value
				| _ ->
					assert false (* does not come here *)
				end
			| None ->
				assert false (* does not come here *)
			end
		| `__builtin_va_arg (_, _, expr, _, typename, _) ->
			begin match expr, typename with
			| `some expr, `some typename ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `lvalue expr in
				let derived_types, source, t = handle_type_name error predefined_types derived_types namespace source typename in
				begin match expr with
				| Some expr ->
					derived_types, source, Some (`va_arg expr, t)
				| None ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `__builtin_compare ((_, func), _, left, _, right, _) ->
			begin match left with
			| `some left ->
				begin match func with
				| `__builtin_isgreater ->
					let int_gt left right = (Integer.compare left right > 0) in
					handle_compare (fun left right -> Some (`gt (left, right))) int_gt left right
				| `__builtin_isgreaterequal ->
					let int_ge left right = (Integer.compare left right >= 0) in
					handle_compare (fun left right -> Some (`ge (left, right))) int_ge left right
				| `__builtin_isless ->
					let int_lt left right = (Integer.compare left right < 0) in
					handle_compare (fun left right -> Some (`lt (left, right))) int_lt left right
				| `__builtin_islessequal ->
					let int_le left right = (Integer.compare left right <= 0) in
					handle_compare (fun left right -> Some (`le (left, right))) int_le left right
				| `__builtin_islessgreater ->
					let int_ne left right = (Integer.compare left right <> 0) in
					handle_compare (fun left right -> Some (`ne (left, right))) int_ne left right
				| `__builtin_isunordered ->
					let int_uo _ _ = false in (* integer is always orderd *)
					handle_compare (fun left right -> Some (`uo (left, right))) int_uo left right
				end
			| `error ->
				derived_types, source, None
			end
		| `element_access (var, _, field) ->
			handle_field false var request field
		| `dereferencing_element_access (var, _, field) ->
			handle_field true var `rvalue field
		| `post_increment (left, _) ->
			handle_unary (fun derived_types left ->
				derived_types, Some (`post_increment left, snd left)
			) `lvalue (`some left)
		| `post_decrement (left, _) ->
			handle_unary (fun derived_types left ->
				derived_types, Some (`post_decrement left, snd left)
			) `lvalue (`some left)
		| `compound (_, typename, _, _, list, _) ->
			let derived_types, source, t = handle_type_name error predefined_types derived_types namespace source typename in
			begin match list with
			| `some list ->
				handle_initializer_list error predefined_types derived_types namespace source t list
			| `error ->
				derived_types, source, None
			end
		| `increment (_, right) ->
			handle_unary (fun derived_types right ->
				derived_types, Some (`increment right, snd right)
			) `lvalue right
		| `decrement (_, right) ->
			handle_unary (fun derived_types right ->
				derived_types, Some (`decrement right, snd right)
			) `lvalue right
		| `unary ((_, `ampersand), right) ->
			handle_unary (fun derived_types right ->
				let t, derived_types = Typing.find_pointer_type (snd right) derived_types in
				derived_types, Some (`address right, t)
			) `lvalue right
		| `unary ((_, `asterisk), right) ->
			handle_unary (fun derived_types right ->
				begin match dereference (snd right) with
				| Some t ->
					derived_types, Some (`dereference right, t)
				| None ->
					error (fst x) "this expression is not a pointer.";
					derived_types, None
				end
			) `rvalue right
		| `unary ((_, `plus), right) ->
			let int_pos x = x in
			handle_unary_folding (fun derived_types right ->
				derived_types, Some right
			) int_pos right
		| `unary ((_, `minus), right) ->
			handle_unary_folding (fun derived_types right ->
				derived_types, Some (`neg right, snd right)
			) Integer.neg right
		| `unary ((_, `tilde), right) ->
			handle_unary_folding (fun derived_types right ->
				derived_types, Some (`bit_not right, snd right)
			) Integer.lognot right
		| `unary ((_, `exclamation), right) ->
			begin match right with
			| `some (_, `unary ((_, `exclamation), (`some right))) -> (* folding !!expr to (bool)expr *)
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source `rvalue right in
				begin match right with
				| Some right ->
					derived_types, source, Some (Expressing.implicit_conv `bool right)
				| None ->
					derived_types, source, None
				end
			| _ ->
				let int_not x = (Integer.compare x Integer.zero = 0) in
				handle_unary_bool (fun right -> Some (`not (Expressing.implicit_conv `bool right))) int_not right
			end
		| `sizeof_expr (_, expr) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					handle_sizeof derived_types source (snd expr)
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `sizeof_type (_, _, typename, _) ->
			let derived_types, source, t = handle_type_name error predefined_types derived_types namespace source typename in
			handle_sizeof derived_types source t
		| `extension (_, right) ->
			handle_unary (fun derived_types right ->
				derived_types, Some right
			) request right
		| `real (_, right) ->
			handle_unary (fun derived_types right ->
				begin match Expressing.real_type_of (snd right) predefined_types with
				| Some t ->
					derived_types, Some (`real right, t)
				| None ->
					error (fst x) "type mismatch for complex.";
					derived_types, None
				end
			) `rvalue right
		| `imag (_, right) ->
			handle_unary (fun derived_types right ->
				(* __imag__ right return imaginary part of right with base floating type (not _Imaginary type) *)
				begin match Expressing.real_type_of (snd right) predefined_types with
				| Some t ->
					derived_types, Some (`imag right, t)
				| None ->
					error (fst x) "type mismatch for complex.";
					derived_types, None
				end
			) `rvalue right
		| `cast (_, typename, _, expr) ->
			let derived_types, source, t = handle_type_name error predefined_types derived_types namespace source typename in
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					let resolved_t = resolve_typedef t in
					begin match resolved_t with
					| #int_prec as int_t ->
						begin match expr with
						| `cast (`cast (_, orig_t as expr), _), _
						| `cast (`explicit_conv (_, orig_t as expr), _), _
						| `cast (`implicit_conv (_, orig_t as expr), _), _
						| `cast ((`int_literal _), orig_t as expr), _ when resolve_typedef orig_t == int_t ->
							(* folding (int)(T)(int)expr to (int)expr *)
							begin match integer_of_expression expr with
							| Some (_, expr) ->
								derived_types, source, Some (Expressing.int_conv predefined_types int_t expr)
							| _ ->
								derived_types, source, Some (`explicit_conv expr, t)
							end
						| _ ->
							begin match integer_of_expression expr with
							| Some (_, expr) ->
								derived_types, source, Some (Expressing.int_conv predefined_types int_t expr)
							| _ ->
								derived_types, source, Some (`cast expr, t)
							end
						end
					| #predefined_numeric_type ->
						begin match resolve_typedef (snd expr) with
						| #predefined_numeric_type ->
							derived_types, source, Some (`explicit_conv expr, t)
						| _ ->
							derived_types, source, Some (`cast expr, t)
						end
					| `pointer _ ->
						let resolved_expr_t = resolve_typedef (snd expr) in
						begin match resolved_expr_t with
						| #signed_int_prec ->
							let sizeof_expr_t = Typing.sizeof resolved_expr_t predefined_types in
							let ptrdiff_t = find_ptrdiff_t predefined_types in
							let sizeof_ptrdiff_t = Typing.sizeof ptrdiff_t predefined_types in
							if sizeof_expr_t <> sizeof_ptrdiff_t then (
								derived_types, source, Some (`cast (`implicit_conv expr, resolve_typedef ptrdiff_t), t)
							) else (
								derived_types, source, Some (`cast expr, t)
							)
						| #unsigned_int_prec ->
							let sizeof_expr_t = Typing.sizeof resolved_expr_t predefined_types in
							let size_t = find_size_t predefined_types in
							let sizeof_size_t = Typing.sizeof size_t predefined_types in
							if sizeof_expr_t <> sizeof_size_t then (
								derived_types, source, Some (`cast (`implicit_conv expr, resolve_typedef size_t), t)
							) else (
								derived_types, source, Some (`cast expr, t)
							)
						| _ ->
							derived_types, source, Some (`cast expr, t)
						end
					| _ ->
						derived_types, source, Some (`cast expr, t)
					end
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `mul (left, _, right) ->
			handle_binary_folding `multiplicative (fun left right -> Some (`mul (left, right))) Integer.mul left right
		| `div (left, _, right) ->
			handle_binary_folding `multiplicative (fun left right -> Some (`div (left, right))) Integer.div left right
		| `rem (left, _, right) ->
			handle_binary_folding `multiplicative (fun left right -> Some (`rem (left, right))) Integer.rem left right
		| `add (left, _, right) ->
			handle_binary_folding `add (fun left right -> Some (`add (left, right))) Integer.add left right
		| `sub (left, _, right) ->
			handle_binary_folding `sub (fun left right -> Some (`sub (left, right))) Integer.sub left right
		| `l_shift (left, _, right) ->
			handle_shift (fun left right -> Some (`l_shift (left, right))) Integer.shift_left left right
		| `r_shift (left, _, right) ->
			handle_shift (fun left right -> Some (`r_shift (left, right))) Integer.shift_right left right
		| `lt (left, _, right) ->
			let int_lt left right = (Integer.compare left right < 0) in
			handle_compare (fun left right -> Some (`lt (left, right))) int_lt left right
		| `gt (left, _, right) ->
			let int_gt left right = (Integer.compare left right > 0) in
			handle_compare (fun left right -> Some (`gt (left, right))) int_gt left right
		| `le (left, _, right) ->
			let int_le left right = (Integer.compare left right <= 0) in
			handle_compare (fun left right -> Some (`le (left, right))) int_le left right
		| `ge (left, _, right) ->
			let int_ge left right = (Integer.compare left right >= 0) in
			handle_compare (fun left right -> Some (`ge (left, right))) int_ge left right
		| `eq (left, _, right) ->
			let int_eq left right = (Integer.compare left right = 0) in
			handle_compare (fun left right -> Some (`eq (left, right))) int_eq left right
		| `ne (left, _, right) ->
			let int_ne left right = (Integer.compare left right <> 0) in
			handle_compare (fun left right -> Some (`ne (left, right))) int_ne left right
		| `bit_and (left, _, right) ->
			handle_binary_folding `bit (fun left right -> Some (`bit_and (left, right))) Integer.logand left right
		| `bit_xor (left, _, right) ->
			handle_binary_folding `bit (fun left right -> Some (`bit_xor (left, right))) Integer.logxor left right
		| `bit_or (left, _, right) ->
			handle_binary_folding `bit (fun left right -> Some (`bit_or (left, right))) Integer.logor left right
		| `and_then (left, _, right) ->
			let int_and_then left right = (Integer.compare left Integer.zero <> 0 && Integer.compare right Integer.zero <> 0) in
			handle_compare (fun left right -> Some (`and_then (left, right))) int_and_then left right
		| `or_else (left, _, right) ->
			let int_or_else left right = (Integer.compare left Integer.zero <> 0 || Integer.compare right Integer.zero <> 0) in
			handle_compare (fun left right -> Some (`or_else (left, right))) int_or_else left right
		| `cond (cond, _, true_case, _, false_case) ->
			begin match true_case, false_case with
			| `some true_case, `some false_case ->
				let derived_types, source, cond = handle_expression error predefined_types derived_types namespace source `rvalue cond in
				let derived_types, source, true_case = handle_expression error predefined_types derived_types namespace source `rvalue true_case in
				let derived_types, source, false_case = handle_expression error predefined_types derived_types namespace source `rvalue false_case in
				begin match cond, true_case, false_case with
				| Some cond, Some true_case, Some false_case ->
					begin match (integer_of_expression cond), (integer_of_expression true_case), (integer_of_expression false_case) with
					| Some (_, cond), Some (prec1, true_case), Some (prec2, false_case) ->
						let prec = Expressing.int_prec prec1 prec2 in
						let result = if Integer.compare cond Integer.zero <> 0 then true_case else false_case in
						derived_types, source, Some (`int_literal (prec, result), find_predefined_type prec predefined_types)
					| _ ->
						begin match Expressing.result_type_of `conditional (snd true_case) (snd false_case) predefined_types derived_types with
						| Some t, derived_types ->
							let cond = Expressing.implicit_conv `bool cond in
							let true_case = Expressing.implicit_conv t true_case in
							let false_case = Expressing.implicit_conv t false_case in
							derived_types, source, Some (`cond (cond, true_case, false_case), t)
						| None, derived_types ->
							error (fst x) "type mismatch for cases of conditional-operator";
							derived_types, source, None
						end
					end
				| _ ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `assign (left, op, right) ->
			begin match right with
			| `some right ->
				let derived_types, source, left = handle_expression error predefined_types derived_types namespace source `lvalue left in
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source `rvalue right in
				begin match left, right with
				| Some left, Some right ->
					let left_t = snd left in
					let right = Expressing.implicit_conv left_t right in
					derived_types, source, Some (`assign (left, snd op, right), left_t)
				| _ ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `comma (left, _, right) ->
			handle_binary (fun derived_types left right ->
				begin match (integer_of_expression left), (integer_of_expression right) with
				| Some _, Some (prec, right) ->
					derived_types, Some (`int_literal (prec, right), find_predefined_type prec predefined_types)
				| _ ->
					derived_types, Some (`comma (left, right), snd right)
				end
			) left right
		end
	) and handle_declaration
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.declaration p)
		: derived_types * namespace * source_item list * named_item list =
	(
		let _, (spec, idecls, _) = x in
		let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
		let derived_types, namespace, source, result =
			Traversing.opt Traversing.fold_idrl (fun (derived_types, namespace, source, rs as result) (idecl: Syntax.init_declarator p) ->
				let derived_types, namespace, source, item = handle_init_declarator error predefined_types derived_types namespace source storage_class base_type attributes `none idecl in
				begin match item with
				| Some item -> derived_types, namespace, source, (item :: rs)
				| None -> result
				end
			) (derived_types, namespace, source, []) idecls
		in
		derived_types, namespace, source, List.rev result
	) and handle_declaration_specifiers
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.declaration_specifiers p)
		: derived_types * namespace * source_item list * ([storage_class | `none] * all_type * attributes) =
	(
		let rec extract (derived_types, namespace, source, attributes, storage_class, type_specs, qualifiers) spec = (
			let result, next =
				begin match snd spec with
				| `storage_class_specifier (sc, next) ->
					(derived_types, namespace, source, attributes, (handle_storage_class error storage_class sc), type_specs, qualifiers), next
				| `type_specifier (ts, next) ->
					let derived_types, namespace, source, type_specs = handle_type_specifier error predefined_types derived_types namespace source attributes.at_aligned ts type_specs in
					(derived_types, namespace, source, attributes, storage_class, type_specs, qualifiers), next
				| `type_qualifier (q, next) ->
					let qualifiers = handle_type_qualifier error qualifiers q in
					(derived_types, namespace, source, attributes, storage_class, type_specs, qualifiers), next
				| `function_specifier (fs, next) ->
					(derived_types, namespace, source, (handle_function_specifier error attributes fs), storage_class, type_specs, qualifiers), next
				| `attributes (attr, next) ->
					(derived_types, namespace, source, (handle_attribute error attributes attr), storage_class, type_specs, qualifiers), next
				| `extension (_, next) ->
					(derived_types, namespace, source, attributes, storage_class, type_specs, qualifiers), next
				end
			in
			begin match next with
			| `some s ->
				extract result s
			| `none ->
				result
			end
		) in
		let attributes = Declaring.attributes_of_alignment alignment in
		let derived_types, namespace, source, attributes, storage_class, specs, qualifiers = extract (derived_types, namespace, source, attributes, `none, (no_type_specifier_set, None), no_type_qualifier_set) x in
		let type_by_spec = get_type_by_specifier_set error predefined_types (fst x) attributes.at_mode specs in
		let derived_types, type_by_qualifiers = get_type_by_qualifier_set error derived_types (fst x) type_by_spec qualifiers in
		derived_types, namespace, source, (storage_class, type_by_qualifiers, attributes)
	) and handle_init_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(storage_class: [storage_class | `none])
		(base_type: all_type)
		(attributes: attributes)
		(alias: [`alias of string | `none])
		(x: Syntax.init_declarator p)
		: derived_types * namespace * source_item list * named_item option =
	(
		let derived_types, namespace, source, analyzed_decl, init =
			begin match snd x with
			| `no_init decl ->
				let derived_types, namespace, source, analyzed_decl = handle_declarator error predefined_types derived_types namespace source base_type attributes (fst x, decl) in
				derived_types, namespace, source, analyzed_decl, None
			| `with_init (decl, _, init) ->
				let derived_types, namespace, source, analyzed_decl = handle_declarator error predefined_types derived_types namespace source base_type attributes decl in
				let required_type =
					begin match analyzed_decl with
					| Some (_, _, t, _) -> t
					| None -> base_type
					end
				in
				begin match init with
				| `some init ->
					let derived_types, source, expr = handle_initializer error predefined_types derived_types namespace source required_type init in
					derived_types, namespace, source, analyzed_decl, expr
				| `error ->
					derived_types, namespace, source, analyzed_decl, None
				end
			end
		in
		begin match analyzed_decl with
		| Some (ps, id, t, attr) ->
			(* fill length *)
			let derived_types, t =
				begin match t, init with
				| `array (None, element_t1), Some (_, `array ((Some _ as length), element_t2)) when element_t1 == element_t2 ->
					let array_type, derived_types = Typing.find_array_type length `char derived_types in
					derived_types, array_type
				| _ ->
					derived_types, t
				end
			in
			(* by storage class *)
			begin match storage_class with
			| `typedef ->
				if init <> None then (
					error ps "initializer was found with typedef."
				);
				let t = (* typedef of language_typedef *)
					begin match id with
					| "ptrdiff_t" ->
						let ptrdiff_t = find_ptrdiff_t predefined_types in
						let `named (_, _, `typedef orig_t, _) = ptrdiff_t in
						if orig_t != resolve_typedef t then (
							error ps "bad difinition of ptrdiff_t."
						);
						ptrdiff_t
					| "size_t" ->
						let size_t = find_size_t predefined_types in
						let `named (_, _, `typedef orig_t, _) = size_t in
						if orig_t != resolve_typedef t then (
							error ps "bad difinition of size_t."
						);
						size_t
					| "wchar_t" ->
						let wchar_t = find_wchar_t predefined_types in
						begin match wchar_t with
						| `named (_, _, `typedef orig_t, _)
						| (#predefined_type as orig_t) ->
							if orig_t != resolve_typedef t then (
								error ps "bad difinition of wchar_t."
							)
						end;
						wchar_t
					| _ ->
						t
					end
				in
				let result = `named (ps, id, `typedef t, attr) in
				let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
				let source = (result :> source_item) :: source in
				derived_types, namespace, source, Some result
			| `extern ->
				if init <> None then (
					error ps "initializer was found with extern."
				);
				begin match t with
				| `function_type _ as t ->
					let result = `named (ps, id, `extern (t, alias), attr) in
					begin match Declaring.is_function_conflicted result namespace with
					| `error ->
						error ps ("\"" ^ id ^ "\" was conflicted.");
						derived_types, namespace, source, None
					| `same ->
						derived_types, namespace, source, None
					| `precedence previous ->
						let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
						let source = (result :> source_item) :: list_remove (previous :> source_item) source in
						derived_types, namespace, source, Some result
					| `none ->
						let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
						let source = (result :> source_item) :: source in
						derived_types, namespace, source, Some result
					end
				| _ ->
					let result = `named (ps, id, `extern (t, alias), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some result
				end
			| `static ->
				begin match t with
				| `function_type _ as t ->
					if init <> None then (
						error ps "initializer was found with function."
					);
					let result = `named (ps, id, `function_forward (`static, t), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some result
				| _ ->
					error (fst x) "unimplemented!";
					assert false
				end
			| `auto ->
				error (fst x) "unimplemented!";
				assert false
			| `register ->
				begin match t with
				| `function_type _ ->
					error (fst x) "\"register\" could not be applied to function type.";
					derived_types, namespace, source, None
				| _ ->
					let result = `named (ps, id, `variable (t, init), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some result
				end
			| `none ->
				begin match t with
				| `function_type _ as t ->
					if init <> None then (
						error ps "initializer was found with function."
					);
					let result = `named (ps, id, `extern (t, alias), attr) in
					begin match Declaring.is_function_conflicted result namespace with
					| `error ->
						error ps ("\"" ^ id ^ "\" was conflicted.");
						derived_types, namespace, source, None
					| `same ->
						derived_types, namespace, source, None
					| `precedence previous ->
						let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
						let source = (result :> source_item) :: list_remove (previous :> source_item) source in
						derived_types, namespace, source, Some result
					| `none ->
						let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
						let source = (result :> source_item) :: source in
						derived_types, namespace, source, Some result
					end
				| _ ->
					let result = `named (ps, id, `variable (t, init), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some result
				end
			end
		| None ->
			derived_types, namespace, source, None
		end
	) and handle_storage_class
		(error: ranged_position -> string -> unit)
		(storage_class: [storage_class | `none])
		(x: Syntax.storage_class_specifier p)
		: [storage_class | `none] =
	(
		if storage_class = `none then (
			begin match snd x with
			| `TYPEDEF -> `typedef
			| `EXTERN -> `extern
			| `STATIC -> `static
			| `AUTO -> `auto
			| `REGISTER -> `register
			end
		) else (
			error (fst x) "storage-class-specifier was duplicated.";
			storage_class
		)
	) and handle_type_specifier
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.type_specifier p)
		(set, named: type_specifier_set * all_type option)
		: derived_types * namespace * source_item list * (type_specifier_set * all_type option) =
	(
		begin match snd x with
		| `VOID ->
			derived_types, namespace, source, ({set with ts_void = set.ts_void + 1}, named)
		| `CHAR ->
			derived_types, namespace, source, ({set with ts_char = set.ts_char + 1}, named)
		| `SHORT ->
			derived_types, namespace, source, ({set with ts_short = set.ts_short + 1}, named)
		| `INT ->
			derived_types, namespace, source, ({set with ts_int = set.ts_int + 1}, named)
		| `LONG ->
			derived_types, namespace, source, ({set with ts_long = set.ts_long + 1}, named)
		| `FLOAT ->
			derived_types, namespace, source, ({set with ts_float = set.ts_float + 1}, named)
		| `DOUBLE ->
			derived_types, namespace, source, ({set with ts_double = set.ts_double + 1}, named)
		| `SIGNED ->
			derived_types, namespace, source, ({set with ts_signed = set.ts_signed + 1}, named)
		| `UNSIGNED ->
			derived_types, namespace, source, ({set with ts_unsigned = set.ts_unsigned + 1}, named)
		| `_BOOL ->
			derived_types, namespace, source, ({set with ts_bool = set.ts_bool + 1}, named)
		| `_IMAGINARY ->
			derived_types, namespace, source, ({set with ts_imaginary = set.ts_imaginary + 1}, named)
		| `_COMPLEX ->
			derived_types, namespace, source, ({set with ts_complex = set.ts_complex + 1}, named)
		| `struct_or_union_specifier spec ->
			let derived_types, namespace, source, named =
				begin match named with
				| Some _ ->
					error (fst x) ("type-specifier was duplicated.");
					derived_types, namespace, source, named
				| None ->
					handle_struct_or_union_specifier error predefined_types derived_types namespace source alignment (fst x, spec)
				end
			in
			derived_types, namespace, source, (set, named)
		| `enum_specifier spec ->
			let derived_types, namespace, source, named =
				begin match named with
				| Some _ ->
					error (fst x) ("type-specifier was duplicated.");
					derived_types, namespace, source, named
				| None ->
					handle_enum_specifier error predefined_types derived_types namespace source (fst x, spec)
				end
			in
			derived_types, namespace, source, (set, named)
		| `typedef_name (`ident name) ->
			let named =
				begin match named with
				| Some _ ->
					error (fst x) ("type-specifier was duplicated.");
					named
				| None ->
					begin try
						begin match StringMap.find name namespace.ns_namespace with
						| (`named (_, _, #named_type_var, _)) as t ->
							Some t
						| _ ->
							error (fst x) (name ^ " was not type.");
							named
						end
					with Not_found ->
						error (fst x) (is_undeclared name);
						named
					end
				end
			in
			derived_types, namespace, source, (set, named)
		| `__int64 ->
			derived_types, namespace, source, ({set with ts_int64 = set.ts_int64 + 1}, named)
		| `__builtin_va_list ->
			derived_types, namespace, source, ({set with ts_builtin_va_list = set.ts_builtin_va_list + 1}, named)
		end
	) and handle_struct_or_union_specifier
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.struct_or_union_specifier p)
		: derived_types * namespace * source_item list * all_type option =
	(
		begin match snd x with
		| `with_body (sou, attrs1, id, _, decls, _, attrs2) ->
			begin match decls with
			| `some decls ->
				(* ready opaque type for recursive *)
				let namespace, source =
					begin match id with
					| `some id ->
						begin match snd sou with
						| `STRUCT ->
							let _, namespace, source = Typing.find_struct id namespace source in
							namespace, source
						| `UNION ->
							let _, namespace, source = Typing.find_union id namespace source in
							namespace, source
						end
					| `none ->
						namespace, source
					end
				in
				(* attributes *)
				let default_attributes = Declaring.attributes_of_alignment alignment in
				let attributes = default_attributes in
				let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs1 in
				let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs2 in
				let alignment = attributes.at_aligned in
				(* items *)
				let derived_types, namespace, source, items =
					Traversing.fold_sdnl (fun (derived_types, namespace, sources, rs) decl ->
						let derived_types, namespace, sources, items = handle_struct_declaration error predefined_types derived_types namespace sources alignment decl in
						derived_types, namespace, sources, (List.rev_append items rs)
					) (derived_types, namespace, source, []) decls
				in
				(* if last member is unsized array, replace to sized [0] *)
				let derived_types, items =
					begin match items with
					| (m_name, `array (None, m_element_t), m_bw, m_attr) :: items_r ->
						let sized_array, derived_types = Typing.find_array_type (Some Integer.zero) (m_element_t :> all_type) derived_types in
						derived_types, ((m_name, sized_array, m_bw, m_attr) :: items_r)
					| _ ->
						derived_types, items
					end
				in
				(* reverse items *)
				let items = List.rev items in
				(* check bit-field or not *)
				let items =
					begin match Typing.is_bitfield items with
					| `is_bitfield | `mixed ->
						(* normal members and bit-fields are mixed *)
						let items, error_report = Typing.fill_bitfield predefined_types items in
						begin match error_report with
						| `error (item_name, _, _, _) -> error (fst decls) (unalignable_struct_item item_name)
						| `none -> ()
						end;
						items
					| `is_not_bitfield | `empty ->
						items
					end
				in
				(* adjust alignment *)
				let alignment =
					begin match alignment with
					| `aligned n ->
						begin match Typing.alignof_struct items predefined_types with
						| Some good_n ->
							if good_n > n then `aligned good_n else alignment
						| None ->
							alignment
						end
					| _ ->
						alignment
					end
				in
				(* type *)
				begin match id with
				| `some (id_p, `ident id_e) ->
					let attributes = {attributes with at_aligned = alignment} in
					let item, namespace =
						begin match snd sou with
						| `STRUCT ->
							let var = `struct_type (alignment, items) in
							let item = `named (id_p, id_e, var, attributes) in
							let namespace = {namespace with ns_struct = StringMap.add id_e item namespace.ns_struct} in
							item, namespace
						| `UNION ->
							let var = `union items in
							let item = `named (id_p, id_e, var, attributes) in
							let namespace = {namespace with ns_union = StringMap.add id_e item namespace.ns_union} in
							item, namespace
						end
					in
					derived_types, namespace, ((item :> source_item) :: source), Some (item :> all_type)
				| `none ->
					if attributes <> default_attributes then (
						error (fst x) "attribute(s) could not be accepted by the anonymous struct/union.";
					);
					let t: struct_or_union_type_var =
						begin match snd sou with
						| `STRUCT -> `struct_type (alignment, items)
						| `UNION -> `union items
						end
					in
					let item = `anonymous (fst x, t) in
					derived_types, namespace, ((item :> source_item) :: source), Some (item :> all_type)
				end
			| `error ->
				derived_types, namespace, source, None
			end;
		| `no_body (sou, id) ->
			begin match id with
			| `some id ->
				let item, namespace, source =
					begin match snd sou with
					| `STRUCT ->
						Typing.find_struct id namespace source
					| `UNION ->
						Typing.find_union id namespace source
					end
				in
				derived_types, namespace, source, Some item
			| `error ->
				derived_types, namespace, source, None
			end
		end
	) and handle_struct_declaration
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.struct_declaration p)
		: derived_types * namespace * source_item list * struct_item list =
	(
		let attributes = Declaring.attributes_of_alignment alignment in
		let handle_anonymous (x: Syntax.struct_or_union_specifier p) = (
			let derived_types, namespace, source, anonymous_sou = handle_struct_or_union_specifier error predefined_types derived_types namespace source alignment x in
			begin match anonymous_sou with
			| Some t ->
				derived_types, namespace, source, ["", (t :> all_type), None, attributes]
			| None ->
				derived_types, namespace, source, []
			end
		) in
		begin match snd x with
		| `named (_, `some sql, `some sdrl, _) ->
			let derived_types, namespace, source, base_type = handle_specifier_qualifier_list error predefined_types derived_types namespace source alignment sql in
			let derived_types, namespace, source, result =
				Traversing.fold_sdrl (fun (derived_types, namespace, source, rs as result) (sdecl: Syntax.struct_declarator p) ->
					let derived_types, namespace, source, item = handle_struct_declarator error predefined_types derived_types namespace source base_type sdecl in
					begin match item with
					| Some item -> derived_types, namespace, source, (item :: rs)
					| None -> result
					end
				) (derived_types, namespace, source, []) sdrl
			in
			derived_types, namespace, source, List.rev result
		| `named (_, `some sql, `error, _) ->
			begin match snd sql with
			| `type_specifier ((sou_spec_p, `struct_or_union_specifier sou_spec_e), `none) ->
				(* anonymous struct or union without __extension__ *)
				handle_anonymous (sou_spec_p, sou_spec_e)
			| _ ->
				(* error *)
				derived_types, namespace, source, []
			end
		| `named (_, `error, _, _) ->
			(* error *)
			derived_types, namespace, source, []
		| `anonymous_struct_or_union (_, sou_spec, _) ->
			handle_anonymous sou_spec
		end
	) and handle_specifier_qualifier_list
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.specifier_qualifier_list p)
		: derived_types * namespace * source_item list * all_type =
	(
		let derived_types, namespace, source, specs, qualifiers =
			Traversing.fold_sql
				(fun (derived_types, namespace, source, type_specs, qualifiers) (ts: Syntax.type_specifier p) ->
					let derived_types, namespace, source, type_specs = handle_type_specifier error predefined_types derived_types namespace source alignment ts type_specs in
					derived_types, namespace, source, type_specs, qualifiers)
				(fun (derived_types, namespace, source, type_specs, qualifiers) (q: Syntax.type_qualifier p) ->
					let qualifiers = handle_type_qualifier error qualifiers q in
					derived_types, namespace, source, type_specs, qualifiers)
				(derived_types, namespace, source, (no_type_specifier_set, None), no_type_qualifier_set)
				x
		in
		let type_by_spec = get_type_by_specifier_set error predefined_types (fst x) None specs in
		let derived_types, type_by_qualifiers = get_type_by_qualifier_set error derived_types (fst x) type_by_spec qualifiers in
		derived_types, namespace, source, type_by_qualifiers
	) and handle_struct_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(x: Syntax.struct_declarator p)
		: derived_types * namespace * source_item list * struct_item option =
	(
		begin match snd x with
		| `item decl ->
			let derived_types, namespace, source, item = handle_declarator error predefined_types derived_types namespace source base_type no_attributes (fst x, decl) in
			begin match item with
			| Some (_, name, t, attrs) ->
				derived_types, namespace, source, Some (name, t, None, attrs)
			| None ->
				derived_types, namespace, source, None
			end
		| `bit_width (decl, _, expr) ->
			let derived_types, namespace, source, item =
				begin match decl with
				| `some decl ->
					handle_declarator error predefined_types derived_types namespace source base_type no_attributes decl
				| `none ->
					derived_types, namespace, source, Some (fst x, "", base_type, no_attributes)
				end
			in
			let derived_types, source, bw =
				begin match expr with
				| `some expr ->
					let derived_types, source, n = handle_expression error predefined_types derived_types namespace source `rvalue expr in
					begin match n with
					| Some n ->
						begin match integer_of_expression n with
						| Some (_, n) ->
							derived_types, source, Some (-1, Integer.to_int n, true)
						| None ->
							error (fst expr) "int const value was required for bit-field.";
							derived_types, source, None
						end
					| None ->
						derived_types, source, None (* error was already reported *)
					end
				| `error ->
					derived_types, source, None
				end
			in
			begin match item with
			| Some (_, name, t, attrs) ->
				derived_types, namespace, source, Some (name, t, bw, attrs)
			| None ->
				derived_types, namespace, source, None
			end
		end
	) and handle_enum_specifier
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(x: Syntax.enum_specifier p)
		: derived_types * namespace * source_item list * all_type option =
	(
		begin match snd x with
		| `with_body (_, id, _, items, _, _) ->
			(* ready opaque type for recursive ??? *)
			let namespace, source =
				begin match id with
				| `some id ->
					let _, namespace, source = Typing.find_enum id namespace source in
					namespace, source
				| `none ->
					namespace, source
				end
			in
			(* items *)
			let derived_types, namespace, source, _, items =
				begin match items with
				| `some items ->
					Traversing.fold_el (fun (derived_types, namespace, source, next, rs) item ->
						let derived_types, namespace, source, (item, next) = handle_enumerator error predefined_types derived_types namespace source next item in
						derived_types, namespace, source, next, (item :: rs)
					) (derived_types, namespace, source, Integer.zero, []) items
				| `error ->
					derived_types, namespace, source, Integer.zero, []
				end
			in
			let items = List.rev items in
			(* type *)
			let namespace, item =
				begin match id with
				| `some (id_p, `ident id_e) ->
					let item = `named (id_p, id_e, `enum items, no_attributes) in
					let namespace = {namespace with ns_enum = StringMap.add id_e item namespace.ns_enum} in
					namespace, (item :> full_enum_type)
				| `none ->
					let t = `enum items in
					let item = `anonymous (fst x, t) in
					namespace, item
				end
			in
			(* add elements to inverted dictionary *)
			let namespace =
				List.fold_left (fun namespace e ->
					let `named (_, e_name, `enum_element _, _) = e in
					{namespace with ns_enum_of_element = StringMap.add e_name item namespace.ns_enum_of_element}
				) namespace items
			in
			(* add to source *)
			let source = (item :> source_item) :: source in
			derived_types, namespace, source, Some (item :> all_type)
		| `no_body (_, id) ->
			begin match id with
			| `some id ->
				let item, namespace, source = Typing.find_enum id namespace source in
				derived_types, namespace, source, Some item
			| `error ->
				derived_types, namespace, source, None
			end
		end
	) and handle_enumerator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(next: Integer.t)
		(x: Syntax.enumerator p)
		: derived_types * namespace * source_item list * (enum_item * Integer.t) =
	(
		let derived_types, source, name, value =
			begin match snd x with
			| `no_repr name
			| `with_repr ((_, name), _, `error) ->
				derived_types, source, name, next
			| `with_repr ((_, name), _, `some expr) ->
				let derived_types, source, n = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				let value =
					begin match n with
					| Some n ->
						begin match integer_of_expression n with
						| Some (_, n) ->
							n
						| None ->
							error (fst expr) "int const value was required for enumerator.";
							next
						end
					| None ->
						next (* error was already reported *)
					end
				in
				derived_types, source, name, value
			end
		in
		let result = `named (fst x, name, `enum_element value, no_attributes) in
		let namespace = {namespace with ns_namespace = StringMap.add name (result :> named_item) namespace.ns_namespace} in
		let source = (result :> source_item) :: source in
		derived_types, namespace, source, (result, Integer.add value Integer.one)
	) and handle_type_qualifier
		(_: ranged_position -> string -> unit)
		(set: type_qualifier_set)
		(x: Syntax.type_qualifier p)
		: type_qualifier_set =
	(
		begin match snd x with
		| `CONST | `__const ->
			{set with tq_const = true}
		| `RESTRICT | `__restrict | `__restrict__ ->
			{set with tq_restrict = true}
		| `VOLATILE ->
			{set with tq_volatile = true}
		end
	) and handle_function_specifier
		(_: ranged_position -> string -> unit)
		(attributes: attributes)
		(x: Syntax.function_specifier p)
		: attributes =
	(
		begin match snd x with
		| `INLINE | `__inline | `__inline__ ->
			if attributes.at_inline = `always_inline then attributes else
			{attributes with at_inline = `inline}
		end
	) and handle_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(attributes: attributes)
		(x: Syntax.declarator p)
		: derived_types * namespace * source_item list * (ranged_position * string * all_type * attributes) option =
	(
		let pointer, dd, attrs = snd x in
		let derived_types, source, (t, attributes) =
			begin match pointer with
			| `some pointer ->
				handle_pointer error derived_types source base_type attributes pointer
			| `none ->
				derived_types, source, (base_type, attributes)
			end
		in
		let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
		begin match dd with
		| `some dd ->
			handle_direct_declarator error predefined_types derived_types namespace source t attributes dd
		| `error ->
			derived_types, namespace, source, None
		end
	) and handle_direct_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(attributes: attributes)
		(x: Syntax.direct_declarator p)
		: derived_types * namespace * source_item list * (ranged_position * string * all_type * attributes) option =
	(
		begin match snd x with
		| `ident name ->
			let t =
				begin match attributes.at_mode with
				| Some mode ->
					let t, err = Typing.apply_bit_width_mode predefined_types mode base_type in
					begin match err with
					| `none -> ()
					| `not_had size -> error (fst x) (not_had_bit_width_int size)
					| `not_int -> error (fst x) inapplicable_attribute_mode
					end;
					t
				| None ->
					base_type
				end
			in
			derived_types, namespace, source, Some (fst x, name, t, attributes)
		| `paren (_, attrs, d, _) ->
			let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
			begin match d with
			| `some d ->
				handle_declarator error predefined_types derived_types namespace source base_type attributes d
			| `error ->
				derived_types, namespace, source, None
			end
		| `array (dd, _, tql, num, _) ->
			if tql <> `none then (
				error (fst x) "unimplemented!";
				assert false
			);
			let derived_types, source, array_type =
				let derived_types, source, n =
					begin match num with
					| `some expr ->
						let derived_types, source, n = handle_expression error predefined_types derived_types namespace source `rvalue expr in
						begin match n with
						| Some n ->
							begin match integer_of_expression n with
							| Some (_, n) ->
								derived_types, source, Some n
							| None ->
								error (fst expr) "int const value was required for array.";
								derived_types, source, None
							end
						| None ->
							derived_types, source, None (* error was already reported *)
						end
					| `none ->
						derived_types, source, None
					end
				in
				let array_type, derived_types = Typing.find_array_type n base_type derived_types in
				derived_types, source, array_type
			in
			handle_direct_declarator error predefined_types derived_types namespace source array_type attributes dd
		| `static_array1 _ ->
			error (fst x) "unimplemented!";
			assert false
		| `static_array2 _ ->
			error (fst x) "unimplemented!";
			assert false
		| `dynamic_array _ ->
			error (fst x) "unimplemented!";
			assert false
		| `function_type (dd, _, ptl, _) ->
			let conventions = attributes.at_conventions in
			let derived_types, namespace, source, (params, varargs) =
				handle_parameter_type_list error predefined_types derived_types namespace source attributes.at_aligned ptl
			in
			let prototype: prototype = conventions, params, varargs, base_type in
			let func_type, source = Typing.find_function_type prototype source in
			handle_direct_declarator error predefined_types derived_types namespace source func_type attributes dd
		| `old_function_type (dd, _, idl, _) ->
			begin match idl with
			| `some _ ->
				error (fst x) "unimplemented!";
				assert false
			| `none ->
				let conventions = attributes.at_conventions in
				let prototype: prototype = conventions, [], `varargs, base_type in
				let func_type, source = Typing.find_function_type prototype source in
				handle_direct_declarator error predefined_types derived_types namespace source func_type attributes dd
			end
		end
	) and handle_pointer
		(error: ranged_position -> string -> unit)
		(derived_types: derived_types)
		(source: source_item list)
		(t: all_type)
		(attributes: attributes)
		(x: Syntax.pointer p)
		: derived_types * (source_item list) * (all_type * attributes) =
	(
		let apply_pointer (derived_types, source, (t, attributes)) ((_, pk), qs, attrs) = (
			let derived_types, source, t =
				begin match pk with
				| `asterisk ->
					let t, source =
						begin match t with
						| `function_type (cc, params, varargs, ret) when cc <> attributes.at_conventions ->
							Typing.find_function_type (attributes.at_conventions, params, varargs, ret) source
						| _ ->
							t, source
						end
					in
					let t, derived_type = Typing.find_pointer_type t derived_types in
					derived_type, source, t
				| `caret ->
					begin match t with
					| `function_type _ as t ->
						let t, derived_types = Typing.find_block_pointer_type t derived_types in
						derived_types, source, t
					| _ ->
						error (fst x) "block pointer could not be applied to not function type.";
						derived_types, source, t
					end
				end
			in
			let qualifiers = Traversing.opt Traversing.fold_tql (handle_type_qualifier error) no_type_qualifier_set qs in
			let derived_types, t = get_type_by_qualifier_set error derived_types (fst x) t qualifiers in
			let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
			derived_types, source, (t, attributes)
		) in
		Traversing.fold_p apply_pointer (derived_types, source, (t, attributes)) x
	) and handle_parameter_type_list
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.parameter_type_list p)
		: derived_types * namespace * source_item list * (variable list * varargs_opt) =
	(
		let params, varargs =
			begin match snd x with
			| `args params ->
				(fst x, params), `none
			| `varargs (params, _, _) ->
				params, `varargs
			end
		in
		let derived_types, namespace, source, params =
			Traversing.fold_pl (fun (derived_types, namespace, source, rs) param ->
				let derived_types, namespace, source, param = handle_parameter_declaration error predefined_types derived_types namespace source alignment param in
				derived_types, namespace, source, (param :: rs)
			) (derived_types, namespace, source, []) params
		in
		let derived_types, params =
			begin match params with
			| `named (_, _, `variable (`void, _), _) :: [] ->
				derived_types, []
			| _ ->
				(* rev and replace type to pointer instead of array/function *)
				List.fold_left (fun (derived_types, params) param ->
					let `named (ps, name, `variable (the_type, init), attrs) = param in
					begin match resolve_typedef (remove_type_qualifiers the_type :> all_type) with
					| `array (_, base_type) ->
						let t, derived_types = Typing.find_pointer_type (base_type :> all_type) derived_types in
						let new_param = `named (ps, name, `variable (t, init), attrs) in
						derived_types, new_param :: params
					| `function_type _ as ft -> (* illegal, but some headers have this form *)
						let t, derived_types = Typing.find_pointer_type (ft :> all_type) derived_types in
						let new_param = `named (ps, name, `variable (t, init), attrs) in
						derived_types, new_param :: params
					| _ ->
						derived_types, param :: params
					end
				) (derived_types, []) params
			end
		in
		derived_types, namespace, source, (params, varargs)
	) and handle_parameter_declaration
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.parameter_declaration p)
		: derived_types * namespace * source_item list * variable =
	(
		begin match snd x with
		| `with_name (spec, decl) ->
			let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
			if storage_class <> `none then (
				error (fst x) "storage-class was found in parameter-declaration."
			);
			let derived_types, namespace, source, param = handle_declarator error predefined_types derived_types namespace source base_type attributes decl in
			begin match param with
			| Some (ps, name, all_type, attributes) ->
				let result = `named (ps, name, `variable (all_type, None), attributes) in
				derived_types, namespace, source, result
			| None ->
				let result = `named (fst x, "", `variable (base_type, None), attributes) in
				derived_types, namespace, source, result
			end
		| `no_name (spec, decl) ->
			let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
			if storage_class <> `none then (
				error (fst x) "storage-class was found in parameter-declaration."
			);
			begin match decl with
			| `some decl ->
				let derived_types, namespace, source, (t, attributes) = handle_abstract_declarator error predefined_types derived_types namespace source base_type attributes decl in
				let result = `named (fst x, "", `variable (t, None), attributes) in
				derived_types, namespace, source, result
			| `none ->
				let result = `named (fst x, "", `variable (base_type, None), attributes) in
				derived_types, namespace, source, result
			end
		end
	) and handle_type_name
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(x: Syntax.type_name p)
		: derived_types * source_item list * all_type =
	(
		let old_namespace = namespace in
		let sql, decl = snd x in
		let derived_types, namespace, source, base_type = handle_specifier_qualifier_list error predefined_types derived_types namespace source `default sql in
		begin match decl with
		| `some decl ->
			let derived_types, namespace, source, (t, attributes) = handle_abstract_declarator error predefined_types derived_types namespace source base_type no_attributes decl in
			if attributes <> no_attributes then (
				error (fst x) "attribute(s) cound not be accepted in expression.";
			);
			if namespace != old_namespace then (
				error (fst x) "new type-specifier was found in expression."
			);
			derived_types, source, t
		| `none ->
			if namespace != old_namespace then (
				error (fst x) "new type-specifier was found in expression."
			);
			derived_types, source, base_type
		end
	) and handle_abstract_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(attributes: attributes)
		(x: Syntax.abstract_declarator p)
		: derived_types * namespace * source_item list * (all_type * attributes) =
	(
		begin match snd x with
		| `pointer p ->
			let derived_types, source, t_with_attr = handle_pointer error derived_types source base_type attributes (fst x, p) in
			derived_types, namespace, source, t_with_attr
		| `declarator (p, dd) ->
			let derived_types, source, (t, attributes) =
				begin match p with
				| `some p ->
					handle_pointer error derived_types source base_type attributes p
				| `none ->
					derived_types, source, (base_type, attributes)
				end
			in
			handle_direct_abstract_declarator error predefined_types derived_types namespace source t attributes dd
		end
	) and handle_direct_abstract_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(attributes: attributes)
		(x: Syntax.direct_abstract_declarator p)
		: derived_types * namespace * source_item list * (all_type * attributes) =
	(
		begin match snd x with
		| `paren (_, attrs, decl, _) ->
			begin match decl with
			| `some decl ->
				let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
				handle_abstract_declarator error predefined_types derived_types namespace source base_type attributes decl
			| `error ->
				derived_types, namespace, source, (base_type, attributes)
			end
		| `array (dd, _, tql, num, _) ->
			if tql <> `none then (
				error (fst x) "unimplemented!";
				assert false
			);
			let derived_types, source, array_type =
				let derived_types, source, n =
					begin match num with
					| `some expr ->
						let derived_types, source, n = handle_expression error predefined_types derived_types namespace source `rvalue expr in
						begin match n with
						| Some n ->
							begin match integer_of_expression n with
							| Some (_, n) ->
								derived_types, source, Some n
							| None ->
								error (fst expr) "int const value was required for array.";
								derived_types, source, None
							end
						| None ->
							derived_types, source, None (* error was already reported *)
						end
					| `none ->
						derived_types, source, None
					end
				in
				let array_type, derived_types = Typing.find_array_type n base_type derived_types in
				derived_types, source, array_type
			in
			begin match dd with
			| `some dd ->
				handle_direct_abstract_declarator error predefined_types derived_types namespace source array_type attributes dd
			| `none ->
				derived_types, namespace, source, ((array_type :> all_type), attributes)
			end
		| `static_array1 _ ->
			error (fst x) "unimplemented!";
			assert false
		| `static_array2 _ ->
			error (fst x) "unimplemented!";
			assert false
		| `dynamic_array _ ->
			error (fst x) "unimplemented!";
			assert false
		| `function_type (dd, _, ptl, _) ->
			let conventions = attributes.at_conventions in
			let derived_types, namespace, source, (params, varargs) =
				begin match ptl with
				| `some ptl ->
					handle_parameter_type_list error predefined_types derived_types namespace source attributes.at_aligned ptl
				| `none ->
					derived_types, namespace, source, ([], `none)
				end
			in
			let prototype: prototype = conventions, params, varargs, base_type in
			let func_type, source = Typing.find_function_type prototype source in
			begin match dd with
			| `some dd ->
				handle_direct_abstract_declarator error predefined_types derived_types namespace source func_type attributes dd
			| `none ->
				derived_types, namespace, source, (func_type, attributes)
			end
		end
	) and handle_initializer
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(required_type: all_type)
		(x: Syntax.initializer_t p)
		: derived_types * source_item list * expression option =
	(
		begin match snd x with
		| `expression expr ->
			let result = handle_expression error predefined_types derived_types namespace source `rvalue (fst x, expr) in
			begin match result with
			| derived_types, sources, Some expr ->
				derived_types, sources, Some (Expressing.implicit_conv required_type expr)
			| _ ->
				result
			end
		| `list (_, list, _) ->
			begin match list with
			| `some list ->
				handle_initializer_list error predefined_types derived_types namespace source required_type list
			| `error ->
				derived_types, source, None
			end
		end
	) and handle_initializer_list
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(required_type: all_type)
		(x: Syntax.initializer_list p)
		: derived_types * source_item list * expression option =
	(
		let resolved_type1 = resolve_typedef required_type in
		let resolved_type2 = Typing.resolve_opaque namespace resolved_type1 in
		let required_type =
			if resolved_type1 != resolved_type2 then (
				resolved_type2 (* opaque to full declaration *)
			) else (
				required_type (* keep typedef *)
			)
		in
		let kind =
			begin match resolved_type2 with
			| `anonymous (_, `struct_type (_, items))
			| `named (_, _, `struct_type (_, items), _) ->
				`aggregate items
			| `anonymous (_, `union items)
			| `named (_, _, `union items, _) as t ->
				begin match items with
				| x :: _ ->
					`aggregate (x :: [])
				| [] ->
					`array (-1, remove_type_qualifiers t) (* empty union *)
				end
			| `array (size, t) ->
				let size =
					begin match size with
					| Some size -> Integer.to_int size
					| None -> -1
					end
				in
				`array (size, (t :> not_qualified_type))
			| `named(ps, _, `generic_type, _) ->
				`array (-1, `named (ps, "", `generic_type, no_attributes))
			| t ->
				error (fst x) "bad type for initializer-list.";
				`array (-1, remove_type_qualifiers t)
			end
		in
		let derived_types, source, kind, list =
			Traversing.fold_il (fun (derived_types, source, kind, rs) (d, i: Syntax.designation opt * Syntax.initializer_t e) ->
				let element_type, kind =
					begin match kind with
					| `aggregate items ->
						begin match items with
						| (_, t, _, _) :: items_r ->
							t, `aggregate items_r
						| [] ->
							error (fst x) "too many elements for struct or union.";
							let dummy_type = `named (fst x, "", `generic_type, no_attributes) in
							dummy_type, `aggregate []
						end
					| `array (size, t) ->
						(t :> all_type), `array (size - 1, t)
					end
				in
				begin match d with
				| `some (d_p, _) ->
					error d_p "unimplemented.";
					assert false
				| `none ->
					()
				end;
				begin match i with
				| `some init ->
					let derived_types, source, elm = handle_initializer error predefined_types derived_types namespace source element_type init in
					begin match elm with
					| Some elm ->
						derived_types, source, kind, (elm :: rs)
					| None ->
						error (fst x) "unimplemented.";
						assert false
					end
				| `error ->
					error (fst x) "unimplemented.";
					assert false
				end
			) (derived_types, source, kind, []) x
		in
		let list, zero =
			begin match kind with
			| `aggregate items ->
				let rec filling_zero_loop items list = (
					begin match items with
					| (_, t, _, _) :: items_r ->
						begin match Expressing.zero t with
						| Some zero ->
							let list = zero :: list in
							filling_zero_loop items_r list
						| None ->
							error (fst x) "unimplemented.";
							assert false
						end
					| [] ->
						list
					end
				) in
				let list = filling_zero_loop items list in
				list, None
			| `array (size, t) ->
				if size <= 0 then list, None else
				begin match Expressing.zero (t :> all_type) with
				| Some _ as zero ->
					list, zero
				| None ->
					error (fst x) "unimplemented.";
					assert false
				end
			end
		in
		derived_types, source, Some (`compound (List.rev list, zero), required_type)
	) and handle_statement
		?(control: [`loop | `switch | `none] = `none)
		?(return_type: all_type option)
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.statement p)
		: derived_types * source_item list * statement option =
	(
		let handle_statement_or_error ?control derived_types source (statement: Syntax.statement e) = (
			begin match statement with
			| `some (cs_p, `compound cs_e) ->
				handle_compound_statement ?control ?return_type error predefined_types derived_types namespace source alignment (cs_p, cs_e)
			| `some statement ->
				let derived_types, source, statement = handle_statement ?control ?return_type error predefined_types derived_types namespace source alignment statement in
				begin match statement with
				| Some statement ->
					derived_types, source, statement :: []
				| None ->
					derived_types, source, []
				end
			| `error ->
				derived_types, source, []
			end
		) in
		begin match snd x with
		| `asm (_, volatile, _, code, in_args, _, _) ->
			let handle_arg request (derived_types, source, rs) arg = (
				let _, ((_, `chars_literal reg), _, expr, _) = arg in
				begin match expr with
				| `some expr ->
					let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source request expr in
					begin match expr with
					| Some expr ->
						derived_types, source, (reg, expr) :: rs
					| None ->
						derived_types, source, rs
					end
				| `error ->
					derived_types, source, rs
				end
			) in
			let volatile =
				begin match volatile with
				| `some (_, (`VOLATILE | `__volatile__)) ->
					`volatile
				| `none ->
					`none
				end
			in
			begin match code with
			| `some (_, `chars_literal code) ->
				let derived_types, source, in_args, out_args, destructive =
					begin match in_args with
					| `some (_, (_, in_args, out_args)) ->
						let derived_types, source, in_args = Traversing.opt Traversing.fold_iaal (handle_arg `lvalue) (derived_types, source, []) in_args in
						let in_args = List.rev in_args in
						begin match out_args with
						| `some (_, (_, out_args, destructive)) ->
							let derived_types, source, out_args = Traversing.opt Traversing.fold_iaal (handle_arg `rvalue) (derived_types, source, []) out_args in
							let out_args = List.rev out_args in
							begin match destructive with
							| `some (_, (_, `some destructive)) ->
								let destructive = Traversing.fold_right_iarl (fun (_, `chars_literal reg) rs -> reg :: rs) destructive [] in
								derived_types, source, in_args, out_args, destructive
							| `some (_, (_, `error)) | `none ->
								derived_types, source, in_args, out_args, []
							end
						| `none ->
							derived_types, source, in_args, [], []
						end
					| `none ->
						derived_types, source, [], [], []
					end
				in
				derived_types, source, Some (`asm (volatile, code, in_args, out_args, destructive))
			| `error ->
				derived_types, source, None
			end
		| `label ((_, `ident label), _, stmt) ->
			let derived_types, source, stmt = handle_statement_or_error ~control derived_types source stmt in
			derived_types, source, Some (`label (label, stmt))
		| `case _ ->
			error (fst x) "unimplemented!";
			assert false
		| `default _ ->
			error (fst x) "unimplemented!";
			assert false
		| `compound cs_e ->
			let derived_types, source, stmts = handle_compound_statement ~control ?return_type error predefined_types derived_types namespace source alignment (fst x, cs_e) in
			derived_types, source, Some (`compound stmts)
		| `expression (expr, _) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					derived_types, source, Some (`expression expr)
				| None ->
					derived_types, source, None
				end
			| `none ->
				derived_types, source, None
			end
		| `__builtin_va_start (_, _, arg, _, format, _, _) ->
			begin match arg, format with
			| `some arg, `some format ->
				let derived_types, source, arg = handle_expression error predefined_types derived_types namespace source `lvalue arg in
				let derived_types, source, format = handle_expression error predefined_types derived_types namespace source `rvalue format in
				begin match arg, format with
				| Some arg, Some format ->
					derived_types, source, Some (`va_start (arg, format))
				| _ ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `__builtin_va_end (_, _, arg, _, _) ->
			begin match arg with
			| `some arg ->
				let derived_types, source, arg = handle_expression error predefined_types derived_types namespace source `lvalue arg in
				begin match arg with
				| Some arg ->
					derived_types, source, Some (`va_end arg)
				| _ ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `__builtin_va_copy (_, _, dest, _, src, _, _) ->
			begin match dest, src with
			| `some dest, `some src ->
				let derived_types, source, dest = handle_expression error predefined_types derived_types namespace source `lvalue dest in
				let derived_types, source, src = handle_expression error predefined_types derived_types namespace source `rvalue src in
				begin match dest, src with
				| Some dest, Some src ->
					derived_types, source, Some (`va_copy (dest, src))
				| _ ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `if_then (_, _, expr, _, true_case) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				let derived_types, source, true_case = handle_statement_or_error ~control derived_types source true_case in
				begin match expr with
				| Some expr ->
					let expr = Expressing.implicit_conv `bool expr in
					derived_types, source, Some (`if_statement (expr, true_case, []))
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `if_then_else (_, _, expr, _, true_case, _, false_case) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				let derived_types, source, true_case = handle_statement_or_error ~control derived_types source true_case in
				let derived_types, source, false_case = handle_statement_or_error ~control derived_types source false_case in
				begin match expr with
				| Some expr ->
					let expr = Expressing.implicit_conv `bool expr in
					derived_types, source, Some (`if_statement (expr, true_case, false_case))
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `switch _ ->
			error (fst x) "unimplemented!";
			assert false
		| `while_loop (_, _, expr, _, stmt) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					let derived_types, source, stmt = handle_statement_or_error ~control:`loop derived_types source stmt in
					derived_types, source, Some (`while_loop (expr, stmt))
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `do_loop (_, stmt, _, _, expr, _, _) ->
			let derived_types, source, stmt = handle_statement_or_error ~control:`loop derived_types source stmt in
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					derived_types, source, Some (`do_loop (stmt, expr))
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `for_loop (_, _, init_expr, _, cond_expr, _, next_expr, _, stmt) ->
			let derived_types, source, init_expr =
				begin match init_expr with
				| `some init_expr ->
					handle_expression error predefined_types derived_types namespace source `rvalue init_expr
				| `none ->
					derived_types, source, None
				end
			in
			let derived_types, source, cond_expr =
				begin match cond_expr with
				| `some cond_expr ->
					handle_expression error predefined_types derived_types namespace source `rvalue cond_expr
				| `none ->
					derived_types, source, None
				end
			in
			let derived_types, source, next_expr =
				begin match next_expr with
				| `some next_expr ->
					handle_expression error predefined_types derived_types namespace source `rvalue next_expr
				| `none ->
					derived_types, source, None
				end
			in
			let derived_types, source, stmt = handle_statement_or_error ~control:`loop derived_types source stmt in
			derived_types, source, Some (`for_loop (init_expr, cond_expr, next_expr, stmt))
		| `for_with_declaration _ ->
			error (fst x) "unimplemented!";
			assert false
		| `goto (_, label, _) ->
			begin match label with
			| `some (_, `ident label) ->
				derived_types, source, Some (`goto label)
			| `error ->
				derived_types, source, None
			end
		| `continue _ ->
			error (fst x) "unimplemented!";
			assert false
		| `break _ ->
			begin match control with
			| `loop | `switch as control ->
				derived_types, source, Some (`break control)
			| `none ->
				error (fst x) "break was found out of loop or switch.";
				derived_types, source, None
			end
		| `return (_, expr, _) ->
			begin match return_type with
			| Some return_type ->
				begin match expr with
				| `some expr ->
					let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
					let expr =
						begin match expr with
						| Some expr -> Some (Expressing.implicit_conv return_type expr)
						| None -> None
						end
					in
					derived_types, source, Some (`return (expr, return_type))
				| `none ->
					derived_types, source, Some (`return (None, return_type))
				end
			| None ->
				error (fst x) "return statement was not allowed here.";
				derived_types, source, None
			end
		end
	) and handle_compound_statement
		?(control: [`loop | `switch | `none] = `none)
		?(return_type: all_type option)
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.compound_statement p)
		: derived_types * source_item list * statement list =
	(
		let _, (_, items, _) = x in
		begin match items with
		| `some items ->
			let items = Traversing.fold_right_bil (fun item rs -> item :: rs) items [] in
			let rec loop derived_types namespace source (xs: Syntax.block_item p list) rs = (
				begin match xs with
				| x :: xr ->
					begin match snd x with
					| `declaration decl ->
						let derived_types, namespace, local, _ = handle_declaration error predefined_types derived_types namespace [] alignment (fst x, decl) in
						let local = List.rev local in (* source order *)
						let derived_types, source, stmts = loop derived_types namespace source xr [] in
						let local = `local (local, stmts) in
						derived_types, source, List.rev (local :: List.rev rs)
					| `statement stmt ->
						let derived_types, source, stmt = handle_statement ~control ?return_type error predefined_types derived_types namespace source alignment (fst x, stmt) in
						let rs =
							begin match stmt with
							| Some stmt -> stmt :: rs
							| None -> rs
							end
						in
						loop derived_types namespace source xr rs
					end
				| [] ->
					derived_types, source, List.rev rs
				end
			) in
			loop derived_types namespace source items []
		| `none ->
			derived_types, source, []
		end
	);;
	
	let rec handle_translation_unit
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(sources: (source_item list * extra_info) StringMap.t)
		(x: Syntax.translation_unit)
		: derived_types * namespace * (source_item list * extra_info) StringMap.t * alignment list * mapping_options =
	(
		let in_f (included: string list) (current_filename: string) (derived_types, namespace, sources, alignment_stack, mapping_options) = (
			let current_source, current_info = StringMap.find_or ~default:empty_source current_filename sources in
			let current_source =
				List.fold_left (fun current_source h -> `include_point h :: current_source) current_source included
			in
			derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
		) in
		let out_f (previous_filename: string) (derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info) = (
			let sources = StringMap.add previous_filename (current_source, current_info) sources in
			derived_types, namespace, sources, alignment_stack, mapping_options
		) in
		Traversing.fold_tu (fun (derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info) (def_p, def_e) ->
			let alignment = List.hd alignment_stack in
			begin match def_e with
			| `function_definition fdef ->
				let derived_types, namespace, current_source, _ = handle_function_definition error predefined_types derived_types namespace current_source alignment (def_p, fdef) in
				derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
			| `aliased_declaration adef ->
				let derived_types, namespace, current_source, _ = handle_aliased_declaration error predefined_types derived_types namespace current_source alignment (def_p, adef) in
				derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
			| `declaration decl ->
				let derived_types, namespace, current_source, _ = handle_declaration error predefined_types derived_types namespace current_source alignment (def_p, decl) in
				derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
			| `pragma pragma ->
				let derived_types, current_info, alignment_stack, mapping_options = handle_pragma error predefined_types derived_types namespace current_info alignment_stack mapping_options (def_p, pragma) in
				derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
			end
		) in_f out_f (derived_types, namespace, sources, [`default], no_mapping_options) x
	) and handle_function_definition
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.function_definition p)
		: derived_types * namespace * source_item list * [> [> function_definition] with_name] option =
	(
		let _, (spec, decl, params, stmt) = x in
		if params <> `none then (
			error (fst x) "unimplemented!";
			assert false
		);
		let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
		begin match decl with
		| `some decl ->
			let derived_types, namespace, source, analyzed_decl = handle_declarator error predefined_types derived_types namespace source base_type attributes decl in
			begin match analyzed_decl with
			| Some (ps, id, t, attr) ->
				begin match t with
				| `function_type prototype as t ->
					let st =
						begin match storage_class with
						| `static ->
							`static
						| `none ->
							`none
						| `extern when attr.at_inline <> `none ->
							`extern_inline
						| `extern | `typedef | `auto | `register ->
							error (fst x) "bad storage-class was found in function-definition.";
							`none
						end
					in
					let derived_types, source, body =
						begin match stmt with
						| `some stmt ->
							(* add parameters to namespace *)
							let _, args, _, return_type = prototype in
							let local =
								List.fold_left (fun local arg ->
									let `named (_, name, _, _) = arg in
									{local with ns_namespace = StringMap.add name (arg :> named_item) local.ns_namespace}
								) namespace args
							in
							handle_compound_statement ~return_type error predefined_types derived_types local source alignment stmt
						| `error ->
							derived_types, source, []
						end
					in
					let result = `named (ps, id, `function_definition (st, t, body), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id (result :> named_item) namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some (result)
				| _ ->
					error ps "function-type was expected.";
					derived_types, namespace, source, None
				end
			| None ->
				derived_types, namespace, source, None
			end
		| `error ->
			derived_types, namespace, source, None
		end
	) and handle_aliased_declaration
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.aliased_declaration p)
		: derived_types * namespace * source_item list * named_item option =
	(
		let _, (spec, decl, _, _, alias, _, attrs, _) = x in
		let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
		let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
		let derived_types, namespace, source, item =
			begin match decl with
			| `some (decl_p, decl_e) ->
				let alias =
					begin match alias with
					| `some (_, `chars_literal alias) -> `alias alias
					| `error -> `none
					end
				in
				let idecl = decl_p, `no_init decl_e in (* provisional... *)
				handle_init_declarator error predefined_types derived_types namespace source storage_class base_type attributes alias idecl
			| `error ->
				derived_types, namespace, source, None
			end
		in
		derived_types, namespace, source, item
	);;
	
	let analyze
		(error: ranged_position -> string -> unit)
		(lang: language)
		(sizeof: sizeof)
		(typedef: language_typedef)
		(builtin: (string *
			[< predefined_type | `pointer of [< predefined_type | `const of [< predefined_type]] | `size_t] list *
			[< predefined_type | `pointer of [< predefined_type | `const of [< predefined_type]] | `size_t]) list)
		(translation_unit: Syntax.translation_unit)
		: predefined_types * derived_types * namespace * (source_item list * extra_info) StringMap.t * mapping_options =
	(
		(* predefined types *)
		let predefined_types = Typing.ready_predefined_types lang sizeof typedef in
		(* builtin functions *)
		let derived_types, namespace, builtin_source =
			Declaring.ready_builtin_functions predefined_types [] empty_namespace builtin
		in
		let sources = StringMap.add builtin_name builtin_source StringMap.empty in
		(* analyzing translation unit *)
		let derived_types, namespace, sources, _, mapping_options =
			handle_translation_unit error predefined_types derived_types namespace sources translation_unit
		in
		(* result is reverse order *)
		predefined_types, derived_types, namespace, sources, mapping_options
	);;
	
	(* reverse items to declared order *)
	
	let rev
		(derived_types: derived_types)
		(sources: (source_item list * extra_info) StringMap.t)
		: derived_types * (source_item list * extra_info) StringMap.t =
	(
		let sources = StringMap.map (fun (items, info) -> List.rev items, info) sources in
		let derived_types = List.rev derived_types in
		derived_types, sources
	);;
	
end;;

module type AnalyzerType = sig
	module Literals: LiteralsType;;
	module Syntax: SyntaxType
		with module Literals := Literals;;
	module Semantics: SemanticsType
		with module Literals := Literals;;
	include module type of Analyzer (Literals) (Syntax) (Semantics);;
end;;
