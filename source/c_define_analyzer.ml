open C_analyzer;;
open C_literals;;
open C_semantics;;
open C_syntax;;
open Position;;

type known_errors_of_define_analzer = [
	| `uninterpretable_macro];;

module type DefineAnalyzerType = sig
	module Literals: LiteralsType;;
	module Syntax: SyntaxType
		with module Literals := Literals;;
	module Semantics: SemanticsType
		with module Literals := Literals;;
	
	type define = [
		| `operator of iso646_operator
		| `declaration_specifiers of Syntax.declaration_specifiers
		| `initializer_t of Syntax.initializer_t
		| `function_expr of (string Syntax.p * [`typedef | `value]) list * [`varargs | `none] * Syntax.expression
		| `function_stmt of (string Syntax.p * [`typedef | `value]) list * [`varargs | `none] * Syntax.statement
		| `any of string];;
	
	val handle_define:
		(ranged_position -> string -> unit) ->
		(ranged_position -> string -> [> known_errors_of_define_analzer] -> bool) ->
		Semantics.predefined_types ->
		Semantics.derived_types ->
		Semantics.namespace ->
		Semantics.source_item list ->
		Semantics.mapping_options ->
		string ->
		define Syntax.p ->
		Semantics.derived_types * Semantics.source_item list;;
	
	val map:
		(ranged_position -> string -> unit) ->
		(ranged_position -> string -> [> known_errors_of_define_analzer] -> bool) ->
		Semantics.predefined_types ->
		Semantics.derived_types ->
		Semantics.namespace ->
		(Semantics.source_item list * Semantics.extra_info) StringMap.t ->
		Semantics.mapping_options ->
		(define Syntax.p) StringMap.t ->
		Semantics.derived_types * (Semantics.source_item list * Semantics.extra_info) StringMap.t;;
	
end;;

module DefineAnalyzer
	(Literals: LiteralsType)
	(Syntax: SyntaxType
		with module Literals := Literals)
	(Semantics: SemanticsType
		with module Literals := Literals)
	(Analyzer: AnalyzerType
		with module Literals := Literals
		with module Syntax := Syntax
		with module Semantics := Semantics)
	: DefineAnalyzerType
		with module Literals := Literals
		with module Syntax := Syntax
		with module Semantics := Semantics =
struct
	open Semantics;;
	open Analyzer;;
	
	(* error messages for type inference *)
	
	let no_struct_or_union_having (s: string): string =
		"no struct or union having " ^ s ^ ".";;
	
	(* type inference *)
	
	let inference_by_field
		(error: ranged_position -> string -> unit)
		(ps: ranged_position)
		(accessing: string list)
		(source: source_item list)
		: [`defined_element_access of struct_or_union_type * struct_item list
			| `error] =
	(
		(* search struct having the field of same name *)
		let rec source_loop ss = (
			begin match ss with
			| s :: sr ->
				begin match s with
				| `anonymous (_, `struct_type (_, items))
				| `anonymous (_, `union items)
				| `named (_, _, `struct_type (_, items), _)
				| `named (_, _, `union items, _) as t ->
					let rec item_loop root_t items accessing route = (
						begin match find_field (List.hd accessing) items with
						| Some ((field_name, field_type, _, _) as field) ->
							let route = field :: route in
							begin match List.tl accessing with
							| _ :: _ as ac_r ->
								begin match field_type with
								| `anonymous (_, `struct_type (_, items))
								| `anonymous (_, `union items)
								| `named (_, _, `struct_type (_, items), _)
								| `named (_, _, `union items, _) ->
									item_loop root_t items ac_r route
								| _ ->
									error ps (is_not_a_struct_or_union field_name);
									`error
								end
							| [] ->
								`defined_element_access (root_t, List.rev route)
							end
						| None ->
							source_loop sr
						end
					) in
					item_loop t items accessing []
				| _ ->
					source_loop sr
				end
			| [] ->
				if List.length accessing = 1 then (
					error ps (is_undeclared (List.hd accessing))
				) else (
					error ps (no_struct_or_union_having (List.hd accessing))
				);
				`error
			end
		) in
		source_loop source
	);;
	
	(* error messages for define-analyzer *)
	
	let alias_macro_is_failed_to_interpret (s: string): string =
		"alias macro " ^ s ^ " is failed to interpret.";;
	let is_an_alias_of_plural_type_qualifiers (s: string): string =
		"macro " ^ s ^ " is an alias of plural type-qualifiers.";;
	let is_an_alias_of_plural_kinds_of_declaration_specifiers (s: string): string =
		"macro " ^ s ^ " is an alias of plural kinds of declaration specifiers.";;
	let typedef_macro_is_making_new_type_specifier (s: string): string =
		"typedef-like macro " ^ s ^ " is making new type-specifier.";;
	let typedef_macro_is_failed_to_interpret (s: string): string =
		"typedef-like macro " ^ s ^ " is failed to interpret.";;
	let initializer_macro_is_overloaded (s: string): string =
		"initializer-like macro " ^ s ^ " is overloaded with #pragma instance.";;
	let initializer_macro_is_failed_to_interpret (s: string): string =
		"initializer-like macro " ^ s ^ " is failed to interpret.";;
	let expression_macro_is_failed_to_interpret (s: string): string =
		"expression-like macro " ^ s ^ " is failed to interpret.";;
	let statement_macro_is_failed_to_interpret (s: string): string =
		"statement-like macro " ^ s ^ " is failed to interpret.";;
	let assert_failed (s: string): string =
		"assert is failed to analyzing macro " ^ s ^ ". (define-analyzer)";;
	
	(* define type *)
	
	type define = [
		| `operator of iso646_operator
		| `declaration_specifiers of Syntax.declaration_specifiers
		| `initializer_t of Syntax.initializer_t
		| `function_expr of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.expression
		| `function_stmt of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.statement
		| `any of string];;
	
	(* define op *)
	
	let is_inline_version (namespace: namespace) (name: string) (define: define p): bool = (
		StringMap.mem name namespace.ns_namespace && (
			let _, def_e = define in
			begin match def_e with
			| `function_expr (m_args, m_varargs, _)
			| `function_stmt (m_args, m_varargs, _) when List.for_all (fun (_, k) -> k = `value) m_args ->
				let existed = StringMap.find name namespace.ns_namespace in
				begin match existed with
				| `named (_, _, `function_forward (_, t), _)
				| `named (_, _, `function_definition (_, t, _), _)
				| `named (_, _, `extern ((#function_type as t), _), _) ->
					let `function_type (_, f_args, f_varargs, _) = t in
					List.length f_args = List.length m_args && f_varargs = m_varargs
				| _ ->
					false
				end
			| `any _ (* if a duplicated item is `any _, it will be ignored. *)
			| _ ->
				false
			end
		)
	);;
	
	(* define analyzer *)
	
	let handle_define
		(error: ranged_position -> string -> unit)
		(is_known_error: ranged_position -> string -> [> known_errors_of_define_analzer] -> bool)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(mapping_options: mapping_options)
		(name: string)
		(define: define p)
		: derived_types * source_item list =
	(
		let def_p, def_e = define in
		if is_inline_version namespace name define then (
			derived_types, source (* suppressing inline-version function macro *)
		) else (
			let new_any (message: string) =
				`named (def_p, name, `defined_any message, no_attributes)
			in
			let new_local (args: (string p * [`typedef | `value]) list) = (
				let local, formal_types =
					List.fold_left (fun (local, formal_types as r) ((ft_p, formal_type), kind) ->
						if kind <> `typedef then r else
						(* dummy type *)
						let item = `named (ft_p, formal_type, `generic_type, no_attributes) in
						{local with ns_namespace = StringMap.add formal_type item local.ns_namespace},
						item :: formal_types
					) (namespace, []) args
				in
				let local, args =
					List.fold_left (fun (local, args as r) ((arg_p, arg), kind) ->
						if kind <> `value then r else
						(* dummy type *)
						let t = `named (arg_p, "", `generic_type, no_attributes) in
						(* dummy value *)
						let item = `named (def_p, arg, `generic_value t, no_attributes) in
						{local with ns_namespace = StringMap.add arg item local.ns_namespace},
						item :: args
					) (local, []) args
				in
				local, List.rev formal_types, List.rev args
			) in
			let redirected_error = ref None in
			let redirect_error ps message = (
				begin match !redirected_error with
				| None -> redirected_error := Some (ps, message)
				| Some _ -> ()
				end
			) in
			begin try
				begin match def_e with
				| `operator op ->
					let item = `named (def_p, name, `defined_operator op, no_attributes) in
					let source = item :: source in
					derived_types, source
				| `declaration_specifiers spec ->
					let rec only_no_type
						(storage_class, type_qualifiers, type_specifiers, attributes)
						(x: Syntax.declaration_specifiers)
						: ([storage_class | `none] * type_qualifier_set * type_specifier_set * attributes) option =
					(
						let do_next result next = (
							begin match next with
							| `some (_, next) ->
								only_no_type result next
							| `none ->
								Some result
							end
						) in
						begin match x with
						| `storage_class_specifier (sc, next) ->
							let storage_class = handle_storage_class redirect_error storage_class sc in
							do_next (storage_class, type_qualifiers, type_specifiers, attributes) next
						| `type_specifier ((_, `_COMPLEX), next) ->
							let type_specifiers = {type_specifiers with ts_complex = type_specifiers.ts_complex + 1} in
							do_next (storage_class, type_qualifiers, type_specifiers, attributes) next
						| `type_specifier ((_, `_IMAGINARY), next) ->
							let type_specifiers = {type_specifiers with ts_imaginary = type_specifiers.ts_imaginary + 1} in
							do_next (storage_class, type_qualifiers, type_specifiers, attributes) next
						| `type_specifier _ ->
							None
						| `type_qualifier (tq, next) ->
							let type_qualifiers = handle_type_qualifier redirect_error type_qualifiers tq in
							do_next (storage_class, type_qualifiers, type_specifiers, attributes) next
						| `function_specifier (fs, next) ->
							let attributes = handle_function_specifier redirect_error attributes fs in
							do_next (storage_class, type_qualifiers, type_specifiers, attributes) next
						| `attributes (attr, next) ->
							let attributes = handle_attribute redirect_error attributes attr in
							do_next (storage_class, type_qualifiers, type_specifiers, attributes) next
						| `extension (_, next) ->
							do_next (storage_class, type_qualifiers, type_specifiers, attributes) next
						end
					) in
					begin match only_no_type (`none, no_type_qualifier_set, no_type_specifier_set, no_attributes) spec with
					| Some (storage_class, type_qualifiers, type_specifiers, attributes) ->
						begin match !redirected_error with
						| Some (r_ps, r_message) ->
							error r_ps r_message;
							error def_p (typedef_macro_is_failed_to_interpret name);
							let source = new_any "bad declaration-specifiers" :: source in
							derived_types, source
						| None ->
							(* storage class *)
							begin match storage_class with
							| #storage_class as storage_class ->
								if type_qualifiers <> no_type_qualifier_set || type_specifiers <> no_type_specifier_set then (
									error def_p (is_an_alias_of_plural_kinds_of_declaration_specifiers name)
								);
								let item = `named (def_p, name, `defined_storage_class storage_class, attributes) in
								let source = item :: source in
								derived_types, source
							| `none ->
								(* type qualifiers *)
								let type_qualifier =
									begin match type_qualifiers with
									| {tq_const = true; tq_restrict = false; tq_volatile = false} ->
										Some `const
									| {tq_const = true; tq_restrict = _; tq_volatile = _} ->
										error def_p (is_an_alias_of_plural_type_qualifiers name);
										Some `const
									| {tq_const = false; tq_restrict = true; tq_volatile = false} ->
										Some `restrict
									| {tq_const = false; tq_restrict = true; tq_volatile = _} ->
										error def_p (is_an_alias_of_plural_type_qualifiers name);
										Some `restrict
									| {tq_const = false; tq_restrict = false; tq_volatile = true} ->
										Some `volatile
									| {tq_const = false; tq_restrict = false; tq_volatile = false} ->
										None
									end
								in
								begin match type_qualifier with
								| Some type_qualifier ->
									if type_specifiers <> no_type_specifier_set then (
										error def_p (is_an_alias_of_plural_kinds_of_declaration_specifiers name)
									);
									let item = `named (def_p, name, `defined_type_qualifier type_qualifier, attributes) in
									let source = item :: source in
									derived_types, source
								| None ->
									(* type specifiers *)
									if type_specifiers = {no_type_specifier_set with ts_complex = 1} then (
										let item = `named (def_p, name, `defined_type_specifier `complex, attributes) in
										let source = item :: source in
										derived_types, source
									) else if type_specifiers = {no_type_specifier_set with ts_imaginary = 1} then (
										let item = `named (def_p, name, `defined_type_specifier `imaginary, attributes) in
										let source = item :: source in
										derived_types, source
									) else (
										(* attributes *)
										let item = `named (def_p, name, `defined_attributes, attributes) in
										let source = item :: source in
										derived_types, source
									)
								end
							end
						end
					| None ->
						let derived_types, n2, source, (storage_class, base_type, attributes) =
							handle_declaration_specifiers redirect_error predefined_types derived_types namespace source `default (def_p, spec)
						in
						begin match !redirected_error with
						| Some (r_ps, r_message) ->
							error r_ps r_message;
							error def_p (typedef_macro_is_failed_to_interpret name);
							let source = new_any "bad typedef" :: source in
							derived_types, source
						| None ->
							if storage_class <> `none then (
								error def_p (is_an_alias_of_plural_kinds_of_declaration_specifiers name);
								let source = new_any "bad typedef" :: source in
								derived_types, source
							) else if n2 != namespace then (
								error def_p (typedef_macro_is_making_new_type_specifier name);
								let source = new_any "bad typedef" :: source in
								derived_types, source
							) else (
								let item = `named (def_p, name, `defined_typedef base_type, attributes) in
								let source = item :: source in
								derived_types, source
							)
						end
					end
				| `initializer_t init ->
					let is_element_access (x: Syntax.initializer_t): string list = (
						let rec loop rs (x: Syntax.expression): string list = (
							begin match x with
							| `ident name ->
								name :: rs
							| `element_access ((_, obj), _, `some (_, `ident field)) ->
								loop (field :: rs) obj
							| _ ->
								[]
							end
						) in
						begin match x with
						| `expression expr -> loop [] expr
						| `list _ -> []
						end
					) in
					begin match is_element_access init with
					| leftmost :: [] when StringMap.mem leftmost namespace.ns_opaque_enum ->
						let t = (StringMap.find leftmost namespace.ns_opaque_enum :> opaque_type) in
						let item = `named (def_p, name, `defined_opaque_type t, no_attributes) in
						let source = item :: source in
						derived_types, source
					| leftmost :: [] when StringMap.mem leftmost namespace.ns_opaque_struct ->
						let t = (StringMap.find leftmost namespace.ns_opaque_struct :> opaque_type) in
						let item = `named (def_p, name, `defined_opaque_type t, no_attributes) in
						let source = item :: source in
						derived_types, source
					| leftmost :: [] when StringMap.mem leftmost namespace.ns_opaque_union ->
						let t = (StringMap.find leftmost namespace.ns_opaque_union :> opaque_type) in
						let item = `named (def_p, name, `defined_opaque_type t, no_attributes) in
						let source = item :: source in
						derived_types, source
					| leftmost :: _ as accessing when not (StringMap.mem leftmost namespace.ns_namespace) ->
						begin match inference_by_field error def_p accessing source with
						| `defined_element_access _ as result ->
							let item = `named (def_p, name, result, no_attributes) in
							let source = item :: source in
							derived_types, source
						| `error ->
							error def_p (alias_macro_is_failed_to_interpret name);
							derived_types, source
						end
					| _ ->
						let required_type =
							begin try
								begin match StringMap.find name mapping_options.mo_instances with
								| t :: [] ->
									(t :> [all_type | `uninterpretable])
								| _ as ts ->
									if List.length ts > 1 then (
										error def_p (initializer_macro_is_overloaded name)
									);
									raise Not_found
								end
							with Not_found ->
								if is_known_error def_p name `uninterpretable_macro then (
									(`uninterpretable :> [all_type | `uninterpretable])
								) else (
									`named (def_p, "", `generic_type, no_attributes) (* dummy type *)
								)
							end
						in
						begin match required_type with
						| `uninterpretable ->
							let source = new_any "uninterpretable" :: source in
							derived_types, source
						| #all_type as required_type ->
							let derived_types, source, expr =
								handle_initializer redirect_error predefined_types derived_types namespace source required_type (def_p, init)
							in
							begin match !redirected_error with
							| Some (r_ps, r_message) ->
								error r_ps r_message;
								error def_p (initializer_macro_is_failed_to_interpret name);
								let source = new_any "bad initializer" :: source in
								derived_types, source
							| None ->
								let item =
									begin match expr with
									| Some expr ->
										begin match expr with
										| _, `named (_, "", `generic_type, _) -> (* failed to type inference *)
											new_any "plase type with #pragma instance"
										| _ ->
											`named (def_p, name, `defined_expression expr, no_attributes)
										end
									| None ->
										assert false
									end
								in
								let source = item :: source in
								derived_types, source
							end
						end
					end
				| `function_expr (args, varargs, expr) ->
					if is_known_error def_p name `uninterpretable_macro then (
						let source = new_any "uninterpretable" :: source in
						derived_types, source
					) else (
						let local, formal_types, args = new_local args in
						let derived_types, source, expr =
							handle_expression redirect_error predefined_types derived_types local source `rvalue (def_p, expr)
						in
						begin match !redirected_error with
						| Some (r_ps, r_message) ->
							error r_ps r_message;
							error def_p (expression_macro_is_failed_to_interpret name);
							let source = new_any "bad expression" :: source in
							derived_types, source
						| None ->
							let item =
								begin match expr with
								| Some expr ->
									`named (def_p, name, `defined_generic_expression (formal_types, args, varargs, expr), no_attributes)
								| None ->
									assert false
								end
							in
							let source = item :: source in
							derived_types, source
						end
					)
				| `function_stmt (args, varargs, stmt) ->
					if is_known_error def_p name `uninterpretable_macro then (
						let source = new_any "uninterpretable" :: source in
						derived_types, source
					) else (
						let local, formal_types, args = new_local args in
						let derived_types, source, stmt =
							handle_statement redirect_error predefined_types derived_types local source `default (def_p, stmt)
						in
						begin match !redirected_error with
						| Some (r_ps, r_message) ->
							error r_ps r_message;
							error def_p (statement_macro_is_failed_to_interpret name);
							let source = new_any "bad statement" :: source in
							derived_types, source
						| None ->
							let item =
								begin match stmt with
								| Some stmt ->
									`named (def_p, name, `defined_generic_statement (formal_types, args, varargs, stmt), no_attributes)
								| None ->
									assert false
								end
							in
							let source = item :: source in
							derived_types, source
						end
					)
				| `any message ->
					let source = new_any message :: source in
					derived_types, source
				end
			with Assert_failure _ as e ->
				prerr_string (Printexc.to_string e);
				prerr_newline ();
				Printexc.print_backtrace stderr;
				flush stderr;
				begin match !redirected_error with
				| Some (r_ps, r_message) ->
					error r_ps r_message;
				| None ->
					()
				end;
				error (fst define) (assert_failed name);
				raise e
			end
		)
	);;
	
	let map
		(error: ranged_position -> string -> unit)
		(is_known_error: ranged_position -> string -> [> known_errors_of_define_analzer] -> bool)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(sources: (source_item list * extra_info) StringMap.t)
		(mapping_options: mapping_options)
		(defines: (define p) StringMap.t)
		: derived_types * (source_item list * extra_info) StringMap.t =
	(
		StringMap.fold (fun name define (derived_types, sources) ->
			let (((current_filename, _, _, _), _), _) = define in
			let current_source, current_info =
				StringMap.find_or ~default:empty_source current_filename sources
			in
			let derived_types, current_source = handle_define
				error
				is_known_error
				predefined_types
				derived_types
				namespace
				current_source
				mapping_options
				name
				define
			in
			let sources =
				StringMap.add current_filename (current_source, current_info) sources
			in
			derived_types, sources
		) defines (derived_types, sources)
	);;
	
end;;
