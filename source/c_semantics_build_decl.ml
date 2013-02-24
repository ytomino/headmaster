open C_literals;;
open C_semantics;;
open C_semantics_build_type;;

module type DeclaringType = sig
	module Literals: LiteralsType;;
	module Semantics: SemanticsType
		with module Literals := Literals;;
	module Typing: TypingType
		with module Literals := Literals
		with module Semantics := Semantics;;
	
	(* attributes *)
	
	val attributes_of_alignment: Semantics.alignment -> Semantics.attributes;;
	
	(* built-in functions *)
	
	val ready_builtin_functions:
		Semantics.predefined_types ->
		Semantics.derived_types ->
		Semantics.namespace ->
		(string *
			[< Semantics.predefined_type | `pointer of [< Semantics.predefined_type | `const of [< Semantics.predefined_type]] | `size_t] list *
			[< Semantics.predefined_type | `pointer of [< Semantics.predefined_type | `const of [< Semantics.predefined_type]] | `size_t]) list ->
		Semantics.derived_types * Semantics.namespace * (Semantics.source_item list * Semantics.extra_info);;
	
	(* functions *)
	
	val is_function_conflicted:
		Semantics.function_item ->
		Semantics.namespace ->
		[`error | `same | `precedence of Semantics.named_item | `none];;
	
end;;

module Declaring
	(Literals: LiteralsType)
	(Semantics: SemanticsType
		with module Literals := Literals)
	(Typing: TypingType
		with module Literals := Literals
		with module Semantics := Semantics)
	: DeclaringType
		with module Literals := Literals
		with module Semantics := Semantics
		with module Typing := Typing =
struct
	open Semantics;;
	
	(* attributes *)
	
	let attributes_of_alignment (alignment: alignment): attributes = (
		if alignment = `default then no_attributes else
		{no_attributes with at_aligned = alignment}
	);;
	
	(* built-in functions *)
	
	let ready_builtin_functions
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(builtin: (string *
			[< predefined_type | `pointer of [< predefined_type | `const of [< predefined_type]] | `size_t] list *
			[< predefined_type | `pointer of [< predefined_type | `const of [< predefined_type]] | `size_t]) list)
		: derived_types * namespace * (source_item list * extra_info) =
	(
		let derived_types, namespace, builtin_source =
			List.fold_left (fun (derived_types, namespace, source) (name, args, ret) ->
				let type_map t predefined_types derived_types: all_type * derived_types = (
					begin match t with
					| `pointer (#predefined_type as target_t) ->
						let target_t = find_predefined_type target_t predefined_types in
						Typing.find_pointer_type target_t derived_types
					| `pointer (`const (#predefined_type as target_t)) ->
						let target_t = find_predefined_type target_t predefined_types in
						let const_type, derived_types = Typing.find_const_type target_t derived_types in
						Typing.find_pointer_type const_type derived_types
					| `size_t ->
						find_size_t predefined_types, derived_types
					| #predefined_type as t ->
						find_predefined_type t predefined_types, derived_types
					end
				) in
				let derived_types, args =
					List.fold_left (fun (derived_types, rs) arg ->
						let arg, derived_types = type_map arg predefined_types derived_types in
						let item = `named (builtin_position, "", `variable (arg, None), no_attributes) in
						derived_types, item :: rs
					) (derived_types, []) args
				in
				let args = List.rev args in
				let ret, derived_types = type_map ret predefined_types derived_types in
				let t = `function_type (`cdecl, args, `none, ret) in
				let item = `named (builtin_position, name, `function_forward (`builtin, t), no_attributes) in
				let namespace = {namespace with ns_namespace = StringMap.add name item namespace.ns_namespace} in
				let source = item :: source in
				derived_types, namespace, source
			) (derived_types, namespace, []) builtin
		in
		derived_types, namespace, (builtin_source, no_extra_info)
	);;
	
	(* functions *)
	
	let is_function_conflicted
		(item: function_item)
		(namespace: namespace)
		: [`error | `same | `precedence of named_item | `none] =
	(
		let id, `function_type prototype, alias =
			begin match item with
			| `named (_, id, `extern (t, alias), _) -> id, t, alias
			| `named (_, id, `function_forward (_, t), _) -> id, t, `none
			| `named (_, id, `function_definition (_, t, _), _) -> id, t, `none
			end
		in
		begin try
			let previous = StringMap.find id namespace.ns_namespace in
			begin match previous with
			| `named (_, _, `extern ((`function_type previous_prototype), _), _)
			| `named (_, _, `function_definition (`extern_inline, `function_type previous_prototype, _), _) as previous ->
				if Typing.prototype_ABI_compatibility ~dest:prototype ~source:previous_prototype = `just then (
					begin match previous with
					| `named (_, _, `extern (_, prev_alias), _) when prev_alias <> alias ->
						`precedence previous
					| _ ->
						`same (* no error when same prototype *)
					end
				) else (
					`error (* prototype mismatch *)
				)
			| _ ->
				`error
			end
		with Not_found ->
			`none
		end
	);;
	
end;;
