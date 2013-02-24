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
