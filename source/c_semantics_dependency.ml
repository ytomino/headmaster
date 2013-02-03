open C_literals;;
open C_semantics;;

let list_unionq (xs: 'a list) (ys: 'a list): 'a list = (
	if ys = [] then xs else
	let rec loop xs ys = (
		begin match xs with
		| [] -> ys
		| x :: xr ->
			let rs = (if List.memq x ys then ys else x :: ys) in
			loop xr rs
		end
	) in
	loop xs ys
);;

module Dependency
	(Literals: LiteralsType)
	(Semantics: SemanticsType
		with module Literals := Literals) =
struct
	open Semantics;;
	
	let rec dummy_of_alias (_: named_item): named_item list = (
		[]
	) and dummy_of_argument (t: all_type): named_item list = (
		of_item (t :> all_item)
	) and of_item (x: all_item): named_item list = (
		begin match x with
		| #named_item as x ->
			x :: []
		| _ ->
			dependents ~of_alias:dummy_of_alias ~of_argument:dummy_of_argument x
		end
	) and of_typedef (x: typedef_var): named_item list = (
		let `typedef t = x in
		of_item (t :> all_item)
	) and of_struct (x: struct_or_union_type_var): named_item list = (
		begin match x with
		| `struct_type (_, items) | `union items ->
			List.fold_left (fun rs (_, t, _, _) ->
				list_unionq (of_item (t: all_type :> all_item)) rs
			) [] items
		end
	) and of_prototype ~(of_argument: all_type -> named_item list) (x: prototype): named_item list = (
		let _, args, _, ret = x in
		List.fold_left (fun rs arg ->
			let `named (_, _, `variable (arg_t, _), _) = arg in
			list_unionq (of_argument arg_t) rs
		) (of_item (ret :> all_item)) args
	) and of_expr (expr: expression): named_item list = (
		fold_expression
			(fun rs _ -> rs)
			(fun rs expr ->
				begin match expr with
				| `enumerator item, _ ->
					(item :> named_item) :: rs
				| `ref_function (item: function_item), _ ->
					(item :> named_item) :: rs
				| _ ->
					rs
				end)
			[]
			expr
	) and dependents
		~(of_alias: named_item -> named_item list)
		~(of_argument: all_type -> named_item list)
		(item: all_item)
		: named_item list =
	(
		begin match item with
		(* predefined types *)
		| #predefined_type ->
			[]
		(* derived types *)
		| `pointer t ->
			of_item (t :> all_item)
		| `block_pointer t ->
			of_item (t :> all_item)
		| `array (_, t) ->
			of_item (t :> all_item)
		| `restrict t ->
			of_item (t :> all_item)
		| `volatile t ->
			of_item (t :> all_item)
		| `const t ->
			of_item (t :> all_item)
		(* anonymous types *)
		| `anonymous (_, `enum _) ->
			[]
		| `anonymous (_, (`struct_type _ as sou)) ->
			of_struct sou
		| `anonymous (_, (`union _ as sou)) ->
			of_struct sou
		| `function_type prototype ->
			of_prototype ~of_argument prototype
		(* named types *)
		| `named (_, _, `opaque_enum, _) ->
			[]
		| `named (_, _, `enum _, _) ->
			[]
		| `named (_, _, `opaque_struct, _) ->
			[]
		| `named (_, _, (`struct_type _ as sou), _) ->
			of_struct sou
		| `named (_, _, `opaque_union, _) ->
			[]
		| `named (_, _, (`union _ as sou), _) ->
			of_struct sou
		| `named (_, _, (`typedef _ as typedef), _) ->
			of_typedef typedef
		| `named (_, _, `generic_type, _) ->
			[]
		(* other named items *)
		| `named (_, _, `enum_element _, _) ->
			[]
		| `named (_, _, `extern ((`function_type prototype), _), _) ->
			of_prototype ~of_argument prototype
		| `named (_, _, `extern (t, _), _) ->
			of_item (t :> all_item)
		| `named (_, _, `variable (t, _), _) ->
			of_item (t :> all_item)
		| `named (_, _, `function_forward (_, (`function_type prototype)), _) ->
			of_prototype ~of_argument prototype
		| `named (_, _, `function_definition (_, (`function_type prototype), _), _) ->
			of_prototype ~of_argument prototype
		| `named (_, _, `defined_operator _, _) ->
			[]
		| `named (_, _, `defined_attributes, _) ->
			[]
		| `named (_, _, `defined_storage_class _, _) ->
			[]
		| `named (_, _, `defined_type_specifier _, _) ->
			[]
		| `named (_, _, `defined_type_qualifier _, _) ->
			[]
		| `named (_, _, `defined_typedef t, _) ->
			of_item (t :> all_item)
		| `named (_, _, `defined_opaque_type t, _) ->
			of_item (t :> all_item)
		| `named (_, _, `defined_element_access _, _) ->
			[]
		| `named (_, _, `defined_expression expr, _) ->
			let r = of_expr expr in
			begin match expr with
			| `ref_function f, _ ->
				let p =
					begin match f with
					| `named (_, _, `extern ((`function_type prototype), _), _)
					| `named (_, _, `function_forward (_, (`function_type prototype)), _)
					| `named (_, _, `function_definition (_, (`function_type prototype), _), _) ->
						of_prototype ~of_argument prototype
					end
				in
				list_unionq p r
			| _ ->
				r
			end
		| `named (_, _, `defined_generic_expression _, _) ->
			[]
		| `named (_, _, `defined_generic_statement _, _) ->
			[]
		| `named (_, _, `defined_any _, _) ->
			[]
		| `named (_, _, `defined_alias item, _) ->
			item :: of_alias item
		| `named (_, _, `generic_value _, _) ->
			[]
		| `anonymous_alias (_, `function_type prototype) ->
			of_prototype ~of_argument prototype
		| `include_point _ ->
			[]
		end
	);;
	
end;;
