open C_literals;;
open C_semantics;;

module Finding
	(Literals: LiteralsType)
	(Semantics: SemanticsType
		with module Literals := Literals) =
struct
	open Semantics;;
	
	(* types *)
	
	let rec find_all_sized_array_in_source_item
		(rs : all_type list)
		(item: source_item)
		: all_type list =
	(
		begin match item with
		| `named (_, _, `defined_element_access (_, route), _) ->
			(* sized array as result type *)
			let t = tail_type_of_element_access route in
			begin match t with
			| `array (Some _, _) ->
				if List.memq t rs then rs else t :: rs
			| _ ->
				rs
			end
		| `named (_, _, `defined_alias item, _) ->
			find_all_sized_array_in_source_item rs (item :> Semantics.source_item)
		| _ ->
			rs
		end
	);;
	
	let find_all_sized_array_in_derived_type
		(rs : all_type list)
		(t: derived_type)
		: all_type list =
	(
		let rec process rs (t: all_type) = (
			begin match t with
			| `array (Some _, _) as t ->
				t :: rs
			| `volatile t ->
				process rs (t :> all_type)
			| `const t ->
				process rs (t :> all_type)
			| `named (_, _, `typedef t, _) ->
				process rs t
			| _ ->
				rs
			end
		) in
		begin match t with
		| `pointer t -> process rs (t :> all_type)
		| `block_pointer t -> process rs (t :> all_type)
		| `array (_, t) -> process rs (t :> all_type)
		| `restrict t -> process rs (t :> all_type)
		| `volatile t -> process rs (t :> all_type)
		| `const t -> process rs (t :> all_type)
		end
	);;
	
	let rec compare_types_by_structure (a: all_type) (b: all_type): bool = (
		begin match a with
		| `pointer a_t ->
			begin match b with
			| `pointer b_t ->
				compare_types_by_structure a_t b_t
			| _ ->
				false
			end
		| `array (a_n, a_t) ->
			begin match b with
			| `array (b_n, b_t) when a_n = b_n ->
				compare_types_by_structure (a_t :> all_type) (b_t :> all_type)
			| _ ->
				false
			end
		| `restrict a_t ->
			begin match b with
			| `restrict b_t ->
				compare_types_by_structure (a_t :> all_type) (b_t :> all_type)
			| _ ->
				false
			end
		| `volatile a_t ->
			begin match b with
			| `volatile b_t ->
				compare_types_by_structure (a_t :> all_type) (b_t :> all_type)
			| _ ->
				false
			end
		| `const a_t ->
			begin match b with
			| `const b_t ->
				compare_types_by_structure (a_t :> all_type) (b_t :> all_type)
			| _ ->
				false
			end
		| `named (_, a_name, (`opaque_enum | `enum _), _) ->
			begin match b with
			| `named (_, b_name, (`opaque_enum | `enum _), _) ->
				a_name = b_name
			| _ ->
				false
			end
		| `named (_, a_name, (`opaque_struct | `struct_type _), _) ->
			begin match b with
			| `named (_, b_name, (`opaque_struct | `struct_type _), _) ->
				a_name = b_name
			| _ ->
				false
			end
		| `named (_, a_name, (`opaque_union | `union _), _) ->
			begin match b with
			| `named (_, b_name, (`opaque_union | `union _), _) ->
				a_name = b_name
			| _ ->
				false
			end
		| _ ->
			a == b
		end
	);;
	
	let rec is_typedef (base_type: all_type) (t: all_type): typedef_type option = (
		begin match t with
		| `named (_, _, `typedef source_t, _) as td ->
			if source_t == base_type then Some td else
			is_typedef base_type source_t
		| _ ->
			None
		end
	);;
	
	let rec expand_typedef (f: typedef_type -> bool) (t: all_type): all_type = (
		begin match t with
		| `pointer t ->
			`pointer (expand_typedef f t)
		| `array (n, t) ->
			`array (n,
				match expand_typedef f (t :> all_type) with
				| #not_qualified_type as t -> t
				| _ -> assert false)
		| `restrict t ->
			`restrict (
				match expand_typedef f (t :> all_type) with
				| #pointer_type as t -> t
				| _ -> assert false)
		| `volatile t ->
			`volatile (
				match expand_typedef f (t :> all_type) with
				| #not_qualified_type as t -> t
				| _ -> assert false)
		| `const t ->
			`const (
				match expand_typedef f (t :> all_type) with
				| #not_const_type as t -> t
				| _ -> assert false)
		| `named (_, _, `typedef raw_t, _) as t ->
			if f t then raw_t else
			expand_typedef f raw_t
		| _ ->
			assert false
		end
	);;
	
	let recursive_fold_derived_types
		~(including_typedef: bool)
		(f: 'a -> derived_type -> 'a) (* argument is not unique type *)
		(a: 'a)
		(base_type: all_type)
		(derived_types: derived_type list)
		: 'a =
	(
		let rec loop (m: derived_type -> derived_type) a base_type xs = (
			begin match xs with
			| x :: xr ->
				let process (t: all_type): 'a = (
					let a =
						if t == base_type then (
							let a = f a (m x) in
							loop m a (x :> all_type) xr
						) else if including_typedef then (
							begin match is_typedef base_type t with
							| Some typedef ->
								let m t =
									m (
										match expand_typedef ((==) typedef) (t :> all_type) with
										| #derived_type as t -> t
										| _ -> assert false)
								in
								loop m a t derived_types
							| _ ->
								a
							end
						) else (
							a
						)
					in
					loop m a base_type xr
				) in
				begin match x with
				| `pointer t ->
					process t
				| `block_pointer t ->
					process (t :> all_type)
				| `array (_, t) ->
					process (t :> all_type)
				| `restrict t ->
					process (t :> all_type)
				| `volatile t ->
					process (t :> all_type)
				| `const t ->
					process (t :> all_type)
				end
			| [] ->
				a
			end
		) in
		loop (fun t -> t) a base_type derived_types
	);;
	
	(* expression *)
	
	let lvalue_referenced_in_statement (variable: variable): statement -> bool = (
		let expr_f expr = (
			begin match expr with
			| `ref_object (v, `lvalue), _ when v == (variable :> object_var with_name) -> true
			| _ -> false
			end
		) in
		exists_in_statement (fun _ -> false) expr_f
	);;
	
	let find_all_pointer_arithmetic_in_source_item
		(ptrdiff_t: all_type)
		(rs : (all_type * all_type) list)
		(item: source_item)
		: (all_type * all_type) list =
	(
		let add t1 t2 rs = (
			if List.exists (fun (u1, u2) -> u1 == t1 && u2 == t2) rs then (
				rs
			) else (
				(t1, t2) :: rs
			)
		) in
		let stmt_f rs _ = rs in
		let expr_f rs e = (
			begin match e with
			| `increment _, t
			| `decrement _, t
			| `post_increment _, t
			| `post_decrement _, t when is_pointer t ->
				add t ptrdiff_t (add ptrdiff_t t rs)
			| _ ->
				rs
			end
		) in
		begin match item with
		| `named (_, _, `function_definition (_, _, stmts), _) ->
			List.fold_left (fold_statement stmt_f expr_f) rs stmts
		| `named (_, _, `defined_expression expr, _) ->
			fold_expression stmt_f expr_f rs expr
		| `named (_, _, `defined_generic_expression (_, _, _, expr), _) ->
			fold_expression stmt_f expr_f rs expr
		| `named (_, _, `defined_generic_statement (_, _, _, stmt), _) ->
			fold_statement stmt_f expr_f rs stmt
		| _ ->
			rs
		end
	);;
	
	let find_all_cast_in_source_item
		(rs : (all_type * all_type) list)
		(item: source_item)
		: (all_type * all_type) list =
	(
		let add t1 t2 rs = (
			if List.exists (fun (u1, u2) -> u1 == t1 && u2 == t2) rs then (
				rs
			) else (
				(t1, t2) :: rs
			)
		) in
		let add_array (`array (_, e1) as t1) t2 rs = (
			if List.exists
				begin fun (u1, u2) ->
					match u1 with
					| `array (_, f1) when f1 == e1 && u2 == t2 -> true
					| _ -> false
				end
				rs
			then (
				rs
			) else (
				((t1 :> all_type), t2) :: rs
			)
		) in
		let stmt_f rs _ = rs in
		let expr_f rs (e: expression) = (
			begin match e with
			| `cast ((_, t1) as expr), t2
			| `explicit_conv ((_, t1) as expr), t2
			| `implicit_conv ((_, t1) as expr), t2 ->
				let t1a = resolve_typedef t1 ~stop_on_anonymous:true in
				let t2a = resolve_typedef t2 ~stop_on_anonymous:true in
				let t1 = resolve_typedef t1a in
				let t2 = resolve_typedef t2a in
				begin match t1, t2 with
				| #int_prec, `pointer `void when is_static_expression expr ->
					rs (* use System'To_Address *)
				| (`array ((Some _), _) as t1), (`pointer _) ->
					add_array t1 t2 rs
				| _, (`pointer _)
				| (`pointer _), _
					when not (is_generic_type t1) && not (is_generic_type t2)
				->
					add t1a t2a rs
				| _ ->
					rs
				end
			| _ ->
				rs
			end
		) in
		begin match item with
		| `named (_, _, `function_definition (_, _, stmts), _) ->
			List.fold_left (fold_statement stmt_f expr_f) rs stmts
		| `named (_, _, `defined_expression expr, _) ->
			fold_expression stmt_f expr_f rs expr
		| `named (_, _, `defined_generic_expression (_, _, _, expr), _) ->
			fold_expression stmt_f expr_f rs expr
		| `named (_, _, `defined_generic_statement (_, _, _, stmt), _) ->
			fold_statement stmt_f expr_f rs stmt
		| _ ->
			rs
		end
	);;
	
	let find_all_chars_literal_as_pointer_in_statement
		(rs: (literal_value * all_type) list)
		(stmt: statement)
		: (literal_value * all_type) list =
	(
		let stmt_f result (_: statement) = result in
		let expr_f result (expr: expression): (literal_value * all_type) list = (
			begin match expr with
			| `implicit_conv (`chars_literal _, _ as e), `pointer (`const `char)
			| `implicit_conv (`wchars_literal _, _ as e), `pointer (`const `wchar) ->
				e :: result
			| _ ->
				result
			end
		) in
		fold_statement stmt_f expr_f rs stmt
	);;
	
	let find_all_chars_literal_as_pointer_in_expression
		(rs: (literal_value * all_type) list)
		(expr: expression)
		: (literal_value * all_type) list =
	(
		find_all_chars_literal_as_pointer_in_statement rs (`expression expr)
	);;
	
	let find_all_assignment_in_statement
		(rs: any_assignment_expression list)
		(stmt: statement)
		: any_assignment_expression list =
	(
		let stmt_f (included, excluded as result) stmt = (
			let module L = struct type aaev = any_assignment_expression_var end in
			begin match stmt with
			| `expression (#L.aaev, _ as e) ->
				included, e :: excluded
			| `for_loop (Some (#L.aaev, _ as e1), _, Some (#L.aaev, _ as e2), _) ->
				included, e1 :: e2 :: excluded
			| `for_loop (Some (#L.aaev, _ as e), _, _, _) ->
				included, e :: excluded
			| `for_loop (_, _, Some (#L.aaev, _ as e), _) ->
				included, e :: excluded
			| _ ->
				result
			end
		) in
		let expr_f (included, excluded as result) expr = (
			begin match expr with
			| #any_assignment_expression_var, _ as e ->
				e :: included, excluded
			| _ ->
				result
			end
		) in
		let included, excluded = fold_statement stmt_f expr_f (rs, []) stmt in
		List.rev (List.filter (fun x -> not (List.memq x excluded)) included)
	);;
	
	let find_all_assignment_in_expression
		(rs: any_assignment_expression list)
		(expr: expression)
		: any_assignment_expression list =
	(
		find_all_assignment_in_statement rs (`expression expr)
	);;
	
	let find_all_conditional_in_statement
		(rs: conditional_expression list)
		(stmt: statement)
		: conditional_expression list =
	(
		let stmt_f result (_: statement) = result in
		let expr_f result expr = (
			begin match expr with
			| #conditional_expression_var, _ as e ->
				e :: result
			| _ ->
				result
			end
		) in
		fold_statement stmt_f expr_f rs stmt
	);;
	
	let find_all_conditional_in_expression
		(rs: conditional_expression list)
		(expr: expression)
		: conditional_expression list =
	(
		find_all_conditional_in_statement rs (`expression expr)
	);;
	
	let find_all_extension_statement_expression_in_statement
		(rs: statement_expression list)
		(stmt: statement)
		: statement_expression list =
	(
		let stmt_f result (_: statement) = result in
		let expr_f result expr = (
			begin match expr with
			| #statement_expression_var, _ as e ->
				e :: result
			| _ ->
				result
			end
		) in
		fold_statement stmt_f expr_f rs stmt
	);;
	
	let find_all_extension_statement_expression_in_expression
		(rs: statement_expression list)
		(expr: expression)
		: statement_expression list =
	(
		find_all_extension_statement_expression_in_statement rs (`expression expr)
	);;
	
	(* inline assembler *)
	
	let asm_exists_in_statement: statement -> bool =
		let stmt_f stmt = (
			begin match stmt with
			| `asm _ -> true
			| _ -> false
			end
		) in
		exists_in_statement stmt_f (fun _ -> false);;
	
	let asm_exists_in_source_item (item: source_item): bool = (
		begin match item with
		| `named (_, _, `function_definition (_, _, stmts), _) ->
			List.exists asm_exists_in_statement stmts
		| `named (_, _, `defined_generic_statement (_, _, _, stmt), _) ->
			asm_exists_in_statement stmt
		| _ ->
			false
		end
	);;
	
end;;
