open C_semantics;;
open Value;;

module Finding
	(Literals: LiteralsType)
	(Semantics: SemanticsType (Literals).S) =
struct
	open Semantics;;
	
	(* expression *)
	
	let lvalue_referenced_in_statement (variable: variable): statement -> bool =
		let expr_f expr = (
			begin match expr with
			| `ref_object (v, `lvalue), _ when v == (variable :> object_var with_name) -> true
			| _ -> false
			end
		) in
		exists_in_statement (fun _ -> false) expr_f;;
	
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
		let stmt_f rs _ = rs in
		let expr_f rs (e: expression) = (
			begin match e with
			| `cast ((_, t1) as expr), t2 ->
				let t1 = resolve_typedef t1 in
				let t2 = resolve_typedef t2 in
				begin match t1, t2 with
				| #int_prec, `pointer `void when is_static_expression expr ->
					rs (* use System'To_Address *)
				| _, (`pointer _)
				| (`pointer _), _
					when not (is_generic_type t1) && not (is_generic_type t2)
				->
					add t1 t2 rs
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
