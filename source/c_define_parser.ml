open C_define_parser_errors;;
open C_lexical;;
open C_parser;;
open C_preprocessor;;
open C_syntax;;
open Position;;
open Value;;

let rec list_combination
	(f: ('b -> 'b) -> ('a * bool) list -> 'b -> 'b)
	(xs: 'a list)
	(last: 'b -> 'b)
	(start: 'b)
	: 'b =
(
	let rec loop xs (cont: 'b -> 'b) ys r = (
		begin match xs with
		| [] ->
			f cont (List.rev ys) r
		| x :: xr ->
			loop xr (loop xr cont ((x, true) :: ys)) ((x, false) :: ys) r
		end
	) in
	loop xs last [] start
);;

module DefineParser
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType (Literals).S)
	(Preprocessor: PreprocessorType (Literals) (LexicalElement).S)
	(Syntax: SyntaxType (Literals).S) =
struct
	module Parser = Parser (Literals) (LexicalElement) (Syntax);;
	
	type 'a p = 'a Syntax.p;;
	
	let rec has_sharps (xs: 'a Parser.in_t): bool = (
		begin match xs with
		| lazy (`cons (_, it, xs)) ->
			begin match it with
			| `sharp | `d_sharp ->
				true
			| _ ->
				has_sharps xs
			end
		| lazy (`nil _) ->
			false
		end
	);;
	
	let parse_operator_option
		(xs: 'a Parser.in_t)
		: operator p option * 'a Parser.in_t =
	(
		begin match xs with
		| lazy (`cons (op_p, (#operator as op_e), xs)) ->
			Some (op_p, op_e), xs
		| _ ->
			None, xs
		end
	);;
	
	type define = [
		| `operator of operator
		| `declaration_specifiers of Syntax.declaration_specifiers
		| `initializer_t of Syntax.initializer_t
		| `function_expr of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.expression
		| `function_stmt of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.statement
		| `any of string];;
	
	let is_alias_of_other_macro
		(macros: Preprocessor.define_map)
		(macro: Preprocessor.define_item)
		(contents: (ranged_position, LexicalElement.t, 'c) LazyList.t)
		: Preprocessor.define_item option =
	(
		if macro.Preprocessor.df_has_arguments then None else
		begin match contents with
		| lazy (`cons (_, `ident target_name, lazy (`nil _))) ->
			begin try
				let result = StringMap.find target_name macros in
				if result == macro then None else (* #define A A *)
				Some result
			with Not_found ->
				None
			end
		| _ ->
			None
		end
	);;
	
	let rec parse_define
		?(name: string option)
		(error: ranged_position -> string -> unit)
		(lang: language)
		(typedefs: Parser.typedef_set)
		(macros: Preprocessor.define_map)
		(macro: Preprocessor.define_item)
		: define p =
	(
		let ps = macro.Preprocessor.df_position in
		if is_known_define_parser_error ps macro.Preprocessor.df_name then ps, (`any "unparsible") else
		let name =
			begin match name with
			| Some name ->
				name
			| None ->
				macro.Preprocessor.df_name
			end
		in
		begin match is_alias_of_other_macro macros macro macro.Preprocessor.df_contents with
		| Some target ->
			(* deriving *)
			let _, result = parse_define
				~name
				error
				lang
				typedefs
				(StringMap.remove macro.Preprocessor.df_name macros)
				target
			in
			ps, result
		| None ->
			let xs = macro.Preprocessor.df_contents in
			let has_arguments = macro.Preprocessor.df_has_arguments in
			if has_arguments && has_sharps xs then ps, (`any "has # or ##") else (* exclude macros having # or ## *)
			let has_error = ref false in
			let dummy_error _ _ = has_error := true in
			let xs = lazy (Preprocessor.preprocess
				dummy_error
				lang
				(fun ~current ?next _ _ _ ->
					ignore current;
					ignore next;
					has_error := true;
					(LazyList.find_nil xs :> Preprocessor.in_prim))
				false
				(StringMap.remove macro.Preprocessor.df_name macros)
				StringMap.empty
				xs)
			in
			let (_: [`nil of ranged_position * Preprocessor.define_map]) = LazyList.find_nil xs in (* error check *)
			begin try
				if !has_error then (
					error ps ("macro " ^ name ^ " could not be parsed. (preprocessor error)");
					ps, (`any "preprocessor error")
				) else if LazyList.is_empty xs then (
					ps, (`any "empty")
				) else if has_arguments then (
					let varargs = if macro.Preprocessor.df_varargs then `varargs else `none in
					let last_f result =
						error ps ("function-macro " ^ name ^ " could not be parsed.");
						result
					in
					list_combination (fun cont args dummy_result ->
						let args =
							List.map (fun (v, is_type) ->
								v, (if is_type then `typedef else `value)
							) args
						in
						let typedefs =
							List.fold_right (fun ((_, k), is_type) v ->
								if is_type = `typedef then StringSet.add k v else v
							) args typedefs
						in
						let expr, xr = has_error := false; Parser.parse_expression_or_error dummy_error lang typedefs xs in
						if not !has_error && LazyList.is_empty xr && expr <> `error then (
							begin match expr with
							| `some expr -> ps, `function_expr (args, varargs, snd expr)
							| `error -> assert false
							end
						) else
						let stmt, xr = has_error := false; Parser.parse_statement_or_error ~semicolon_need:false dummy_error lang typedefs xs in
						if not !has_error && LazyList.is_empty xr && stmt <> `error then (
							begin match stmt with
							| `some stmt -> ps, `function_stmt (args, varargs, snd stmt)
							| `error -> assert false
							end
						) else (
							cont dummy_result
						)
					) macro.Preprocessor.df_args last_f (ps, `any "parser error")
				) else (
					begin match is_alias_of_other_macro macros macro xs with
					| Some target ->
						(* deriving *)
						let _, result = parse_define
							~name
							error
							lang
							typedefs
							(StringMap.remove macro.Preprocessor.df_name macros)
							target
						in
						ps, result
					| None ->
						let op, xr = has_error := false; parse_operator_option xs in
						if not !has_error && LazyList.is_empty xr && op <> None then (
							begin match op with
							| Some op -> ps, `operator (snd op)
							| None -> assert false
							end
						) else
						let spec, xr = has_error := false; Parser.parse_declaration_specifiers_option dummy_error lang typedefs xs in
						if not !has_error && LazyList.is_empty xr && spec <> `none then (
							begin match spec with
							| `some spec -> ps, `declaration_specifiers (snd spec)
							| `none -> assert false
							end
						) else
						let expr, xr = has_error := false; Parser.parse_initializer_or_error dummy_error lang typedefs xs in
						if not !has_error && LazyList.is_empty xr && expr <> `error then (
							begin match expr with
							| `some expr -> ps, `initializer_t (snd expr)
							| `error -> assert false
							end
						) else (
							error ps ("macro " ^ name ^ " could not be parsed.");
							ps, `any "parser error"
						)
					end
				)
			with Assert_failure _ as e ->
				prerr_string (Printexc.to_string e);
				prerr_newline ();
				Printexc.print_backtrace stderr;
				flush stderr;
				error ps ("macro " ^ name ^ " could not be parsed. (assert failure)");
				raise e
			end
		end
	);;
	
	let map
		(error: ranged_position -> string -> unit)
		(lang: language)
		(typedefs: Parser.typedef_set)
		(items: Preprocessor.define_map)
		: define p StringMap.t =
	(
		StringMap.map (parse_define error lang typedefs items) items
	);;
	
end;;
