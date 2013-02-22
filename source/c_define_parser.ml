open C_lexical;;
open C_literals;;
open C_parser;;
open C_preprocessor;;
open C_syntax;;
open Position;;

let list_combination
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

type known_errors_of_define_parser = [known_errors_of_preprocessor
	| `unparsible_macro];;

module type DefineParserType = sig
	module Literals: LiteralsType;;
	module LexicalElement: LexicalElementType
		with module Literals := Literals;;
	module Preprocessor: PreprocessorType
		with module Literals := Literals
		with module LexicalElement := LexicalElement;;
	module Syntax: SyntaxType
		with module Literals := Literals;;
	
	type define = [
		| `operator of iso646_operator
		| `declaration_specifiers of Syntax.declaration_specifiers
		| `initializer_t of Syntax.initializer_t
		| `function_expr of (string Syntax.p * [`typedef | `value]) list * [`varargs | `none] * Syntax.expression
		| `function_stmt of (string Syntax.p * [`typedef | `value]) list * [`varargs | `none] * Syntax.statement
		| `any of string];;
	
	val parse_define:
		(ranged_position -> string -> unit) ->
		(ranged_position -> string -> [> known_errors_of_define_parser] -> bool) ->
		language ->
		TypedefSet.t ->
		Preprocessor.define_map ->
		Preprocessor.define_item ->
		define Syntax.p;;
	
	val map:
		(ranged_position -> string -> unit) ->
		(ranged_position -> string -> [> known_errors_of_define_parser] -> bool) ->
		language ->
		TypedefSet.t ->
		Preprocessor.define_map ->
		define Syntax.p StringMap.t;;
	
end;;

module DefineParser
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType
		with module Literals := Literals)
	(Preprocessor: PreprocessorType
		with module Literals := Literals
		with module LexicalElement := LexicalElement)
	(Syntax: SyntaxType
		with module Literals := Literals)
	(Parser: ParserType
		with module Literals := Literals
		with module LexicalElement := LexicalElement
		with module Syntax := Syntax)
	: DefineParserType
		with module Literals := Literals
		with module LexicalElement := LexicalElement
		with module Preprocessor := Preprocessor
		with module Syntax := Syntax =
struct
	
	(* error messages *)
	
	let failed_to_preprocess (s: string): string =
		"macro " ^ s ^ " is failed to preprocess.";;
	let failed_to_parse (s: string): string =
		"macro " ^ s ^ " is failed to parse.";;
	let function_macro_failed_to_parse (s: string): string =
		"function macro " ^ s ^ " is failed to parse.";;
	let assert_failed (s: string): string =
		"assert is failed to parsing macro " ^ s ^ ". (define-parser)";;
	
	(* additional parser *)
	
	type 'a p = 'a Syntax.p;;
	
	let parse_operator_option
		(xs: 'a Parser.in_t)
		: iso646_operator p option * 'a Parser.in_t =
	(
		begin match xs with
		| lazy (`cons (op_p, (#iso646_operator as op_e), xs)) ->
			Some (op_p, op_e), xs
		| _ ->
			None, xs
		end
	);;
	
	(* macro handling *)
	
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
				Some result
			with Not_found ->
				None
			end
		| _ ->
			None
		end
	);;
	
	(* define parser *)
	
	type define = [
		| `operator of iso646_operator
		| `declaration_specifiers of Syntax.declaration_specifiers
		| `initializer_t of Syntax.initializer_t
		| `function_expr of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.expression
		| `function_stmt of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.statement
		| `any of string];;
	
	let parse_define
		(error: ranged_position -> string -> unit)
		(is_known_error: ranged_position -> string -> [> known_errors_of_define_parser] -> bool)
		(lang: language)
		(typedefs: TypedefSet.t)
		(macros: Preprocessor.define_map)
		(macro: Preprocessor.define_item)
		: define p =
	(
		let rec process
			(macros: Preprocessor.define_map)
			(macro_name: string)
			(macro: Preprocessor.define_item)
			: define p =
		(
			let ps = macro.Preprocessor.df_position in
			if is_known_error ps macro.Preprocessor.df_name `unparsible_macro then ps, (`any "unparsible") else
			let other_macros = StringMap.remove macro.Preprocessor.df_name macros in
			begin match is_alias_of_other_macro macros macro macro.Preprocessor.df_contents with
			| Some target when target != macro ->
				(* deriving *)
				let _, result = process other_macros macro_name target in
				ps, result
			| _ ->
				let xs = macro.Preprocessor.df_contents in
				let has_arguments = macro.Preprocessor.df_has_arguments in
				if has_arguments && has_sharps xs then ps, (`any "has # or ##") else (* exclude macros having # or ## *)
				let has_error = ref false in
				let dummy_error _ _ = has_error := true in
				let xs = lazy (Preprocessor.preprocess
					dummy_error
					is_known_error
					lang
					(fun ~current ?next _ _ _ ->
						ignore current;
						ignore next;
						has_error := true;
						(LazyList.find_nil xs :> Preprocessor.in_prim))
					`top_level
					other_macros
					StringMap.empty
					xs)
				in
				let (_: [`nil of ranged_position * Preprocessor.define_map]) = LazyList.find_nil xs in (* error check *)
				begin try
					if !has_error then (
						error ps (failed_to_preprocess macro_name);
						ps, (`any "preprocessor error")
					) else if LazyList.is_empty xs then (
						ps, (`any "empty")
					) else if has_arguments then (
						let varargs = if macro.Preprocessor.df_varargs then `varargs else `none in
						let last_f result =
							error ps (function_macro_failed_to_parse macro_name);
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
									if is_type = `typedef then TypedefSet.add k v else v
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
						| Some target when target != macro ->
							(* deriving *)
							let _, result = process other_macros macro_name target in
							ps, result
						| Some _ ->
							(* self *)
							ps, `any "repeating itself"
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
								error ps (failed_to_parse macro_name);
								ps, `any "parser error"
							)
						end
					)
				with Assert_failure _ as e ->
					prerr_string (Printexc.to_string e);
					prerr_newline ();
					Printexc.print_backtrace stderr;
					flush stderr;
					error ps (assert_failed macro_name);
					raise e
				end
			end
		) in
		process macros macro.Preprocessor.df_name macro
	);;
	
	let map
		(error: ranged_position -> string -> unit)
		(is_known_error: ranged_position -> string -> [> known_errors_of_define_parser] -> bool)
		(lang: language)
		(typedefs: TypedefSet.t)
		(items: Preprocessor.define_map)
		: define p StringMap.t =
	(
		StringMap.map (parse_define error is_known_error lang typedefs items) items
	);;
	
end;;
