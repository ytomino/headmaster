open C_filename;;
open C_lexical;;
open C_lexical_scanner;;
open C_literals;;
open Position;;

module StringMap = Map.Make (String);;

type known_errors_of_preprocessor = [
	| `undefined_macro
	| `redefine_compiler_macro
	| `redefine_extended_word
	| `push_defined_macro];;

module type PreprocessorType = sig
	module Literals: LiteralsType;;
	module LexicalElement: LexicalElementType
		with module Literals := Literals;;
	
	type include_from = [`user | `system];;
	
	type in_t = (ranged_position, LexicalElement.t, unit) LazyList.t
	and in_prim = (ranged_position, LexicalElement.t, unit) LazyList.prim
	and out_t = (ranged_position, LexicalElement.t, define_map) LazyList.t
	and out_prim = (ranged_position, LexicalElement.t, define_map) LazyList.prim
	and define_map = define_item StringMap.t
	and define_item = private {
		df_name: string;
		df_position: ranged_position;
		df_has_arguments: bool;
		df_args: (ranged_position * string) list;
		df_varargs: bool;
		df_contents: in_t};;
	
	type macro_argument_map = macro_argument_item StringMap.t
	and macro_argument_item = ((ranged_position * LexicalElement.t) option * ranged_position * concatable_element) option * out_t
	and concatable_element = [reserved_word
		| `ident of string
		| `numeric_literal of string * LexicalElement.numeric_literal];;
	
	type state_t = [`top_level | `in_ifdef of int | `in_macro_expr];;
	
	val preprocess:
		(ranged_position -> string -> unit) ->
		(ranged_position -> string -> [> known_errors_of_preprocessor] -> bool) ->
		language ->
		(current:string -> ?next:bool -> include_from -> string -> (ranged_position -> in_prim) -> in_prim) ->
		state_t ->
		define_map ->
		macro_argument_map ->
		in_t ->
		out_prim;;
	
end;;

module Preprocessor
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType
		with module Literals := Literals)
	(NumericScanner: NumericScannerType
		with module Literals := Literals
		with module LexicalElement := LexicalElement)
	: PreprocessorType
		with module Literals := Literals
		with module LexicalElement := LexicalElement =
struct
	open Literals;;
	
	(* error messages *)
	
	let unexpected_extra_tokens: string =
		"extra token(s) exists. (preprocessor)";;
	let unexpected_elif: string =
		"#elif without #if.";;
	let unexpected_else: string =
		"#else without #if.";;
	let unexpected_endif: string =
		"#endif without #if.";;
	let endif_is_missing: string =
		"#endif is missing.";;
	let parenthesis_is_missing: string =
		"\")\" is missing. (preprocessor)";;
	let term_is_missing: string =
		"term is missing. (preprocessor)";;
	let colon_is_missing: string =
		"\":\" is missing. (preprocessor)";;
	let macro_name_is_missing_for_define: string =
		"macro name is missing for #define";;
	let macro_name_is_missing_for_undef: string =
		"macro name is missing for #undef";;
	let macro_name_is_missing_for_ifdef: string =
		"macro name is missing for #ifdef";;
	let macro_name_is_missing_for_ifndef: string =
		"macro name is missing for #ifndef";;
	let redefined_extended_word (s: string): string =
		"extended keyword " ^ s ^ " is re-defined.";;
	let redefined_compiler_macro (s: string): string =
		"compiler macro " ^ s ^ " is re-defined.";;
	let vanished_macro (s: string): string =
		"macro " ^ s ^ " is vanished.";;
	let redefined_macro (s: string): string =
		"macro " ^ s ^ " is re-defined.";;
	let defined_macro_is_pushed (s: string): string =
		s ^ " is defined, and pushed. (not supported)";;
	let undefined_macro (s: string): string =
		s ^ " is undefined. (preprocessor)";;
	let too_many_arguments (s: string): string =
		"too many arguments for " ^ s ^ ".";;
	let too_few_arguments (s: string): string =
		"too few arguments for " ^ s ^ ".";;
	let one_identifier_is_required_for_shap: string =
		"one identifier_is_required for #.";;
	let two_identifiers_are_required_for_d_shap: string =
		"two identifiers are required for ##.";;
	let illegal_concatenated_token (s: string): string =
		"concatenated token \"" ^ s ^ "\" is illegal.";;
	let header_file_is_not_found (s: string): string =
		s ^ " is not found.";;
	let error_message (s: string): string =
		"#error " ^ s;;
	let warning_message (s: string): string =
		"#warning " ^ s;;
	let pragma_syntax_error (s: string): string =
		"#pragma " ^ s ^ " syntax error.";;
	let defined_syntax_error: string =
		"defined expression requires single name of macro.";;
	
	(* literals *)
	
	let the_one = `numeric_literal (
		"1",
		`int_literal (`signed_int, Integer.one));;
	
	let leinteger_of_bool (n: bool): Integer.t = (
		if n then (
			Integer.one
		) else (
			Integer.zero
		)
	);;
	
	let bool_of_leinteger (n: Integer.t): bool = (
		Integer.compare n Integer.zero <> 0
	);;
	
	let __STDC__ = the_one;;
	let __STDC_VERSION__ = `numeric_literal (
		"199901L",
		`int_literal (`signed_long, Integer.of_int 199901));;
	
	(* types *)
	
	type include_from = [`user | `system];;
	
	type in_t = (ranged_position, LexicalElement.t, unit) LazyList.t
	and in_prim = (ranged_position, LexicalElement.t, unit) LazyList.prim
	and out_t = (ranged_position, LexicalElement.t, define_map) LazyList.t
	and out_prim = (ranged_position, LexicalElement.t, define_map) LazyList.prim
	and define_map = define_item StringMap.t
	and define_item = {
		df_name: string;
		df_position: ranged_position;
		df_has_arguments: bool;
		df_args: (ranged_position * string) list;
		df_varargs: bool;
		df_contents: in_t};;
	
	type macro_argument_map = macro_argument_item StringMap.t
	and macro_argument_item = ((ranged_position * LexicalElement.t) option * ranged_position * concatable_element) option * out_t
	and concatable_element = [reserved_word
		| `ident of string
		| `numeric_literal of string * LexicalElement.numeric_literal];;
	
	(* define_item *)
	
	let define_item_eq (left: define_item) (right: define_item): bool = (
		let rec equals_args (left: (ranged_position * string) list) (right: (ranged_position * string) list): bool = (
			begin match left with
			| (_, left_e) :: left_r ->
				begin match right with
				| (_, right_e) :: right_r ->
					left_e = right_e && equals_args left_r right_r
				| _ ->
					false
				end
			| [] ->
				begin match right with
				| [] ->
					true
				| _ ->
					false
				end
			end
		) in
		let rec equals_in (left: in_t) (right: in_t): bool = (
			begin match left with
			| lazy (`cons (_, left_e, left_r)) ->
				begin match right with
				| lazy (`cons (_, right_e, right_r)) ->
					left_e = right_e && equals_in left_r right_r
				| _ ->
					false
				end
			| lazy (`nil _) ->
				begin match right with
				| lazy (`nil _) ->
					true
				| _ ->
					false
				end
			end
		) in
		left.df_has_arguments = right.df_has_arguments
		&& equals_args left.df_args right.df_args
		&& left.df_varargs = right.df_varargs
		&& equals_in left.df_contents right.df_contents
	);;
	
	(* line *)
	
	let rec skip_line (xs: in_t): in_t = (
		begin match xs with
		| lazy (`cons (_, token, xs)) ->
			if token = `end_of_line then (
				xs
			) else (
				skip_line xs
			)
		| lazy (`nil _) as eof ->
			eof
		end
	);;
	
	let rec take_line (xs: in_t): in_t * in_t = (
		begin match xs with
		| lazy (`cons (ps, token, xs)) ->
			if token = `end_of_line then (
				lazy (`nil (ps, ())), xs
			) else (
				let cs, xs = take_line xs in
				lazy (`cons (ps, token, cs)), xs
			)
		| lazy (`nil _) as eof ->
			eof, eof
		end
	);;
	
	let take_end_of_line
		(error: ranged_position -> string -> unit)
		(xs: in_t)
		: in_t =
	(
		begin match xs with
		| lazy (`cons (_, `end_of_line, xs)) ->
			xs
		| lazy (`cons (ps, _, _)) | lazy (`nil (ps, _)) ->
			error ps unexpected_extra_tokens;
			xs
		end
	);;
	
	(* ifdef *)
	
	let rec skip_structure_until
		(f: LexicalElement.t -> bool)
		(level: int)
		(xs: in_t)
		: in_t =
	(
		begin match xs with
		| lazy (`cons (_, (`sharp_IF | `sharp_IFDEF | `sharp_IFNDEF), xs)) ->
			skip_structure_until f (level + 1) xs
		| lazy (`cons (_, `sharp_ENDIF, xs)) when level > 0 ->
			skip_structure_until f (level - 1) xs
		| lazy (`cons (_, token, xs)) when level > 0 || not (f token) ->
			skip_structure_until f level xs
		| _ ->
			xs
		end
	);;
	
	let is_endif (x: LexicalElement.t): bool = (
		x = `sharp_ENDIF
	);;
	
	let is_else_or_endif (x: LexicalElement.t): bool = (
		begin match x with
		| `sharp_ELIF | `sharp_ELSE | `sharp_ENDIF ->
			true
		| _ ->
			false
		end
	);;
	
	(* expression *)
	
	let rec take_assignment_expression (level: int) (xs: in_t): in_t * in_t = (
		begin match xs with
		| lazy (`cons (ps, `l_paren, xs)) ->
			let cs, xs = take_assignment_expression (level + 1) xs in
			lazy (`cons (ps, `l_paren, cs)), xs
		| lazy (`cons (ps, `r_paren, xs)) when level > 0 ->
			let cs, xs = take_assignment_expression (level - 1) xs in
			lazy (`cons (ps, `r_paren, cs)), xs
		| lazy (`cons (ps, (`comma | `r_paren), _)) when level = 0 ->
			lazy (`nil (ps, ())), xs
		| lazy (`cons (ps, token, xs)) ->
			let cs, xs = take_assignment_expression level xs in
			lazy (`cons (ps, token, cs)), xs
		| lazy (`nil _) as eof ->
			eof, eof
		end
	);;
	
	let calc_expr
		(error: ranged_position -> string -> unit)
		(is_known_error: ranged_position -> string -> [> known_errors_of_preprocessor] -> bool)
		(xs: out_t)
		: Integer.t =
	(
		let rec calc_term (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			begin match xs with
			| lazy (`cons (_, `l_paren, xs)) ->
				let value, xs = calc_cond shortcircuit xs in
				begin match xs with
				| lazy (`cons (_, `r_paren, xs)) ->
					value, xs
				| lazy (`cons (ps, _, _) | `nil (ps, _)) ->
					error ps parenthesis_is_missing;
					value, xs
				end
			| lazy (`cons (_, `exclamation, xs)) ->
				let value, xs = calc_term shortcircuit xs in
				let value' = leinteger_of_bool (not (bool_of_leinteger value)) in
				value', xs
			| lazy (`cons (_, `plus, xs)) ->
				let value, xs = calc_term shortcircuit xs in
				value, xs
			| lazy (`cons (_, `minus, xs)) ->
				let value, xs = calc_term shortcircuit xs in
				let value' = Integer.neg value in
				value', xs
			| lazy (`cons (_, `numeric_literal (_, `int_literal (_, value)), xs)) ->
				value, xs
			| lazy (`cons (_, `char_literal value, xs)) ->
				let x =
					(* treat char as signed int *)
					let c = int_of_char value in
					if c < 0x80 then c else c lor (-1 lxor 0xff)
				in
				Integer.of_int x, xs
			| lazy (`cons (ps, `ident name, xs)) ->
				if not shortcircuit && not (is_known_error ps name `undefined_macro) then (
					error ps (undefined_macro name)
				);
				Integer.zero, xs
			| lazy (`cons (ps, _, _) | `nil (ps, _)) ->
				error ps term_is_missing;
				Integer.zero, xs
			end
		) and calc_sum (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let value1, xs as result = calc_term shortcircuit xs in
			begin match xs with
			| lazy (`cons (_, `plus, xs)) ->
				let value2, xs = calc_term shortcircuit xs in
				(Integer.add value1 value2), xs
			| lazy (`cons (_, `minus, xs)) ->
				let value2, xs = calc_term shortcircuit xs in
				(Integer.sub value1 value2), xs
			| _ ->
				result
			end
		) and calc_shift (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let value1, xs as result = calc_sum shortcircuit xs in
			begin match xs with
			| lazy (`cons (_, `l_shift, xs)) ->
				let value2, xs = calc_sum shortcircuit xs in
				(Integer.shift_left value1 (Integer.to_int value2)), xs
			| lazy (`cons (_, `r_shift, xs)) ->
				let value2, xs = calc_sum shortcircuit xs in
				(Integer.shift_right value1 (Integer.to_int value2)), xs
			| _ ->
				result
			end
		) and calc_relation (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let value1, xs as result = calc_shift shortcircuit xs in
			begin match xs with
			| lazy (`cons (_, `eq, xs)) ->
				let value2, xs = calc_shift shortcircuit xs in
				let r = leinteger_of_bool (Integer.compare value1 value2 = 0) in
				r, xs
			| lazy (`cons (_, `ne, xs)) ->
				let value2, xs = calc_shift shortcircuit xs in
				let r = leinteger_of_bool (Integer.compare value1 value2 <> 0) in
				r, xs
			| lazy (`cons (_, `lt, xs)) ->
				let value2, xs = calc_shift shortcircuit xs in
				let r = leinteger_of_bool (Integer.compare value1 value2 < 0) in
				r, xs
			| lazy (`cons (_, `le, xs)) ->
				let value2, xs = calc_shift shortcircuit xs in
				let r = leinteger_of_bool (Integer.compare value1 value2 <= 0) in
				r, xs
			| lazy (`cons (_, `gt, xs)) ->
				let value2, xs = calc_shift shortcircuit xs in
				let r = leinteger_of_bool (Integer.compare value1 value2 > 0) in
				r, xs
			| lazy (`cons (_, `ge, xs)) ->
				let value2, xs = calc_shift shortcircuit xs in
				let r = leinteger_of_bool (Integer.compare value1 value2 >= 0) in
				r, xs
			| _ ->
				result
			end
		) and calc_bit_and (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let value1, xs as result = calc_relation shortcircuit xs in
			begin match xs with
			| lazy (`cons (_, `ampersand, xs)) ->
				let value2, xs = calc_relation shortcircuit xs in
				let r = Integer.logand value1 value2 in
				r, xs
			| _ ->
				result
			end
		) and calc_and_then (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let value1, xs as result = calc_bit_and shortcircuit xs in
			begin match xs with
			| lazy (`cons (_, `and_then, xs)) ->
				let b1 = bool_of_leinteger value1 in
				let value2, xs = calc_and_then (shortcircuit || not b1) xs in
				let r = leinteger_of_bool (b1 && bool_of_leinteger value2) in
				r, xs
			| _ ->
				result
			end
		) and calc_or_else (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let value1, xs as result = calc_and_then shortcircuit xs in
			begin match xs with
			| lazy (`cons (_, `or_else, xs)) ->
				let b1 = bool_of_leinteger value1 in
				let value2, xs = calc_or_else (shortcircuit || b1) xs in
				let r = leinteger_of_bool (b1 || bool_of_leinteger value2) in
				r, xs
			| _ ->
				result
			end
		) and calc_cond (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let value_c, xs as result = calc_or_else shortcircuit xs in
			begin match xs with
			| lazy (`cons (_, `question, xs)) ->
				let cond = bool_of_leinteger value_c in
				let value1, xs = calc_or_else (shortcircuit || not cond) xs in
				begin match xs with
				| lazy (`cons (_, `colon, xs)) ->
					let value2, xs = calc_or_else (shortcircuit || cond) xs in
					(if cond then value1 else value2), xs
				| lazy (`cons (ps, _, _) | `nil (ps, _)) ->
					error ps colon_is_missing;
					(if cond then value1 else Integer.zero), xs
				end
			| _ ->
				result
			end
		) in
		let result, xs = calc_cond false xs in
		begin match xs with
		| lazy (`cons (ps, _, _)) ->
			error ps unexpected_extra_tokens;
		| lazy (`nil _) ->
			()
		end;
		result
	);;
	
	type comma_token = ranged_position * [`comma];;
	
	let take_arguments
		(error: ranged_position -> string -> unit)
		(xs: in_t)
		: (in_t * comma_token option) list * in_t =
	(
		let rec loop xs rs =
			begin match xs with
			| lazy (`cons (ps, `r_paren, xs)) ->
				let rs = (lazy (`nil (ps, ())), None) :: rs in
				List.rev rs, xs (* MACRO(..., EMPTY) *)
			| _ ->
				let arg, xs = take_assignment_expression 0 xs in
				begin match xs with
				| lazy (`cons (comma_p, (`comma as comma_e), xs)) ->
					let rs = (arg, Some (comma_p, comma_e)) :: rs in
					loop xs rs
				| lazy (`cons (_, `r_paren, xs)) ->
					let rs = (arg, None) :: rs in
					List.rev rs, xs
				| lazy (`cons (ps, _, _)) | lazy (`nil (ps, ())) ->
					error ps parenthesis_is_missing;
					let rs = (arg, None) :: rs in
					rs, xs
				end
			end
		in
		begin match xs with
		| lazy (`cons (_, `r_paren, xs)) ->
			[], xs
		| _ ->
			loop xs []
		end
	);;
	
	let concat_with_comma (xs: (in_t * comma_token option) list): in_prim = (
		let ys = List.map
			begin fun (tokens, comma) ->
				begin match comma with
				| Some (comma_p, comma_e) ->
					lazy (LazyList.append
						tokens
						(lazy (`cons (comma_p, (comma_e :> LexicalElement.t),
							lazy (`nil (comma_p, ()))))))
				| None ->
					tokens
				end
			end
			xs
		in
		LazyList.concat ys
	);;
	
	(* ## *)
	
	let merge_positions (ps1: ranged_position) (ps2: ranged_position): ranged_position = (
		let (filename1, _, line1, column1 as ps1_fst), _ = ps1 in
		let _, (filename2, _, line2, column2 as ps2_snd) = ps2 in
		if filename1 = filename2 && (line1 < line2 || (line1 = line2 && column1 < column2)) then (
			ps1_fst, ps2_snd
		) else (
			ps1
		)
	);;
	
	let rescan
		(error: ranged_position -> string -> unit)
		(ps: ranged_position)
		(s: string)
		: LexicalElement.t =
	(
		assert (String.length s > 0);
		if s.[0] >= '0' && s.[0] <= '9' then (
			let error_flag = ref false in
			let result, index = NumericScanner.scan_numeric_literal
				(fun _ _ -> error_flag := true)
				(Buffer.create 256)
				(fun _ _ -> 0)
				(fun _ _ -> 0)
				(fun s index -> if index >= String.length s then '\x1a' else s.[index])
				(fun _ index -> index + 1)
				s
				0
			in
			if !error_flag || index <> String.length s then (
				error ps (illegal_concatenated_token s)
			);
			result
		) else (
			`ident s
		)
	);;
	
	let for_d_sharp
		(xs: (ranged_position, LexicalElement.t, 'c) LazyList.t)
		: ((ranged_position * LexicalElement.t) option * ranged_position * concatable_element) option =
	(
		begin match xs with
		| lazy (`cons (ps, (#concatable_element as e), lazy (`nil _))) ->
			Some (None, ps, e)
		| lazy (`cons (p1, (`minus as e1), lazy (`cons (ps, (#concatable_element as e), lazy (`nil _))))) ->
			Some (Some (p1, e1), ps, e) (* for -1 ## L *)
		| _ ->
			None
		end
	);;
	
	(* preprocessor *)
	
	type state_t = [`top_level | `in_ifdef of int | `in_macro_expr];;
	
	let inc_level (state: state_t): state_t = (
		begin match state with
		| `top_level ->
			`in_ifdef 1
		| `in_ifdef n ->
			assert (n > 0);
			`in_ifdef (n + 1)
		| `in_macro_expr ->
			assert false
		end
	);;
	
	let dec_level (state: state_t): state_t = (
		begin match state with
		| `in_ifdef n ->
			if n = 1 then `top_level else
			if n > 0 then `in_ifdef (n - 1) else
			assert false
		| `top_level | `in_macro_expr ->
			assert false
		end
	);;
	
	let expanding_level (state: state_t): state_t = (
		begin match state with
		| `top_level | `in_ifdef _ ->
			`top_level
		| `in_macro_expr ->
			`in_macro_expr
		end
	);;
	
	let preprocess
		(error: ranged_position -> string -> unit)
		(is_known_error: ranged_position -> string -> [> known_errors_of_preprocessor] -> bool)
		(_: language)
		(read: current:string -> ?next:bool -> include_from -> string -> (ranged_position -> in_prim) -> in_prim)
		: state_t -> define_map -> macro_argument_map -> in_t -> out_prim =
	(
		let rec process
			(state: state_t)
			(predefined: define_map)
			(macro_arguments: macro_argument_map)
			(xs: in_t)
			: out_prim =
		(
			let rec process_if (cond: bool) (xs: in_t): out_prim = (
				if cond then (
					process (inc_level state) predefined StringMap.empty xs
				) else (
					let xs = skip_structure_until is_else_or_endif 0 xs in
					begin match xs with
					| lazy (`cons (_, `sharp_ELIF, xs)) ->
						let expr, xs = take_line xs in
						let expr' = lazy (process `in_macro_expr predefined StringMap.empty expr) in
						let cond = bool_of_leinteger (calc_expr error is_known_error expr') in
						process_if cond xs
					| lazy (`cons (_, `sharp_ELSE, xs)) ->
						let xs = skip_line xs in
						process_if true xs
					| lazy (`cons (_, `sharp_ENDIF, xs)) ->
						let xs = skip_line xs in
						process state predefined StringMap.empty xs
					| lazy (`cons (ps, _, _)) | lazy (`nil (ps, _)) ->
						error ps endif_is_missing;
						process state predefined StringMap.empty xs
					end
				)
			) in
			let process_replace (ps: ranged_position) (token: LexicalElement.t) (name: string) (xs: in_t): out_prim = (
				let item = StringMap.find name predefined in
				if item.df_has_arguments then (
					begin match xs with
					| lazy (`cons (_, `l_paren, xs)) ->
						let in_arguments, xs = take_arguments error xs in
						let the_macro_arguments =
							let rec loop
								(names: (ranged_position * string) list)
								(in_arguments: (in_t * comma_token option) list)
								(out_arguments: macro_argument_map)
								: macro_argument_map =
							(
								begin match names, in_arguments with
								| [], [] ->
									out_arguments
								| [], _ ->
									if item.df_varargs then (
										let n = string_of_rw `__VA_ARGS__ in
										let a = lazy (concat_with_comma in_arguments) in
										let expanded_a = lazy (process (expanding_level state) predefined macro_arguments a) in
										StringMap.add n (for_d_sharp expanded_a, expanded_a) out_arguments
									) else (
										error ps (too_many_arguments name);
										out_arguments
									)
								| _ :: _, [] ->
									error ps (too_few_arguments name);
									out_arguments
								| (_, n) :: nr, (a, _) :: ar ->
									let a_pair =
										begin match a with
										| lazy (`cons (_, `ident a_name, lazy (`nil _))) when StringMap.mem a_name macro_arguments ->
											let expanded_a = snd (StringMap.find a_name macro_arguments) in
											for_d_sharp expanded_a, expanded_a
										| _ ->
											let expanded_a = lazy (process (expanding_level state) predefined macro_arguments a) in
											for_d_sharp a, expanded_a
										end
									in
									loop nr ar (StringMap.add n a_pair out_arguments)
								end
							) in
							loop item.df_args in_arguments StringMap.empty
						in
						let ys =
							let removed = StringMap.remove name predefined in
							let ys = lazy (process (expanding_level state) removed the_macro_arguments item.df_contents) in
							(* replace position-info to current *)
							let (filename, _, line, _), _ = ps in
							lazy (LazyList.map_a
								begin fun ps' ->
									let (filename', _, line', _), _ = ps' in
									if filename' = filename && line' >= line then ps' else ps
								end
								ys)
						in
						LazyList.append ys (lazy (
							process state predefined macro_arguments xs))
					| _ ->
						(* not expainding when the function macro used without arguments *)
						`cons (ps, token, lazy (
							process state predefined macro_arguments xs))
					end
				) else (
					let removed = StringMap.remove name predefined in
					let ys = lazy (process (expanding_level state) removed StringMap.empty item.df_contents) in
					begin match ys with
					| lazy (`cons (_, `ident replaced, lazy (`nil _)))
						when StringMap.mem replaced removed
							&& (StringMap.find replaced removed).df_has_arguments
							&& (match xs with lazy (`cons (_, `l_paren, _)) -> true | _ -> false)
					->
						(* re-expanding function-macro with following arguments *)
						(* replace position-info to current *)
						let ys = lazy (LazyList.map_a (fun _ -> ps) ys) in
						process state predefined macro_arguments (lazy (
							LazyList.append ys xs))
					| _ ->
						(* replace position-info to current *)
						let ys = lazy (LazyList.map_a (fun _ -> ps) ys) in
						LazyList.append ys (lazy (
							process state predefined macro_arguments xs))
					end
				)
			) in
			let process_d_sharp (ps1: ranged_position) (it1: concatable_element) (ds_p: ranged_position) (xs: in_t): out_prim = (
				let name1 =
					match it1 with
					| `ident name1 -> name1
					| `numeric_literal (name1, _) -> name1
					| #reserved_word as rw -> string_of_rw rw
				in
				begin match xs with
				| lazy (`cons (_, `ident name2, xs)) when StringMap.mem name2 macro_arguments ->
					let arg = StringMap.find name2 macro_arguments in
					begin match arg with
					| Some (None, ps2, (`ident name2 | `numeric_literal (name2, _))), _ -> (* ## use unexpanded token *)
						let merged_ps = merge_positions ps1 ps2 in
						let merged_token = rescan error merged_ps (name1 ^ name2) in
						process state predefined macro_arguments
							(lazy (`cons (merged_ps, merged_token, xs)))
					| None, lazy (`cons (ps2, `chars_literal s, ys)) ->
						if name1 = "L" then (
							let s = Array.init (String.length s) (fun i -> Int32.of_int (int_of_char s.[i])) in
							let merged_ps = merge_positions ps1 ps2 in
							let merged_token = `wchars_literal (WideString.of_array s) in
							let rs = lazy (LazyList.append ys xs) in
							`cons (merged_ps, merged_token, lazy (
								process state predefined macro_arguments rs))
						) else (
							error ds_p two_identifiers_are_required_for_d_shap;
							`cons (ps1, (it1 :> LexicalElement.t), lazy (`cons (ds_p, `d_sharp, lazy (
								LazyList.append (snd arg) (lazy (
									process state predefined macro_arguments xs))))))
						)
					| None, lazy (`nil _) ->
						(* skip ## *)
						`cons (ps1, (it1 :> LexicalElement.t), lazy (
							LazyList.append (snd arg) (lazy (
								process state predefined macro_arguments xs))))
					| _ ->
						error ds_p two_identifiers_are_required_for_d_shap;
						`cons (ps1, (it1 :> LexicalElement.t), lazy (`cons (ds_p, `d_sharp, lazy (
							LazyList.append (snd arg) (lazy (
								process state predefined macro_arguments xs))))))
					end
				| lazy (`cons (ps2, (`ident name2 | `numeric_literal (name2, _)), xs)) ->
					let merged_ps = merge_positions ps1 ps2 in
					let merged_token = rescan error merged_ps (name1 ^ name2) in
					process state predefined macro_arguments
						(lazy (`cons (merged_ps, merged_token, xs)))
				| _ ->
					error ds_p two_identifiers_are_required_for_d_shap;
					`cons (ps1, (it1 :> LexicalElement.t), lazy (`cons (ds_p, `d_sharp, lazy (
						process state predefined macro_arguments xs))))
				end
			) in
			begin match xs with
			| lazy (`cons (ps, token, xs)) ->
				begin match token with
				| `sharp_DEFINE ->
					assert (state <> `in_macro_expr);
					begin match xs with
					| lazy (`cons (name_ps, `ident name, xs)) ->
						let new_item, xs =
							let (_, (name_file, name_lpos, _, _)) = name_ps in
							begin match xs with
							| lazy (`cons (((lp_file, lp_fpos, _, _), _), `l_paren, xs))
								when lp_file = name_file && lp_fpos = name_lpos + 1 ->
								(* function macro *)
								let rec loop xs = (
									begin match xs with
									| lazy (`cons (arg_p, `ident arg,
										lazy (`cons (_, `comma, xs)))) ->
										let args, varargs, xs = loop xs in
										((arg_p, arg) :: args), varargs, xs
									| lazy (`cons (arg_p, `ident arg,
										lazy (`cons (_, `r_paren, xs)))) ->
										[arg_p, arg], false, xs
									| lazy (`cons (_, `varargs,
										lazy (`cons (_, `r_paren, xs)))) ->
										[], true, xs
									| lazy (`cons (_, `r_paren, xs)) ->
										[], false, xs
									| lazy (`cons (ps, _, _)) | lazy (`nil (ps, _)) ->
										error ps parenthesis_is_missing;
										[], false, xs
									end
								) in
								let args, varargs, xs = loop xs in
								let cs, xs = take_line xs in
								let new_item = {
									df_name = name;
									df_position = name_ps;
									df_has_arguments = true;
									df_args = args;
									df_varargs = varargs;
									df_contents = cs}
								in
								new_item, xs
							| _ ->
								(* simple macro *)
								let cs, xs = take_line xs in
								let new_item = {
									df_name = name;
									df_position = name_ps;
									df_has_arguments = false;
									df_args = [];
									df_varargs = false;
									df_contents = cs}
								in
								new_item, xs
							end
						in
						let inserting =
							if StringMap.mem name predefined then (
								let old_item = StringMap.find name predefined in
								if define_item_eq new_item old_item then false else (
									error old_item.df_position (vanished_macro name);
									error ps (redefined_macro name);
									true
								)
							) else (
								true
							)
						in
						let predefined =
							if inserting then StringMap.add new_item.df_name new_item predefined else
							predefined
						in
						process state predefined StringMap.empty xs
					| lazy (`cons (name_ps, (#extended_word as ew), xs)) ->
						let filename, _, _, _ = fst name_ps in
						let name = string_of_rw ew in
						if filename <> predefined_name && not (is_known_error name_ps name `redefine_extended_word) then (
							error name_ps (redefined_extended_word name)
						);
						let xs = skip_line xs in
						process state predefined StringMap.empty xs
					| lazy (`cons (name_ps, (#compiler_macro as be_defined), xs)) ->
						let filename, _, _, _ = fst name_ps in
						let name = string_of_rw be_defined in
						if filename <> predefined_name && not (is_known_error name_ps name `redefine_compiler_macro) then (
							error name_ps (redefined_compiler_macro name)
						);
						let xs = skip_line xs in
						process state predefined StringMap.empty xs
					| _ ->
						error ps macro_name_is_missing_for_define;
						let xs = skip_line xs in
						process state predefined StringMap.empty xs
					end
				| `sharp_UNDEF ->
					assert (state <> `in_macro_expr);
					begin match xs with
					| lazy (`cons (_, `ident name, xs)) ->
						let predefined = StringMap.remove name predefined in
						let xs = take_end_of_line error xs in
						process state predefined StringMap.empty xs
					| lazy (`cons (_, (#reserved_word as rw), xs)) -> (* implies #extended_word *)
						let name = string_of_rw rw in
						let predefined = StringMap.remove name predefined in
						let xs = take_end_of_line error xs in
						process state predefined StringMap.empty xs
					| _ ->
						error ps macro_name_is_missing_for_undef;
						let xs = skip_line xs in
						process state predefined StringMap.empty xs
					end
				| `sharp_IF ->
					assert (state <> `in_macro_expr);
					let expr, xs = take_line xs in
					let expr' = lazy (process `in_macro_expr predefined StringMap.empty expr) in
					let cond = bool_of_leinteger (calc_expr error is_known_error expr') in
					process_if cond xs
				| `sharp_IFDEF ->
					assert (state <> `in_macro_expr);
					begin match xs with
					| lazy (`cons (_, `ident name, xs)) ->
						let cond = StringMap.mem name predefined in
						let xs = take_end_of_line error xs in
						process_if cond xs
					| lazy (`cons (_, (#extended_word as ew), xs)) ->
						let name = string_of_rw ew in
						let cond = StringMap.mem name predefined in
						let xs = take_end_of_line error xs in
						process_if cond xs
					| lazy (`cons (_, #compiler_macro, xs)) ->
						let xs = take_end_of_line error xs in
						process_if true xs
					| _ ->
						error ps macro_name_is_missing_for_ifdef;
						let xs = skip_line xs in
						process_if false xs
					end
				| `sharp_IFNDEF ->
					assert (state <> `in_macro_expr);
					begin match xs with
					| lazy (`cons (_, `ident name, xs)) ->
						let cond = not (StringMap.mem name predefined) in
						let xs = take_end_of_line error xs in
						process_if cond xs
					| lazy (`cons (_, (#extended_word as ew), xs)) ->
						let name = string_of_rw ew in
						let cond = not (StringMap.mem name predefined) in
						let xs = take_end_of_line error xs in
						process_if cond xs
					| lazy (`cons (_, #compiler_macro, xs)) ->
						let xs = take_end_of_line error xs in
						process_if false xs
					| _ ->
						error ps macro_name_is_missing_for_ifndef;
						let xs = skip_line xs in
						process_if false xs
					end
				| `sharp_ELIF
				| `sharp_ELSE as token ->
					begin match state with
					| `top_level ->
						begin match token with
						| `sharp_ELIF -> error ps unexpected_elif
						| `sharp_ELSE -> error ps unexpected_else
						end;
						let xs = skip_line xs in
						process state predefined StringMap.empty xs
					| `in_ifdef _ ->
						let xs = skip_structure_until is_endif 0 xs in
						begin match xs with
						| lazy (`cons (_, `sharp_ENDIF, xs)) ->
							let xs = skip_line xs in
							process (dec_level state) predefined StringMap.empty xs
						| lazy (`cons (ps, _, _)) | lazy (`nil (ps, _)) ->
							error ps endif_is_missing;
							process (dec_level state) predefined StringMap.empty xs
						end
					| `in_macro_expr ->
						assert false
					end
				| `sharp_ENDIF ->
					begin match state with
					| `top_level ->
						error ps unexpected_endif;
						let xs = skip_line xs in
						process state predefined StringMap.empty xs
					| `in_ifdef _ ->
						let xs = skip_line xs in
						process (dec_level state) predefined StringMap.empty xs
					| `in_macro_expr ->
						assert false
					end
				| `sharp_INCLUDE
				| `sharp_INCLUDE_NEXT as token ->
					assert (state <> `in_macro_expr);
					begin match xs with
					| lazy (`cons (_, `directive_parameter s,
						lazy (`cons (_, `end_of_line, xs)))) ->
						let s_length = String.length s in
						let from =
							if s_length <= 2 then (
								assert false
							) else if s.[0] = '<' && s.[s_length - 1] = '>' then (
								`system
							) else if s.[0] = '\"' && s.[s_length - 1] = '\"' then (
								`user
							) else (
								assert false
							)
						in
						begin try
							let filename = String.sub s 1 (s_length - 2) in
							let included: in_t =
								let (current, _, _, _), _ = ps in
								let next = token = `sharp_INCLUDE_NEXT in
								lazy (read ~current ~next from filename (fun _ -> Lazy.force xs))
							in
							process state predefined StringMap.empty included
						with Not_found ->
							error ps (header_file_is_not_found s);
							process state predefined StringMap.empty xs
						end
					| _ ->
						assert false
					end
				| `sharp_ERROR ->
					assert (state <> `in_macro_expr);
					begin match xs with
					| lazy (`cons (_, `directive_parameter message,
						lazy (`cons (_, `end_of_line, xs)))) ->
						error ps (error_message message);
						process state predefined StringMap.empty xs
					| _ ->
						assert false
					end
				| `sharp_WARNING ->
					assert (state <> `in_macro_expr);
					begin match xs with
					| lazy (`cons (_, `directive_parameter message,
						lazy (`cons (_, `end_of_line, xs)))) ->
						error ps (warning_message message);
						process state predefined StringMap.empty xs
					| _ ->
						assert false
					end
				| `sharp_LINE -> (* does not handle #line *)
					assert (state <> `in_macro_expr);
					let xs = skip_line xs in
					process state predefined StringMap.empty xs
				| `sharp_PRAGMA ->
					assert (state <> `in_macro_expr);
					begin match xs with
					| lazy (`cons (_, `ident ("push_macro" | "pop_macro" as pragma), xs)) ->
						begin match xs with
						| lazy (`cons (_, `l_paren,
							lazy (`cons (target_ps, `chars_literal target_name,
								lazy (`cons (_, `r_paren,
									lazy (`cons (_, `end_of_line, xs))))))))
						->
							let push_or_pop =
								if pragma = "push_macro" then `push else `pop
							in
							let predefined =
								begin match push_or_pop with
								| `push ->
									if StringMap.mem target_name predefined
										&& not (is_known_error ps target_name `push_defined_macro)
									then (
										error target_ps (defined_macro_is_pushed target_name);
									);
									predefined
								| `pop ->
									StringMap.remove target_name predefined
								end
							in
							process state predefined StringMap.empty xs
						| _ ->
							error ps (pragma_syntax_error pragma);
							let xs = skip_line xs in
							process state predefined StringMap.empty xs
						end
					| _ ->
						(* #pragma should be handled by compiler, not preprocessor *)
						`cons (ps, token, lazy (
							process state predefined StringMap.empty xs))
					end
				| `sharp ->
					begin match xs with
					| lazy (`cons (_, `ident name2, xs)) when StringMap.mem name2 macro_arguments ->
						let arg = StringMap.find name2 macro_arguments in
						begin match arg with
						| Some (None, ps2, (`ident name2 | `numeric_literal (name2, _))), _ -> (* # use unexpanded token *)
							let chars_token = `chars_literal name2 in
							let merged_ps = merge_positions ps ps2 in
							`cons (merged_ps, chars_token, lazy (
								process state predefined macro_arguments xs))
						| None, lazy (`nil (ps2, _)) -> (* #define S(X) #X and used without argument as S() *)
							let chars_token = `chars_literal "" in
							let merged_ps = merge_positions ps ps2 in
							`cons (merged_ps, chars_token, lazy (
								process state predefined macro_arguments xs))
						| _ ->
							error ps one_identifier_is_required_for_shap;
							LazyList.append (snd arg) (lazy (
								process state predefined macro_arguments xs))
						end
					| lazy (`cons (ps2, `ident name2, xs)) ->
						let chars_token = `chars_literal name2 in
						let merged_ps = merge_positions ps ps2 in
						`cons (merged_ps, chars_token, lazy (
							process state predefined macro_arguments xs))
					| _ ->
						error ps one_identifier_is_required_for_shap;
						process state predefined macro_arguments xs
					end
				| `ident "defined" when state = `in_macro_expr ->
					begin match xs with
					| lazy (`cons (_, `l_paren,
						lazy (`cons (_, `ident name,
							lazy (`cons ((_, p2), `r_paren, xs))))))
					| lazy (`cons ((_, p2), `ident name, xs)) ->
						let (p1, _) = ps in
						let cond = leinteger_of_bool (StringMap.mem name predefined) in
						let image = Integer.to_based_string ~base:10 cond in
						let value = `numeric_literal (image, `int_literal (`signed_int, cond)) in
						`cons ((p1, p2), value, lazy (
							process `in_macro_expr predefined macro_arguments xs))
					| lazy (`cons (_, `l_paren,
						lazy (`cons (_, (#extended_word as ew),
							lazy (`cons ((_, p2), `r_paren, xs))))))
					| lazy (`cons ((_, p2), (#extended_word as ew), xs)) ->
						let (p1, _) = ps in
						let name = string_of_rw ew in
						let cond = leinteger_of_bool (StringMap.mem name predefined) in
						let image = Integer.to_based_string ~base:10 cond in
						let value = `numeric_literal (image, `int_literal (`signed_int, cond)) in
						`cons ((p1, p2), value, lazy (
							process `in_macro_expr predefined macro_arguments xs))
					| lazy (`cons (_, `l_paren,
						lazy (`cons (_, #compiler_macro,
							lazy (`cons ((_, p2), `r_paren, xs))))))
					| lazy (`cons ((_, p2), #compiler_macro, xs)) ->
						let (p1, _) = ps in
						`cons ((p1, p2), the_one, lazy (
							process `in_macro_expr predefined macro_arguments xs))
					| _ ->
						error ps defined_syntax_error;
						process `in_macro_expr predefined macro_arguments xs
					end
				| `__STDC__ ->
					`cons (ps, __STDC__, lazy (
						process state predefined macro_arguments xs))
				| `__STDC_VERSION__ ->
					`cons (ps, __STDC_VERSION__, lazy (
						process state predefined macro_arguments xs))
				| `ident name when StringMap.mem name macro_arguments ->
					let arg = StringMap.find name macro_arguments in
					begin match xs with
					| lazy (`cons (ds_p, (`d_sharp as ds_e), xs)) ->
						begin match arg with
						| Some (pre_item, ps1, it1), _ -> (* ## use unexpanded token *)
							begin match pre_item with
							| Some (pre_p, pre_e) ->
								`cons (pre_p, pre_e, lazy (process_d_sharp ps1 it1 ds_p xs))
							| None ->
								process_d_sharp ps1 it1 ds_p xs
							end
						| _ ->
							LazyList.append (snd arg) (lazy (`cons (ds_p, ds_e, lazy (
								process state predefined macro_arguments xs))))
						end
					| _ ->
						let has_following_arguments =
							begin match snd arg with
							| lazy (`cons (_, (`ident replaced), lazy (`nil _))) ->
								begin match xs with
								| lazy (`cons (_, `l_paren, _))
									when StringMap.mem replaced predefined
										&& (StringMap.find replaced predefined).df_has_arguments
								->
									true
								| _ ->
									false
								end
							| _ ->
								false
							end
						in
						if has_following_arguments then (
							(* re-expanding function-macro with following arguments *)
							let rs = lazy (LazyList.append (snd arg) xs) in
							process state predefined macro_arguments rs
						) else (
							LazyList.append (snd arg) (lazy (
								process state predefined macro_arguments xs))
						)
					end
				| `ident name as it1 when StringMap.mem name predefined ->
					begin match xs with
					| lazy (`cons (ds_p, `d_sharp, xs)) ->
						process_d_sharp ps it1 ds_p xs
					| _ ->
						process_replace ps token name xs
					end
				| `ident _ | `numeric_literal _ as it1 -> (* ##-able *)
					begin match xs with
					| lazy (`cons (ds_p, `d_sharp, xs)) ->
						process_d_sharp ps it1 ds_p xs
					| _ ->
						`cons (ps, token, lazy (
							process state predefined macro_arguments xs))
					end
				| #reserved_word as rw -> (* implies #extended_word *)
					begin match xs with
					| lazy (`cons (ds_p, `d_sharp, xs)) ->
						process_d_sharp ps (`ident (string_of_rw rw)) ds_p xs
					| _ ->
						let s_rw = string_of_rw rw in
						if StringMap.mem s_rw predefined then (
							process_replace ps token s_rw xs
						) else (
							`cons (ps, token, lazy (
								process state predefined macro_arguments xs))
						)
					end
				| _ ->
					`cons (ps, token, lazy (
						process state predefined macro_arguments xs))
				end
			| lazy (`nil (ps, _)) ->
				begin match state with
				| `in_ifdef _ -> error ps endif_is_missing
				| _ -> ()
				end;
				`nil (ps, predefined)
			end
		) in
		process
	);;
	
end;;
