open C_lexical;;
open C_lexical_scanner;;
open C_literals;;
open Position;;

module type ScannerType = sig
	module Literals: LiteralsType;;
	module LexicalElement: LexicalElementType
		with module Literals := Literals;;
	
	module NumericScanner: NumericScannerType
		with module Literals := Literals
		with module LexicalElement := LexicalElement;;
	
	type prim = (ranged_position, LexicalElement.t, unit) LazyList.prim;;
	type t = (ranged_position, LexicalElement.t, unit) LazyList.t;;
	
	val make_nil: ranged_position -> prim;;
	
	val scan:
		(ranged_position -> string -> unit) ->
		(unit -> unit) ->
		language ->
		TextFile.t ->
		(ranged_position -> prim) ->
		prim;;
	
end;;

module Scanner
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType
		with module Literals := Literals)
	: ScannerType
		with module Literals := Literals
		with module LexicalElement := LexicalElement =
struct
	open Literals;;
	module NumericScanner = NumericScanner (Literals) (LexicalElement);;
	
	(* error messages *)
	
	let unexpected (c: char): string =
		"\'" ^ Char.escaped c ^ "\' is bad character.";;
	let illegal_char_literal (s: string): string =
		"\'" ^ String.escaped s ^ "\' is illegal char literal.";;
	let unclosed_string (s: string): string =
		"\"" ^ String.escaped s ^ "\" is not closed string.";;
	let unclosed_comment: string =
		"unclosed comment.";;
	let illegal_include_form: string =
		"include form should be \"...\" or <...>.";;
	let unknown_objcdirective (s: string): string =
		"@" ^ s ^ " is unknown Objective-C directive.";;
	let unknown_ppdirective (s: string): string =
		"#" ^ s ^ " is unknown preprocessor directive.";;
	
	(* char handling *)
	
	let is_tailing (c: char): bool = (
		begin match c with
		| 'A'..'Z' | 'a'..'z' | '_' | '0'..'9' -> true
		| _ -> false
		end
	);;
	
	let is_lineescape (c: char): bool = c = '\\';;
	
	(* scanner *)
	
	type prim = (ranged_position, LexicalElement.t, unit) LazyList.prim;;
	type t = (ranged_position, LexicalElement.t, unit) LazyList.t;;
	
	let make_nil (ps: ranged_position): prim = `nil (ps, ());;
	
	let scan
		(error: ranged_position -> string -> unit)
		(finalize: unit -> unit)
		(lang: language)
		(source: TextFile.t)
		(next: ranged_position -> prim): prim =
	(
		(* reading functions *)
		let read_string_to_buffer (p1: position) ~(quote: char) (buf: Buffer.t) (index: int): int = (
			assert (TextFile.get source index = quote);
			let index = TextFile.succ source index in
			let rec loop (index: int): int = (
				begin match TextFile.get source index with
				| '\\' ->
					let index = TextFile.succ source index in
					let index =
						begin match TextFile.get source index with
						| 'a' ->
							Buffer.add_char buf '\x07';
							TextFile.succ source index
						| 'b' ->
							Buffer.add_char buf '\x08';
							TextFile.succ source index
						| 'f' ->
							Buffer.add_char buf '\x0c';
							TextFile.succ source index
						| 'n' ->
							Buffer.add_char buf '\n';
							TextFile.succ source index
						| 'r' ->
							Buffer.add_char buf '\r';
							TextFile.succ source index
						| 't' ->
							Buffer.add_char buf '\t';
							TextFile.succ source index
						| 'v' ->
							Buffer.add_char buf '\x0b';
							TextFile.succ source index
						| 'x' ->
							let index = TextFile.succ source index in
							let rec loop_x (code: int) (index: int): int = (
								begin match TextFile.get source index with
								| '0'..'9' as h ->
									let code = code * 16 + (Char.code h - Char.code '0') in
									let index = TextFile.succ source index in
									loop_x code index
								| 'A'..'F' as h ->
									let code = code * 16 + (Char.code h - (Char.code 'A' - 10)) in
									let index = TextFile.succ source index in
									loop_x code index
								| 'a'..'f' as h ->
									let code = code * 16 + (Char.code h - (Char.code 'a' - 10)) in
									let index = TextFile.succ source index in
									loop_x code index
								| _ ->
									Buffer.add_char buf (Char.chr code);
									index
								end
							) in
							loop_x 0 index
						| '0'..'7' ->
							let rec loop_o (code: int) (index: int): int = (
								begin match TextFile.get source index with
								| '0'..'7' as h ->
									let code = code * 8 + (Char.code h - Char.code '0') in
									let index = TextFile.succ source index in
									loop_o code index
								| _ ->
									Buffer.add_char buf (Char.chr code);
									index
								end
							) in
							loop_o 0 index
						| _ as h ->
							Buffer.add_char buf h;
							TextFile.succ source index
						end
					in
					loop index
				| '\n' | '\r' | '\x0c' | '\x1a' ->
					let p2 = TextFile.position source index in
					let result = Buffer.contents buf in
					error (p1, p2) (unclosed_string result);
					index
				| h when h = quote ->
					let index = TextFile.succ source index in
					index
				| _ as h ->
					Buffer.add_char buf h;
					let index = TextFile.succ source index in
					loop index
				end
			) in
			loop index
		) in
		let read_header_to_buffer (buf: Buffer.t) (index: int): int = (
			let p1 = TextFile.position source index in
			let k = TextFile.get source index in
			begin match k with
			| '\"' | '<' ->
				let pair = (if k = '<' then '>' else '\"') in
				let index = TextFile.succ source index in
				Buffer.add_char buf k;
				let rec loop (index: int): int = (
					begin match TextFile.get source index with
					| '\n' | '\r' | '\x0c' | '\x1a' ->
						let p2 = TextFile.prev_position source index in
						error (p1, p2) illegal_include_form;
						Buffer.add_char buf pair; (* error recovery *)
						index
					| _ as c ->
						Buffer.add_char buf c;
						let index = TextFile.succ source index in
						if c <> pair then loop index else
						index
					end
				) in
				loop index
			| _ ->
				error (p1, p1) illegal_include_form;
				TextFile.succ_until_line is_lineescape source index (* skip line *)
			end
		) in
		(* shared buffer *)
		let buf = Buffer.create 256 in
		(* main *)
		let rec process (state: [`first | `pp | `normal]) (index: int): prim = (
			(* nested functions *)
			let nx (state: [`first | `pp | `normal]): [`first | `pp | `normal] = (
				if state = `first then `normal else state
			) in
			let do_single_char_token token = (
				let state = nx state in
				let p = TextFile.position source index in
				let index = TextFile.succ source index in
				`cons ((p, p), token, lazy (process state index))
			) in
			(* body *)
			begin match TextFile.get source index with
			| ' ' | '\t' | '\b' ->
				let index = TextFile.succ source index in
				process state index
			| '\n' | '\r' | '\x0c' ->
				let making_token = state = `pp in
				if making_token then (
					let p1 = TextFile.position source index in
					let index = TextFile.succ_line source index in
					let p2 = TextFile.prev_position source index in
					`cons ((p1, p2), `end_of_line, lazy (process `first index))
				) else (
					let index = TextFile.succ_line source index in
					process `first index
				)
			| '\\' ->
				let index = TextFile.succ source index in
				if state = `pp then (
					(* macro-line continuation *)
					let index =
						begin match TextFile.get source index with
						| '\n' | '\r' | '\x0c' ->
							TextFile.succ_line source index
						| _ ->
							let p = TextFile.prev_position source index in
							error (p, p) (unexpected '\\');
							index
						end
					in
					process state index
				) else (
					begin match TextFile.get source index with
					| '\n' | '\r' | '\x0c' ->
						() (* '\' and line feed be skipped *)
					| _ ->
						let p = TextFile.prev_position source index in
						error (p, p) (unexpected '\\')
					end;
					process state index
				)
			| '(' ->
				do_single_char_token `l_paren
			| ')' ->
				do_single_char_token `r_paren
			| '[' ->
				do_single_char_token `l_bracket
			| ']' ->
				do_single_char_token `r_bracket
			| '{' ->
				do_single_char_token `l_curly
			| '}' ->
				do_single_char_token `r_curly
			| '~' ->
				do_single_char_token `tilde
			| '?' ->
				do_single_char_token `question
			| ',' ->
				do_single_char_token `comma
			| ';' ->
				do_single_char_token `semicolon
			| '.' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '.' ->
					let index = TextFile.succ source index in
					begin match TextFile.get source index with
					| '.' ->
						let p3 = TextFile.position source index in
						let index = TextFile.succ source index in
						`cons ((p1, p3), `varargs, lazy (process state index))
					| _ ->
						let p2 = TextFile.prev_position source index in
						`cons ((p1, p1), `period, lazy (
							`cons ((p2, p2), `period, lazy (process state index))))
					end
				| '*' when cxx lang ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `period_ref, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `period, lazy (process state index))
				end
			| ':' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '>' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `r_bracket, lazy (process state index))
				| ':' when cxx lang ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `d_colon, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `colon, lazy (process state index))
				end
			| '<' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| ':' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `l_bracket, lazy (process state index))
				| '%' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `l_curly, lazy (process state index))
				| '<' ->
					let index = TextFile.succ source index in
					begin match TextFile.get source index with
					| '=' ->
						let p3 = TextFile.position source index in
						let index = TextFile.succ source index in
						`cons ((p1, p3), `l_shift_assign, lazy (process state index))
					| _ ->
						let p2 = TextFile.prev_position source index in
						`cons ((p1, p2), `l_shift, lazy (process state index))
					end
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `le, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `lt, lazy (process state index))
				end
			| '>' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '>' ->
					let index = TextFile.succ source index in
					begin match TextFile.get source index with
					| '=' ->
						let p3 = TextFile.position source index in
						let index = TextFile.succ source index in
						`cons ((p1, p3), `r_shift_assign, lazy (process state index))
					| _ ->
						let p2 = TextFile.prev_position source index in
						`cons ((p1, p2), `r_shift, lazy (process state index))
					end
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `ge, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `gt, lazy (process state index))
				end
			| '-' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '>' ->
					let index = TextFile.succ source index in
					begin match TextFile.get source index with
					| '*' when cxx lang ->
						let p3 = TextFile.position source index in
						let index = TextFile.succ source index in
						`cons ((p1, p3), `arrow_ref, lazy (process state index))
					| _ ->
						let p2 = TextFile.prev_position source index in
						`cons ((p1, p2), `arrow, lazy (process state index))
					end
				| '-' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `decrement, lazy (process state index))
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `sub_assign, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `minus, lazy (process state index))
				end
			| '+' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '+' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `increment, lazy (process state index))
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `add_assign, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `plus, lazy (process state index))
				end
			| '*' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `mul_assign, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `asterisk, lazy (process state index))
				end
			| '/' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '/' ->
					let index = TextFile.succ source index in
					let index = TextFile.succ_until_line is_lineescape source index in
					process state index
				| '*' ->
					let index = TextFile.succ source index in
					let rec loop_comment (index: int): int = (
						begin match TextFile.get source index with
						| '\x1a' ->
							let p2 = TextFile.prev_position source index in
							error (p1, p2) unclosed_comment;
							index
						| '*' ->
							let index = TextFile.succ source index in
							begin match TextFile.get source index with
							| '/' ->
								let index = TextFile.succ source index in
								index
							| _ ->
								loop_comment index
							end
						| _ ->
							let index = TextFile.succ source index in
							loop_comment index
						end
					) in
					let index = loop_comment index in
					process state index
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `div_assign, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `slash, lazy (process state index))
				end
			| '%' ->
				let state = nx state in
				let rec scan_after_percent (p1: position) (index: int): prim = (
					begin match TextFile.get source index with
					| ':' -> (* replace %: to #, mixing and starting macro with %: are not supported *)
						let p2 = TextFile.position source index in
						let index = TextFile.succ source index in
						begin match TextFile.get source index with
						| '%' ->
							let index = TextFile.succ source index in
							begin match TextFile.get source index with
							| ':' -> (* ## *)
								let p4 = TextFile.position source index in
								let index = TextFile.succ source index in
								`cons ((p1, p4), `d_sharp, lazy (process state index))
							| _ -> (* #%... *)
								let p3 = TextFile.prev_position source index in
								`cons ((p1, p2), `sharp, lazy (scan_after_percent p3 index))
							end
						| _ ->
							`cons ((p1, p2), `sharp, lazy (process state index))
						end
					| '>' ->
						let p2 = TextFile.position source index in
						let index = TextFile.succ source index in
						`cons ((p1, p2), `r_curly, lazy (process state index))
					| '=' ->
						let p2 = TextFile.position source index in
						let index = TextFile.succ source index in
						`cons ((p1, p2), `rem_assign, lazy (process state index))
					| _ ->
						`cons ((p1, p1), `percent, lazy (process state index))
					end
				) in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				scan_after_percent p1 index
			| '^' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `xor_assign, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `caret, lazy (process state index))
				end
			| '|' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '|' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `or_else, lazy (process state index))
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `or_assign, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `vertical, lazy (process state index))
				end
			| '&' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '&' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `and_then, lazy (process state index))
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `and_assign, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `ampersand, lazy (process state index))
				end
			| '!' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `ne, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `exclamation, lazy (process state index))
				end
			| '=' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '=' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `eq, lazy (process state index))
				| _ ->
					`cons ((p1, p1), `assign, lazy (process state index))
				end
			| '#' ->
				let first_of_line = state = `first in
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '#' ->
					let p2 = TextFile.position source index in
					let index = TextFile.succ source index in
					`cons ((p1, p2), `d_sharp, lazy (process state index))
				| _ ->
					if first_of_line then (
						let index = TextFile.succ_while Triming.is_space source index in
						Buffer.reset buf;
						let index = TextFile.succ_while_to_buffer is_tailing buf source index in
						let d = Buffer.contents buf in
						if d = "" then (
							`cons ((p1, p1), `sharp, lazy (process state index))
						) else (
							begin match ppdirective_of_string d with
							| #preprocessor_directive as d ->
								let p2 = TextFile.position source index in
								begin match d with
								| `sharp_INCLUDE | `sharp_INCLUDE_NEXT ->
									let index = TextFile.succ_while Triming.is_space source index in
									let p3 = TextFile.position source index in
									Buffer.reset buf;
									let index = read_header_to_buffer buf index in
									let header = Buffer.contents buf in (* empty if error *)
									let p4 = TextFile.prev_position source index in
									`cons ((p1, p2), (d :> LexicalElement.t), lazy (
										`cons ((p3, p4), `directive_parameter header, lazy (process `pp index))))
								| `sharp_WARNING | `sharp_ERROR ->
									let p3 = TextFile.position source index in
									Buffer.reset buf;
									let index = TextFile.succ_until_line_to_buffer is_lineescape buf source index in
									let message = Buffer.contents buf in
									let message = Triming.trim Triming.is_space message in
									let p4 = TextFile.prev_position source index in
									`cons ((p1, p2), (d :> LexicalElement.t), lazy (
										`cons ((p3, p4), `directive_parameter message, lazy (process `pp index))))
								| _ ->
									`cons ((p1, p2), (d :> LexicalElement.t), lazy (process `pp index))
								end
							| `none ->
								let p2 = TextFile.prev_position source index in
								error (p1, p2) (unknown_ppdirective d);
								let index = TextFile.succ_until_line is_lineescape source index in (* skip line *)
								process state index
							end
						)
					) else (
						`cons ((p1, p1), `sharp, lazy (process state index))
					)
				end
			| '0'..'9' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let the_numeric_literal, index = NumericScanner.scan_numeric_literal
					error
					buf
					TextFile.position
					TextFile.prev_position
					TextFile.get
					TextFile.succ
					source
					index
				in
				let p2 = TextFile.prev_position source index in
				`cons ((p1, p2), the_numeric_literal, lazy (process state index))
			| '\'' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				Buffer.reset buf;
				let index = read_string_to_buffer p1 ~quote:'\'' buf index in
				let s = Buffer.contents buf in
				let p2 = TextFile.prev_position source index in
				let length = String.length s in
				let the_char_literal =
					if length = 0 then (
						error (p1, p2) (illegal_char_literal s);
						`char_literal '\x00'
					) else if length = 1 then (
						`char_literal s.[0]
					) else (
						(* 'abcd' = 'a' << 32 | 'b' << 16 | 'c' << 8 | 'd' *)
						let rec loop i result =
							if i >= length then result else
							let result = Integer.logor
								(Integer.shift_left result 8)
								(Integer.of_int (int_of_char s.[i]))
							in
							loop (i + 1) result
						in
						`numeric_literal ("", `int_literal (`unsigned_int, loop 0 Integer.zero))
					)
				in
				`cons ((p1, p2), the_char_literal, lazy (process state index))
			| '\"' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				Buffer.reset buf;
				let index = read_string_to_buffer p1 ~quote:'\"' buf index in
				let s = Buffer.contents buf in
				let p2 = TextFile.prev_position source index in
				`cons ((p1, p2), `chars_literal s, lazy (process state index))
			| 'A'..'Z' | 'a'..'z' | '_' ->
				let state = nx state in
				let p1 = TextFile.position source index in
				Buffer.reset buf;
				let index = TextFile.succ_while_to_buffer is_tailing buf source index in
				let s = Buffer.contents buf in
				let is_L = s = "L" in
				if is_L && TextFile.get source index = '\'' then (
					Buffer.reset buf;
					let index = read_string_to_buffer p1 ~quote:'\'' buf index in
					let s = Buffer.contents buf in
					let p2 = TextFile.prev_position source index in
					if String.length s <> 1 then (
						error (p1, p2) (illegal_char_literal s)
					);
					let s = Array.init (String.length s) (fun i -> Int32.of_int (int_of_char s.[i])) in
					let s = WideString.of_array s in
					let c = (if s = WideString.empty then 0l else WideString.get s 0) in
					`cons ((p1, p2), `wchar_literal c, lazy (process state index))
				) else if is_L && TextFile.get source index = '\"' then (
					Buffer.reset buf;
					let index = read_string_to_buffer p1 ~quote:'\"' buf index in
					let s = Buffer.contents buf in
					let p2 = TextFile.prev_position source index in
					let s = Array.init (String.length s) (fun i -> Int32.of_int (int_of_char s.[i])) in
					let s = WideString.of_array s in
					`cons ((p1, p2), `wchars_literal s, lazy (process state index))
				) else (
					let p2 = TextFile.prev_position source index in
					let element = rw_of_string lang s in (* implies reserved_word, extended_word, identifier *)
					`cons ((p1, p2), (element :> LexicalElement.t), lazy (process state index))
				)
			| '@' when objc lang ->
				let state = nx state in
				let p1 = TextFile.position source index in
				let index = TextFile.succ source index in
				begin match TextFile.get source index with
				| '\"' ->
					Buffer.reset buf;
					let index = read_string_to_buffer p1 ~quote:'\"' buf index in
					let s = Buffer.contents buf in
					let p2 = TextFile.prev_position source index in
					`cons ((p1, p2), `objc_string_literal s, lazy (process state index))
				| 'A'..'Z' | 'a'..'z' | '_' ->
					Buffer.reset buf;
					let index = TextFile.succ_while_to_buffer is_tailing buf source index in
					let s = Buffer.contents buf in
					let p2 = TextFile.prev_position source index in
					begin match objcdirective_of_string s with
					| #objc_directive as d ->
						`cons ((p1, p2), (d :> LexicalElement.t), lazy (process state index))
					| `none ->
						error (p1, p2) (unknown_objcdirective s);
						process state index
					end
				| _ ->
					error (p1, p1) (unexpected '@');
					process state index
				end
			| '\x1a' ->
				let p = TextFile.position source index in
				finalize ();
				let ps = p, p in
				if state = `pp then (
					`cons (ps, `end_of_line, lazy (next ps))
				) else (
					next ps
				)
			| _ as h ->
				let p = TextFile.position source index in
				let index = TextFile.succ source index in
				error (p, p) (unexpected h);
				process state index
			end
		) in
		process `first 0
	);;
	
end;;
