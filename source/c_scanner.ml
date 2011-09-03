open C_lexical;;
open C_scanner_errors;;
open Input;;
open Position;;
open Value;;

module Scanner
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType (Literals).S) =
struct
	open Literals;;
	
	type prim = (ranged_position, LexicalElement.t, unit) LazyList.prim;;
	type t = (ranged_position, LexicalElement.t, unit) LazyList.t;;
	
	let scan
		(error: ranged_position -> string -> unit)
		(lang: language)
		(filename: string)
		(tab_width: int)
		(finalize: unit -> unit)
		(input: string -> int -> int -> int): prim =
	(
		let unexpected_error (ps: ranged_position) (c: char): unit = (
			error ps ("\'" ^ Char.escaped c ^ "\' is bad character.")
		) in
		let char_literal_error (ps: ranged_position) (c: string): unit = (
			error ps ("\'" ^ String.escaped c ^ "\' is invalid char literal.")
		) in
		let unclosed_string_error (ps: ranged_position) (s: string): unit = (
			error ps ("\"" ^ String.escaped s ^ "\" is not closed string.")
		) in
		let hexadecimal_literal_error (ps: ranged_position) (): unit = (
			if not (is_known_hexadecimal_literal_error ps) then (
				error ps "illegal hexadecimal literal."
			)
		) in
		let not_10_based_float_literal_error (ps: ranged_position) (): unit = (
			error ps "floating-point literal should be 10-based."
		) in
		let decimal_error (ps: ranged_position) (): unit = (
			error ps "the suffix of decimal literal should be one of DF, DD, DL."
		) in
		let include_error (ps: ranged_position) (): unit = (
			error ps "include form should be \"...\" or <...>."
		) in
		let unknown_objcdirective_error (ps: ranged_position) (d: string): unit = (
			error ps ("@" ^ d ^ " is unknown Objective-C directive.")
		) in
		let unknown_ppdirective_error (ps: ranged_position) (d: string): unit = (
			error ps ("#" ^ d ^ " is unknown preprocessor directive.")
		) in
		let stream = TextStream.create filename tab_width input in
		let tokens_in_line = ref 0 in
		let need_eol = ref false in
		let read_digits ~(base: int): string = (
			let b = Buffer.create 16 in
			while
				begin match TextStream.hd stream with
				| '0'..'9' as h when Char.code h - Char.code '0' < base ->
					TextStream.junk stream;
					Buffer.add_char b h;
					true
				| ('A'..'F' | 'a'..'f') as h when base = 16 ->
					TextStream.junk stream;
					Buffer.add_char b h;
					true
				| _ ->
					false
				end
			do () done;
			Buffer.contents b
		) in
		let read_string (p1: position) ~(quote: char): string = (
			assert (TextStream.hd stream = quote);
			TextStream.junk stream;
			let rec loop b = (
				begin match TextStream.hd stream with
				| '\\' ->
					TextStream.junk stream;
					begin match TextStream.hd stream with
					| 'a' ->
						Buffer.add_char b '\x07';
						TextStream.junk stream
					| 'b' ->
						Buffer.add_char b '\x08';
						TextStream.junk stream
					| 'f' ->
						Buffer.add_char b '\x0c';
						TextStream.junk stream
					| 'n' ->
						Buffer.add_char b '\n';
						TextStream.junk stream
					| 'r' ->
						Buffer.add_char b '\r';
						TextStream.junk stream
					| 't' ->
						Buffer.add_char b '\t';
						TextStream.junk stream
					| 'v' ->
						Buffer.add_char b '\x0b';
						TextStream.junk stream
					| 'x' ->
						TextStream.junk stream;
						let r = ref 0 in
						while
							begin match TextStream.hd stream with
							| '0'..'9' as h -> r := !r * 16 + (Char.code h - Char.code '0'); true
							| 'A'..'F' as h -> r := !r * 16 + (Char.code h - (Char.code 'A' - 10)); true
							| 'a'..'f' as h -> r := !r * 16 + (Char.code h - (Char.code 'a' - 10)); true
							| _ -> false
							end
						do TextStream.junk stream done;
						Buffer.add_char b (Char.chr !r)
					| '0'..'7' ->
						let r = ref 0 in
						while
							begin match TextStream.hd stream with
							| '0'..'7' as h -> r := !r * 8 + (Char.code h - Char.code '0'); true
							| _ -> false
							end
						do TextStream.junk stream done;
						Buffer.add_char b (Char.chr !r)
					| _ as h ->
						Buffer.add_char b h;
						TextStream.junk stream
					end;
					loop b
				| '\n' | '\r' | '\x0c' | '\x1a' ->
					let p2 = TextStream.position stream in
					let result = Buffer.contents b in
					unclosed_string_error (p1, p2) result;
					result
				| h when h = quote ->
					TextStream.junk stream;
					Buffer.contents b
				| _ as h ->
					Buffer.add_char b h;
					TextStream.junk stream;
					loop b
				end
			) in
			loop (Buffer.create 256)
		) in
		let read_word (): string = (
			let b = Buffer.create 256 in
			while
				begin match TextStream.hd stream with
				| ('A'..'Z' | 'a'..'z' | '_' | '0'..'9') as c ->
					Buffer.add_char b c;
					TextStream.junk stream;
					true
				| _ ->
					false
				end
			do () done;
			Buffer.contents b
		) in
		let read_line (): string = (
			let b = Buffer.create 256 in
			while
				begin match TextStream.hd stream with
				| '\n' | '\r' | '\x0c' | '\x1a' ->
					false
				| '\\' ->
					TextStream.junk stream;
					begin match TextStream.hd stream with
					| '\n' | '\r' | '\x0c' ->
						TextStream.junk stream;
						true
					| _ ->
						Buffer.add_char b '\\';
						true
					end
				| _ as c ->
					TextStream.junk stream;
					Buffer.add_char b c;
					true
				end
			do () done;
			Buffer.contents b
		) in
		let read_header (): string = (
			let b = Buffer.create 256 in
			let p1 = TextStream.position stream in
			let k = TextStream.hd stream in
			let pair = (if k = '<' then '>' else '\"') in
			TextStream.junk stream;
			Buffer.add_char b k;
			while
				begin match TextStream.hd stream with
				| '\n' | '\r' | '\x0c' | '\x1a' ->
					let p2 = TextStream.prev_position stream in
					include_error (p1, p2) ();
					Buffer.add_char b pair;
					false
				| _ as c ->
					TextStream.junk stream;
					Buffer.add_char b c;
					c <> pair
				end
			do () done;
			Buffer.contents b
		) in
		let rec process (): prim = (
			begin match TextStream.hd stream with
			| ' ' | '\t' | '\b' ->
				TextStream.junk stream;
				process ()
			| '\n' | '\r' | '\x0c' ->
				let making_token = !need_eol in
				need_eol := false;
				tokens_in_line := 0;
				if making_token then (
					let ps = TextStream.junk_newline_with_ranged_position stream in
					`cons (ps, `end_of_line, lazy (process ()))
				) else (
					TextStream.junk_newline stream;
					process ()
				)
			| '\\' when !need_eol -> (* macro-line continuation *)
				TextStream.junk stream;
				begin match TextStream.hd stream with
				| '\n' | '\r' | '\x0c' ->
					TextStream.junk stream
				| _ ->
					let p = TextStream.prev_position stream in
					unexpected_error (p, p) '\\'
				end;
				process ()
			| '(' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `l_paren, lazy (process ()))
			| ')' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `r_paren, lazy (process ()))
			| '[' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `l_bracket, lazy (process ()))
			| ']' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `r_bracket, lazy (process ()))
			| '{' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `l_curly, lazy (process ()))
			| '}' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `r_curly, lazy (process ()))
			| '~' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `tilde, lazy (process ()))
			| '?' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `question, lazy (process ()))
			| ',' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `comma, lazy (process ()))
			| ';' ->
				incr tokens_in_line;
				let p = TextStream.junk_with_position stream in
				`cons ((p, p), `semicolon, lazy (process ()))
			| '.' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '.' ->
					let p2 = TextStream.junk_with_position stream in
					begin match TextStream.hd stream with
					| '.' ->
						let p3 = TextStream.junk_with_position stream in
						`cons ((p1, p3), `varargs, lazy (process ()))
					| _ ->
						incr tokens_in_line;
						`cons ((p1, p1), `period, lazy (
							`cons ((p2, p2), `period, lazy (process ()))))
					end
				| '*' when cxx lang ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `period_ref, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `period, lazy (process ()))
				end
			| ':' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '>' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `r_bracket, lazy (process ()))
				| ':' when cxx lang ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `d_colon, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `colon, lazy (process ()))
				end
			| '<' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| ':' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `l_bracket, lazy (process ()))
				| '%' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `l_curly, lazy (process ()))
				| '<' ->
					let p2 = TextStream.junk_with_position stream in
					begin match TextStream.hd stream with
					| '=' ->
						let p3 = TextStream.junk_with_position stream in
						`cons ((p1, p3), `l_shift_assign, lazy (process ()))
					| _ ->
						`cons ((p1, p2), `l_shift, lazy (process ()))
					end
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `le, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `lt, lazy (process ()))
				end
			| '>' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '>' ->
					let p2 = TextStream.junk_with_position stream in
					begin match TextStream.hd stream with
					| '=' ->
						let p3 = TextStream.junk_with_position stream in
						`cons ((p1, p3), `r_shift_assign, lazy (process ()))
					| _ ->
						`cons ((p1, p2), `r_shift, lazy (process ()))
					end
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `ge, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `gt, lazy (process ()))
				end
			| '-' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '>' ->
					let p2 = TextStream.junk_with_position stream in
					begin match TextStream.hd stream with
					| '*' when cxx lang ->
						let p3 = TextStream.junk_with_position stream in
						`cons ((p1, p3), `arrow_ref, lazy (process ()))
					| _ ->
						`cons ((p1, p2), `arrow, lazy (process ()))
					end
				| '-' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `decrement, lazy (process ()))
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `sub_assign, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `minus, lazy (process ()))
				end
			| '+' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '+' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `increment, lazy (process ()))
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `add_assign, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `plus, lazy (process ()))
				end
			| '*' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `mul_assign, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `asterisk, lazy (process ()))
				end
			| '/' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '/' ->
					TextStream.junk stream;
					let (_: string) = read_line () in
					process ()
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `div_assign, lazy (process ()))
				| '*' ->
					TextStream.junk stream;
					while
						begin match TextStream.hd stream with
						| '\x1a' -> false
						| '*' ->
							TextStream.junk stream;
							begin match TextStream.hd stream with
							| '/' ->
								TextStream.junk stream;
								false;
							| _ ->
								true
							end
						| _ ->
							TextStream.junk stream;
							true
						end
					do () done;
					process ()
				| _ ->
					`cons ((p1, p1), `slash, lazy (process ()))
				end
			| '%' ->
				let rec scan_after_percent (p1: position): prim = (
					begin match TextStream.hd stream with
					| ':' ->
						let p2 = TextStream.junk_with_position stream in
						begin match TextStream.hd stream with
						| '%' ->
							let p3 = TextStream.junk_with_position stream in
							begin match TextStream.hd stream with
							| ':' ->
								let p4 = TextStream.junk_with_position stream in
								`cons ((p1, p4), `d_sharp, lazy (process ()))
							| _ ->
								incr tokens_in_line;
								(* #%... *)
								`cons ((p1, p2), `sharp, lazy (scan_after_percent p3))
							end
						| _ ->
							`cons ((p1, p2), `sharp, lazy (process ()))
						end
					| '>' ->
						let p2 = TextStream.junk_with_position stream in
						`cons ((p1, p2), `r_curly, lazy (process ()))
					| '=' ->
						let p2 = TextStream.junk_with_position stream in
						`cons ((p1, p2), `rem_assign, lazy (process ()))
					| _ ->
						`cons ((p1, p1), `percent, lazy (process ()))
					end
				) in
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				scan_after_percent p1
			| '^' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `xor_assign, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `caret, lazy (process ()))
				end
			| '|' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '|' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `or_else, lazy (process ()))
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `or_assign, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `vertical, lazy (process ()))
				end
			| '&' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '&' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `and_then, lazy (process ()))
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `and_assign, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `ampersand, lazy (process ()))
				end
			| '!' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `ne, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `exclamation, lazy (process ()))
				end
			| '=' ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '=' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `eq, lazy (process ()))
				| _ ->
					`cons ((p1, p1), `assign, lazy (process ()))
				end
			| '#' ->
				let first_of_line = !tokens_in_line = 0 in
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '#' ->
					let p2 = TextStream.junk_with_position stream in
					`cons ((p1, p2), `d_sharp, lazy (process ()))
				| _ ->
					if first_of_line then (
						let junk_spaces () = (
							while
								begin match TextStream.hd stream with
								| ' ' | '\t' ->
									TextStream.junk stream;
									true
								| _ ->
									false
								end
							do () done
						) in
						junk_spaces ();
						let d = read_word () in
						if d = "" then (
							`cons ((p1, p1), `sharp, lazy (process ()))
						) else if is_ppdirective d then (
							need_eol := true;
							let d = ppdirective_of_string d in
							let p2 = TextStream.prev_position stream in
							begin match d with
							| `sharp_INCLUDE | `sharp_INCLUDE_NEXT ->
								junk_spaces ();
								let p3 = TextStream.position stream in
								let header =
									begin match TextStream.hd stream with
									| '\"' | '<' ->
										read_header ()
									| _ ->
										include_error (p3, p3) ();
										let (_: string) = read_line () in
										""
									end
								in
								let p4 = TextStream.prev_position stream in
								`cons ((p1, p2), (d :> LexicalElement.t), lazy (
									`cons ((p3, p4), `directive_parameter header, lazy (process ()))))
							| `sharp_WARNING | `sharp_ERROR ->
								let p3 = TextStream.position stream in
								let message = Triming.trim (read_line ()) in
								let p4 = TextStream.prev_position stream in
								`cons ((p1, p2), (d :> LexicalElement.t), lazy (
									`cons ((p3, p4), `directive_parameter message, lazy (process ()))))
							| _ ->
								`cons ((p1, p2), (d :> LexicalElement.t), lazy (process ()))
							end
						) else (
							let p2 = TextStream.prev_position stream in
							unknown_ppdirective_error (p1, p2) d;
							let (_: string) = read_line () in (* skip line *)
							process ()
						)
					) else (
						`cons ((p1, p1), `sharp, lazy (process ()))
					)
				end
			| '0'..'9' as h ->
				incr tokens_in_line;
				let p1 = TextStream.position stream in
				let base =
					if h = '0' then (
						TextStream.junk stream;
						begin match TextStream.hd stream with
						| 'x' | 'X' ->
							TextStream.junk stream;
							16
						| '.' ->
							10
						| _ ->
							8
						end
					) else (
						10
					)
				in
				let integer_part =
					let s = read_digits ~base in
					if s <> "" then s else (
						if base = 16 then (
							hexadecimal_literal_error (p1, TextStream.prev_position stream) ()
						);
						"0"
					)
				in
				begin match TextStream.hd stream with
				| '.' | 'e' | 'E' | 'p' | 'P' as h ->
					let v =
						if h = '.' then (
							TextStream.junk stream;
							let decimal_part = read_digits ~base:10 in
							Real.of_based_string ~base:10 (integer_part ^ "." ^ decimal_part)
						) else (
							Real.of_based_string ~base:10 integer_part
						)
					in
					if base <> 10 then (
						not_10_based_float_literal_error (p1, TextStream.prev_position stream) ()
					);
					let v =
						begin match TextStream.hd stream with
						| 'e' | 'E' ->
							TextStream.junk stream;
							let sign =
								begin match TextStream.hd stream with
								| '+' ->
									TextStream.junk stream;
									1
								| '-' ->
									TextStream.junk stream;
									-1
								| _ ->
									1
								end
							in
							let e = int_of_string (read_digits ~base:10) in
							Real.scale v ~base:10 ~exponent:(sign * e)
						| 'p' | 'P' ->
							TextStream.junk stream;
							let sign =
								begin match TextStream.hd stream with
								| '+' ->
									TextStream.junk stream;
									1
								| '-' ->
									TextStream.junk stream;
									-1
								| _ ->
									1
								end
							in
							let e = int_of_string (read_digits ~base:10) in
							Real.scale v ~base:2 ~exponent:(sign * e)
						| _ ->
							v
						end
					in
					begin match TextStream.hd stream with
					| 'd' | 'D' ->
						TextStream.junk stream;
						begin match TextStream.hd stream with
						| 'f' | 'F' ->
							TextStream.junk stream;
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `float_literal (`decimal32, v), lazy (process ()))
						| 'd' | 'D' ->
							TextStream.junk stream;
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `float_literal (`decimal64, v), lazy (process ()))
						| 'l' | 'L' ->
							TextStream.junk stream;
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `float_literal (`decimal128, v), lazy (process ()))
						| _ ->
							let p2 = TextStream.prev_position stream in
							decimal_error (p1, p2) ();
							`cons ((p1, p2), `float_literal (`decimal32, v), lazy (process ()))
						end
					| 'f' | 'F' ->
						TextStream.junk stream;
						let v = round_to_float v in
						begin match TextStream.hd stream with
						| 'i' | 'I' ->
							TextStream.junk stream;
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `imaginary_literal (`float, v), lazy (process ()))
						| _ ->
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `float_literal (`float, v), lazy (process ()))
						end
					| 'i' | 'I' ->
						(* is imaginary literal gcc's extended?? *)
						TextStream.junk stream;
						begin match TextStream.hd stream with
						| 'f' | 'F' ->
							TextStream.junk stream;
							let v = round_to_float v in
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `imaginary_literal (`float, v), lazy (process ()))
						| 'l' | 'L' ->
							TextStream.junk stream;
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `imaginary_literal (`long_double, v), lazy (process ()))
						| _ ->
							let v = round_to_double v in
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `imaginary_literal (`double, v), lazy (process ()))
						end
					| 'l' | 'L' ->
						begin match TextStream.hd stream with
						| 'i' | 'I' ->
							TextStream.junk stream;
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `imaginary_literal (`long_double, v), lazy (process ()))
						| _ ->
							TextStream.junk stream;
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `float_literal (`long_double, v), lazy (process ()))
						end
					| _ ->
						let v = round_to_double v in
						let p2 = TextStream.prev_position stream in
						`cons ((p1, p2), `float_literal (`double, v), lazy (process ()))
					end
				| 'U' | 'u' ->
					TextStream.junk stream;
					begin match TextStream.hd stream with
					| 'L' | 'l' ->
						TextStream.junk stream;
						begin match TextStream.hd stream with
						| 'L' | 'l' ->
							TextStream.junk stream;
							let v = Integer.of_based_string ~base integer_part in
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `int_literal (`unsigned_long_long, v), lazy (process ()))
						| _ ->
							let v = Integer.of_based_string ~base integer_part in
							let p2 = TextStream.prev_position stream in
							`cons ((p1, p2), `int_literal (`unsigned_long, v), lazy (process ()))
						end
					| _ ->
						let v = Integer.of_based_string ~base integer_part in
						let p2 = TextStream.prev_position stream in
						`cons ((p1, p2), `int_literal (`unsigned_int, v), lazy (process ()))
					end
				| 'L' | 'l' ->
					TextStream.junk stream;
					begin match TextStream.hd stream with
					| 'L' | 'l' ->
						TextStream.junk stream;
						let v = Integer.of_based_string ~base integer_part in
						let p2 = TextStream.prev_position stream in
						`cons ((p1, p2), `int_literal (`signed_long_long, v), lazy (process ()))
					| _ ->
						let v = Integer.of_based_string ~base integer_part in
						let p2 = TextStream.prev_position stream in
						`cons ((p1, p2), `int_literal (`signed_long, v), lazy (process ()))
					end
				| _ ->
					let v = Integer.of_based_string ~base integer_part in
					let p2 = TextStream.prev_position stream in
					`cons ((p1, p2), `int_literal (`signed_int, v), lazy (process ()))
				end
			| '\'' ->
				incr tokens_in_line;
				let p1 = TextStream.position stream in
				let s = read_string p1 ~quote:'\'' in
				let p2 = TextStream.prev_position stream in
				if String.length s <> 1 then (
					char_literal_error (p1, p2) s
				);
				let c = (if s = "" then '\x00' else s.[0]) in
				`cons ((p1, p2), `char_literal c, lazy (process ()))
			| '\"' ->
				incr tokens_in_line;
				let p1 = TextStream.position stream in
				let s = read_string p1 ~quote:'\"' in
				let p2 = TextStream.prev_position stream in
				`cons ((p1, p2), `chars_literal s, lazy (process ()))
			| 'A'..'Z' | 'a'..'z' | '_' ->
				incr tokens_in_line;
				let p1 = TextStream.position stream in
				let s = read_word () in
				if s = "L" && TextStream.hd stream = '\'' then (
					let s = read_string p1 ~quote:'\'' in
					let p2 = TextStream.prev_position stream in
					if String.length s <> 1 then (
						char_literal_error (p1, p2) s
					);
					let s = Array.init (String.length s) (fun i -> Int32.of_int (int_of_char s.[i])) in
					let s = WideString.of_array s in
					let c = (if s = WideString.empty then 0l else WideString.get s 0) in
					`cons ((p1, p2), `wchar_literal c, lazy (process ()))
				) else if s = "L" && TextStream.hd stream = '\"' then (
					let s = read_string p1 ~quote:'\"' in
					let p2 = TextStream.prev_position stream in
					let s = Array.init (String.length s) (fun i -> Int32.of_int (int_of_char s.[i])) in
					let s = WideString.of_array s in
					`cons ((p1, p2), `wchars_literal s, lazy (process ()))
				) else (
					let p2 = TextStream.prev_position stream in
					if is_rw lang s then (
						let rw = rw_of_string lang s in
						`cons ((p1, p2), (rw :> LexicalElement.t), lazy (process ()))
					) else if is_ew s then (
						let ew = ew_of_string s in
						`cons ((p1, p2), (ew :> LexicalElement.t), lazy (process ()))
					) else (
						`cons ((p1, p2), `ident s, lazy (process ()))
					)
				)
			| '@' when objc lang ->
				incr tokens_in_line;
				let p1 = TextStream.junk_with_position stream in
				begin match TextStream.hd stream with
				| '\"' ->
					let s = read_string p1 ~quote:'\"' in
					let p2 = TextStream.prev_position stream in
					`cons ((p1, p2), `objc_string_literal s, lazy (process ()))
				| 'A'..'Z' | 'a'..'z' ->
					let s = read_word () in
					let p2 = TextStream.prev_position stream in
					if is_objcdirective s then (
						let d = objcdirective_of_string s in
						`cons ((p1, p2), (d :> LexicalElement.t), lazy (process ()))
					) else (
						unknown_objcdirective_error (p1, p2) s;
						process ();
					)
				| _ ->
					unexpected_error (p1, p1) '@';
					process ()
				end
			| '\x1a' ->
				let p = TextStream.position stream in
				finalize ();
				let ps = p, p in
				if !need_eol then (
					`cons (ps, `end_of_line, lazy (`nil (ps, ())))
				) else (
					`nil (ps, ())
				)
			| _ as h ->
				let p = TextStream.junk_with_position stream in
				unexpected_error (p, p) h;
				process ()
			end
		) in
		process ()
	);;
	
end;;
