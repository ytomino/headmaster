open C_lexical;;
open Value;;

let hex = "0123456789ABCDEF";;

let hex4 (x: Int32.t): string = (
	let s = String.make 4 '_' in
	s.[0] <- hex.[Int32.to_int (Int32.shift_right x 24) land 15];
	s.[1] <- hex.[Int32.to_int (Int32.shift_right x 16) land 15];
	s.[2] <- hex.[Int32.to_int (Int32.shift_right x  8) land 15];
	s.[3] <- hex.[Int32.to_int x land 15];
	s
);;

module Output
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType (Literals).S) =
struct
	open Literals;;
	
	let print_element (print_string: string -> unit) (e: LexicalElement.t): unit = (
		let image =
			begin match e with
			| #reserved_word as w ->
				string_of_rw w
			| #extended_word as w ->
				string_of_ew w
			| #objc_directive as w ->
				string_of_objcdirective w
			| #preprocessor_directive as w ->
				"#" ^ string_of_ppdirective w
			| `directive_parameter s ->
				s
			| `end_of_line ->
				"\n"
			| `ident w ->
				w
			| `numeric_literal (_, `int_literal (prec, value)) ->
				Integer.to_based_string ~base:10 value ^
				begin match prec with
				| `signed_char -> ""
				| `unsigned_char -> ""
				| `signed_short -> ""
				| `unsigned_short -> ""
				| `signed_int -> ""
				| `unsigned_int -> "U"
				| `signed_long -> "L"
				| `unsigned_long -> "UL"
				| `signed_long_long -> "LL"
				| `unsigned_long_long -> "ULL"
				end
			| `numeric_literal (_, `float_literal (prec, value)) ->
				let m, e = Real.frexp value in
				"0x" ^
				Real.to_based_string ~base:16 m ^
				"P" ^
				string_of_int e ^
				begin match prec with
				| `float -> "F"
				| `double -> ""
				| `long_double -> "L"
				| `decimal32 -> "DF"
				| `decimal64 -> "DD"
				| `decimal128 -> "DL"
				end
			| `numeric_literal (_, `imaginary_literal (prec, value)) ->
				let m, e = Real.frexp value in
				"0x" ^
				Real.to_based_string ~base:16 m ^
				"P" ^
				string_of_int e ^
				begin match prec with
				| `float -> "FI"
				| `double -> "I"
				| `long_double -> "LI"
				end
			| `char_literal s ->
				"\'" ^ Char.escaped s ^ "\'"
			| `chars_literal s ->
				"\"" ^ String.escaped s ^ "\""
			| `wchar_literal s ->
				"L\'\\x" ^ hex4 s ^ "\'"
			| `wchars_literal s ->
				let length = WideString.length s in
				let buf = Buffer.create (length * 6 + 3) in
				Buffer.add_string buf "L\"";
				for i = 0 to length do
					Buffer.add_string buf "\\x";
					Buffer.add_string buf (hex4 (WideString.get s i))
				done;
				Buffer.add_char buf '\"';
				Buffer.contents buf
			| `objc_string_literal s ->
				"@\"" ^ String.escaped s ^ "\""
			| `l_paren ->
				"("
			| `r_paren ->
				")"
			| `l_bracket ->
				"["
			| `r_bracket ->
				"]"
			| `l_curly ->
				"{"
			| `r_curly ->
				"}"
			| `period ->
				"."
			| `arrow ->
				"->"
			| `increment ->
				"++"
			| `decrement ->
				"--"
			| `ampersand ->
				"&"
			| `asterisk ->
				"*"
			| `plus ->
				"+"
			| `minus ->
				"-"
			| `tilde ->
				"~"
			| `exclamation ->
				"!"
			| `slash ->
				"/"
			| `percent ->
				"%"
			| `l_shift ->
				"<<"
			| `r_shift ->
				">>"
			| `lt ->
				"<"
			| `gt ->
				">"
			| `le ->
				"<="
			| `ge ->
				">="
			| `eq ->
				"=="
			| `ne ->
				"!="
			| `caret ->
				"^"
			| `vertical ->
				"|"
			| `and_then ->
				"&&"
			| `or_else ->
				"||"
			| `question ->
				"?"
			| `colon ->
				":"
			| `semicolon ->
				";"
			| `varargs ->
				"..."
			| `assign ->
				"="
			| `mul_assign ->
				"*="
			| `div_assign ->
				"/="
			| `rem_assign ->
				"%="
			| `add_assign ->
				"+="
			| `sub_assign ->
				"-="
			| `l_shift_assign ->
				"<<="
			| `r_shift_assign ->
				">>="
			| `and_assign ->
				"&="
			| `xor_assign ->
				"^="
			| `or_assign ->
				"|="
			| `comma ->
				","
			| `sharp ->
				"#"
			| `d_sharp ->
				"##"
			| `d_colon ->
				"::"
			| `period_ref ->
				".*"
			| `arrow_ref ->
				"->*"
			end
		in
		print_string image
	);;
	
end;;
