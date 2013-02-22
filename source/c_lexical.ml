open C_literals;;

let snd_of_fst_table list = (
	let table = Hashtbl.create (List.length list) in
	List.iter (fun (s, w) -> Hashtbl.add table s w) list;
	table
);;

let fst_of_snd_table list = (
	let table = Hashtbl.create (List.length list) in
	List.iter (fun (s, w) -> Hashtbl.add table w s) list;
	table
);;

(* c *)

type compiler_macro = [
	| `__DATE__
	| `__FILE__
	| `__LINE__
	| `__STDC__
	| `__STDC_VERSION__
	| `__STDC_ISO_10646__
	| `__STDC_IEC_559__
	| `__STDC_IEC_559_COMPLEX__
	| `__TIME__
	| `__VA_ARGS__];;

type c_reserved_word = [
	| `AUTO
	| `BREAK
	| `CASE
	| `CHAR
	| `CONST
	| `CONTINUE
	| `DEFAULT
	| `DO
	| `DOUBLE
	| `ELSE
	| `ENUM
	| `EXTERN
	| `FLOAT
	| `FOR
	| `GOTO
	| `IF
	| `INLINE
	| `INT
	| `LONG
	| `REGISTER
	| `RESTRICT
	| `RETURN
	| `SHORT
	| `SIGNED
	| `SIZEOF
	| `STATIC
	| `STRUCT
	| `SWITCH
	| `TYPEDEF
	| `UNION
	| `UNSIGNED
	| `VOID
	| `VOLATILE
	| `WHILE
	| `_BOOL
	| `_COMPLEX
	| `_IMAGINARY
	| `_PRAGMA
	| compiler_macro];;
	
let c_reserved_word_table: (string * c_reserved_word) list = [
	"auto", `AUTO;
	"break", `BREAK;
	"case", `CASE;
	"char", `CHAR;
	"const", `CONST;
	"continue", `CONTINUE;
	"default", `DEFAULT;
	"do", `DO;
	"double", `DOUBLE;
	"else", `ELSE;
	"enum", `ENUM;
	"extern", `EXTERN;
	"float", `FLOAT;
	"for", `FOR;
	"goto", `GOTO;
	"if", `IF;
	"inline", `INLINE;
	"int", `INT;
	"long", `LONG;
	"register", `REGISTER;
	"restrict", `RESTRICT;
	"return", `RETURN;
	"short", `SHORT;
	"signed", `SIGNED;
	"sizeof", `SIZEOF;
	"static", `STATIC;
	"struct", `STRUCT;
	"switch", `SWITCH;
	"typedef", `TYPEDEF;
	"union", `UNION;
	"unsigned", `UNSIGNED;
	"void", `VOID;
	"volatile", `VOLATILE;
	"while", `WHILE;
	"_Bool", `_BOOL;
	"_Complex", `_COMPLEX;
	"_Imaginary", `_IMAGINARY;
	"_Pragma", `_PRAGMA;
	"__DATE__", `__DATE__;
	"__FILE__", `__FILE__;
	"__LINE__", `__LINE__;
	"__STDC__", `__STDC__;
	"__STDC_VERSION__", `__STDC_VERSION__;
	"__STDC_ISO_10646__", `__STDC_ISO_10646__;
	"__STDC_IEC_559__", `__STDC_IEC_559__;
	"__STDC_IEC_559_COMPLEX__", `__STDC_IEC_559_COMPLEX__;
	"__TIME__", `__TIME__;
	"__VA_ARGS__", `__VA_ARGS__];;

(* c++ *)

type cxx_only_reserved_word = [
	| `ASM
	| `BOOL
	| `CATCH
	| `CLASS
	| `CONST_CAST
	| `DELETE
	| `DYNAMIC_CAST
	| `EXPLICIT
	| `EXPORT
	| `FALSE
	| `FRIEND
	| `MUTABLE
	| `NAMESPACE
	| `NEW
	| `OPERATOR
	| `PRIVATE
	| `PROTECTED
	| `PUBLIC
	| `REINTERPRET_CAST
	| `STATIC_CAST
	| `TEMPLATE
	| `THIS
	| `THROW
	| `TRY
	| `TRUE
	| `TYPEID
	| `TYPENAME
	| `USING
	| `VIRTUAL
	| `WCHAR_T];;

let cxx_only_reserved_word_table: (string * cxx_only_reserved_word) list = [
	"asm", `ASM;
	"bool", `BOOL;
	"catch", `CATCH;
	"class", `CLASS;
	"const_cast", `CONST_CAST;
	"delete", `DELETE;
	"dynamic_cast", `DYNAMIC_CAST;
	"export", `EXPORT;
	"explicit", `EXPLICIT;
	"false", `FALSE;
	"friend", `FRIEND;
	"mutable", `MUTABLE;
	"namespace", `NAMESPACE;
	"new", `NEW;
	"operator", `OPERATOR;
	"private", `PRIVATE;
	"protected", `PROTECTED;
	"public", `PUBLIC;
	"reinterpret_cast", `REINTERPRET_CAST;
	"static_cast", `STATIC_CAST;
	"template", `TEMPLATE;
	"this", `THIS;
	"throw", `THROW;
	"true", `TRUE;
	"try", `TRY;
	"typeid", `TYPEID;
	"typename", `TYPENAME;
	"using", `USING;
	"virtual", `VIRTUAL;
	"wchar_t", `WCHAR_T];;

type cxx_reserved_word = [c_reserved_word | cxx_only_reserved_word];;

(* objective-c *)

type objc_only_reserved_word = [
	| `BYCOPY
	| `BYREF
	| `IN
	| `INOUT
	| `ONEWAY
	| `OUT
	| `__STRONG
	| `__WEAK];;

let objc_only_reserved_word_table: (string * objc_only_reserved_word) list = [
	"bycopy", `BYCOPY;
	"byref", `BYREF;
	"in", `IN;
	"inout", `INOUT;
	"oneway", `ONEWAY;
	"out", `OUT;
	"__strong", `__STRONG;
	"__weak", `__WEAK];;

type objc_reserved_word = [c_reserved_word | objc_only_reserved_word];;

type objc_directive = [
	| `at_CATCH
	| `at_CLASS
	| `at_DYNAMIC
	| `at_ENCODE
	| `at_END
	| `at_FINALLY
	| `at_IMPLEMENTATION
	| `at_INTERFACE
	| `at_PRIVATE
	| `at_PROPERTY
	| `at_PROTECTED
	| `at_PROTOCOL
	| `at_PUBLIC
	| `at_SELECTOR
	| `at_SYNCHRONIZED
	| `at_SYNTHESIZE
	| `at_THROW
	| `at_TRY];;

let objc_directive_table = [
	"catch", `at_CATCH;
	"class", `at_CLASS;
	"dynamic", `at_DYNAMIC;
	"encode", `at_ENCODE;
	"end", `at_END;
	"finally", `at_FINALLY;
	"implementation", `at_IMPLEMENTATION;
	"interface", `at_INTERFACE;
	"private", `at_PRIVATE;
	"property", `at_PROPERTY;
	"protected", `at_PROTECTED;
	"protocol", `at_PROTOCOL;
	"public", `at_PUBLIC;
	"selector", `at_SELECTOR;
	"synchronized", `at_SYNCHRONIZED;
	"synthesize", `at_SYNTHESIZE;
	"throw", `at_THROW;
	"try", `at_TRY];;

let string_of_objcdirective_table = fst_of_snd_table objc_directive_table;;
let objcdirective_of_string_table = snd_of_fst_table objc_directive_table;;

let string_of_objcdirective (s: objc_directive): string = (
	Hashtbl.find string_of_objcdirective_table s
);;

let objcdirective_of_string (s: string): [objc_directive | `none] = (
	begin try
		let result: objc_directive = Hashtbl.find objcdirective_of_string_table s in
		(result :> [objc_directive | `none])
	with Not_found ->
		`none
	end
);;

(* objective-c++ *)

type objcxx_reserved_word = [c_reserved_word | objc_only_reserved_word | cxx_only_reserved_word];;

(* extended (currently, gcc only) *)

type extended_word = [
	| `__asm
	| `__asm__
	| `__attribute__
	| `__builtin_va_arg
	| `__builtin_va_copy
	| `__builtin_va_end
	| `__builtin_va_list
	| `__builtin_va_start
	| `__const
	| `__extension__
	| `__imag__
	| `__inline
	| `__inline__
	| `__int64
	| `__real__
	| `__restrict
	| `__restrict__
	| `__typeof__
	| `__volatile__];;

let extended_word_table = [
	"__asm", `__asm;
	"__asm__", `__asm__;
	"__attribute__", `__attribute__;
	"__builtin_va_list", `__builtin_va_list;
	"__builtin_va_arg", `__builtin_va_arg;
	"__builtin_va_copy", `__builtin_va_copy;
	"__builtin_va_end", `__builtin_va_end;
	"__builtin_va_start", `__builtin_va_start;
	"__const", `__const;
	"__extension__", `__extension__;
	"__imag__", `__imag__;
	"__inline", `__inline;
	"__inline__", `__inline__;
	"__int64", `__int64;
	"__real__", `__real__;
	"__restrict", `__restrict;
	"__restrict__", `__restrict__;
	"__typeof__", `__typeof__;
	"__volatile__", `__volatile__];;

(* string_of/of_string for reserved word *)

type reserved_word = [objcxx_reserved_word | extended_word];;

let reserved_word_table: (string * (reserved_word * [language | `extended])) list =
	List.map (fun (s, k) -> s, ((k :> reserved_word), `c)) c_reserved_word_table @
	List.map (fun (s, k) -> s, ((k :> reserved_word), `cxx)) cxx_only_reserved_word_table @
	List.map (fun (s, k) -> s, ((k :> reserved_word), `objc)) objc_only_reserved_word_table @
	List.map (fun (s, k) -> s, ((k :> reserved_word), `extended)) extended_word_table;;

let string_of_rw_table = fst_of_snd_table (List.map (fun (s, (k, _)) -> s, k) reserved_word_table);;

let string_of_rw (k: reserved_word): string = (
	Hashtbl.find string_of_rw_table k
);;

let rw_of_string_table = snd_of_fst_table reserved_word_table;;

let rw_of_string (lang: language) (s: string): [reserved_word | `ident of string] = (
	begin try
		let k, k_lang = Hashtbl.find rw_of_string_table s in
		begin match lang with
		| `c ->
			begin match k_lang with
			| `c | `extended -> (k :> [reserved_word | `ident of string])
			| `cxx | `objc | `objcxx -> `ident s
			end
		| `cxx ->
			begin match k_lang with
			| `c | `cxx | `extended -> (k :> [reserved_word | `ident of string])
			| `objc | `objcxx -> `ident s
			end
		| `objc ->
			begin match k_lang with
			| `c | `objc | `extended -> (k :> [reserved_word | `ident of string])
			| `cxx | `objcxx -> `ident s
			end
		| `objcxx ->
			(k :> [reserved_word | `ident of string])
		end
	with Not_found ->
		`ident s
	end
);;

(* preprocessor directives *)

type preprocessor_directive = [
	| `sharp_DEFINE
	| `sharp_ELIF
	| `sharp_ELSE
	| `sharp_ENDIF
	| `sharp_ERROR
	| `sharp_IF
	| `sharp_IFDEF
	| `sharp_IFNDEF
	| `sharp_INCLUDE
	| `sharp_INCLUDE_NEXT (* extended *)
	| `sharp_LINE
	| `sharp_PRAGMA
	| `sharp_UNDEF
	| `sharp_WARNING];;

let preprocessor_directive_table = [
	"define", `sharp_DEFINE;
	"elif", `sharp_ELIF;
	"else", `sharp_ELSE;
	"endif", `sharp_ENDIF;
	"error", `sharp_ERROR;
	"if", `sharp_IF;
	"ifdef", `sharp_IFDEF;
	"ifndef", `sharp_IFNDEF;
	"include", `sharp_INCLUDE;
	"include_next", `sharp_INCLUDE_NEXT;
	"line", `sharp_LINE;
	"pragma", `sharp_PRAGMA;
	"undef", `sharp_UNDEF;
	"warning", `sharp_WARNING];;

let string_of_ppdirective_table = fst_of_snd_table preprocessor_directive_table;;
let ppdirective_of_string_table = snd_of_fst_table preprocessor_directive_table;;

let string_of_ppdirective (s: preprocessor_directive): string = (
	Hashtbl.find string_of_ppdirective_table s
);;

let ppdirective_of_string (s: string): [preprocessor_directive | `none] = (
	begin try
		let result: preprocessor_directive = Hashtbl.find ppdirective_of_string_table s in
		(result :> [preprocessor_directive | `none])
	with Not_found ->
		`none
	end
);;

module LexicalElement (Literals: LiteralsType) = struct
	open Literals;;
	
	type numeric_literal = [
		| `int_literal of int_prec * Integer.t
		| `float_literal of real_prec * Real.t
		| `imaginary_literal of float_prec * Real.t];;
	
	type t = [
		| reserved_word
		| extended_word
		| objc_directive
		| preprocessor_directive
		| `directive_parameter of string
		| `end_of_line
		| `ident of string
		| `numeric_literal of string * numeric_literal
		| `char_literal of char
		| `chars_literal of string
		| `wchar_literal of WideString.elm
		| `wchars_literal of WideString.t
		| `objc_string_literal of string (* @"..." *)
		| `l_paren
		| `r_paren
		| `l_bracket (* [ or <: *)
		| `r_bracket (* ] or :> *)
		| `l_curly (* { or <% *)
		| `r_curly (* } or %> *)
		| `period
		| `arrow (* -> *)
		| `increment (* ++ *)
		| `decrement (* -- *)
		| `ampersand
		| `asterisk
		| `plus
		| `minus
		| `tilde
		| `exclamation
		| `slash
		| `percent
		| `l_shift (* << *)
		| `r_shift (* >> *)
		| `lt
		| `gt
		| `le (* <= *)
		| `ge (* >= *)
		| `eq (* == *)
		| `ne (* != *)
		| `caret
		| `vertical
		| `and_then (* && *)
		| `or_else (* || *)
		| `question
		| `colon
		| `semicolon
		| `varargs (* ... *)
		| `assign (* = *)
		| `mul_assign (* *= *)
		| `div_assign (* /= *)
		| `rem_assign (* %= *)
		| `add_assign (* += *)
		| `sub_assign (* -= *)
		| `l_shift_assign (* <<= *)
		| `r_shift_assign (* >>= *)
		| `and_assign (* &= *)
		| `xor_assign (* ^= *)
		| `or_assign (* |= *)
		| `comma
		| `sharp (* # or %: *)
		| `d_sharp (* ## or %:%: *)
		| `d_colon (* C++ :: *)
		| `period_ref (* C++ .* *)
		| `arrow_ref];; (* C++ ->* *)
	
end;;

module type LexicalElementType = sig
	module Literals: LiteralsType;;
	include module type of LexicalElement (Literals);;
end;;
