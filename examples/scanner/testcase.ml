open Test_scanner;;

assert (
	match scan_c "" with
	| lazy (`nil ((("<test>", 0, 1, 1), _), _)) ->
		print_string "o";
		true
	| _ -> false);;

assert (
	match scan_c "()." with
	| lazy (`cons (_, `l_paren,
		lazy (`cons (_, `r_paren,
			lazy (`cons (_, `period,
				lazy (`nil _))))))) ->
		print_string "o";
		true
	| _ -> false);;

assert (
	match scan_c "#define\n+" with
	| lazy (`cons (_, `sharp_DEFINE,
		lazy (`cons (_, `end_of_line,
			lazy (`cons (_, `plus,
				lazy (`nil _))))))) ->
		print_string "o";
		true
	| _ -> false);;

assert (
	match scan_c "#define" with
	| lazy (`cons (_, `sharp_DEFINE,
		lazy (`cons (_, `end_of_line,
			lazy (`nil _))))) ->
		print_string "o";
		true
	| _ -> false);;

assert (
	match scan_c "*/**/*" with
	| lazy (`cons (_, `asterisk,
		lazy (`cons (_, `asterisk,
			lazy (`nil _))))) ->
		print_string "o";
		true
	| _ -> false);;

assert (
	match scan_c "123 010U 0xfeL" with
	| lazy (`cons (_, `numeric_literal ("123", `int_literal (`signed_int, v1)),
		lazy (`cons (_, `numeric_literal ("010U", `int_literal (`unsigned_int, v2)),
			lazy (`cons (_, `numeric_literal ("0xfeL", `int_literal (`signed_long, v3)),
				lazy (`nil _)))))))
		when v1 = Literals.Integer.of_int 123
			&& v2 = Literals.Integer.of_int 0o10
			&& v3 = Literals.Integer.of_int 0xfe ->
		print_string "o";
		true
	| _ -> false);;

assert (
	match scan_c "0x1.fffffep+127f" with
	| lazy (`cons (_, `numeric_literal ("0x1.fffffep+127f", `float_literal (`float, _)),
		lazy (`nil _))) ->
		print_string "o";
		true
	| _ -> false);;

assert (
	match scan_c "\"a\\nb\"" with
	| lazy (`cons (_, `chars_literal "a\nb",
		lazy (`nil _))) ->
		print_string "o";
		true
	| _ -> false);;

assert (
	match scan_c "bool _Bool" with
	| lazy (`cons (_, `ident "bool",
		lazy (`cons (_, `_BOOL,
			lazy (`nil _))))) ->
		print_string "o";
		true
	| _ -> false);;

(* Objective-C *)

assert (
	match scan_objc "@catch @finally" with
	| lazy (`cons (_, `at_CATCH,
		lazy (`cons (_, `at_FINALLY,
			lazy (`nil _))))) ->
		print_string "o";
		true
	| _ -> false);;

(* C++ *)

assert (
	match scan_cxx "bool" with
	| lazy (`cons (_, `BOOL,
		lazy (`nil _))) ->
		print_string "o";
		true
	| _ -> false);;

print_newline ();;
