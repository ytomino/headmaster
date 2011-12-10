open C_lexical;;
open C_scanner;;
open Position;;
open Value;;
open Value_ocaml;;

let read data = (
	let cursor = ref 0 in
	fun dest index length -> (
		let really_length = (
			if !cursor + length <= String.length data then length else
			String.length data - !cursor
		) in
		String.blit data !cursor dest index really_length;
		cursor := !cursor + really_length;
		really_length
	)
);;

let error (ps: ranged_position) (m: string): unit = (
	let ((f, _, l, c), _) = ps in
	Printf.printf "%s:%d:%d: %s\n" f l c m
);;

module Test (L: LiteralsType) = struct
	module E = LexicalElement (L);;
	module S = Scanner (L) (E);;
	let tab_width = 3;;
	assert (
		match lazy (S.scan error `c "test" tab_width ignore (read "") S.make_nil) with
		| lazy (`nil ((("test", 0, 1, 1), _), _)) ->
			print_string "o";
			true
		| _ -> false);;
	assert (
		match lazy (S.scan error `c "test" tab_width ignore (read "().") S.make_nil) with
		| lazy (`cons (_, `l_paren,
			lazy (`cons (_, `r_paren,
				lazy (`cons (_, `period,
					lazy (`nil _))))))) ->
			print_string "o";
			true
		| _ -> false);;
	assert (
		match lazy (S.scan error `c "test" tab_width ignore (read "#define\n+") S.make_nil) with
		| lazy (`cons (_, `sharp_DEFINE,
			lazy (`cons (_, `end_of_line,
				lazy (`cons (_, `plus,
					lazy (`nil _))))))) ->
			print_string "o";
			true
		| _ -> false);;
	assert (
		match lazy (S.scan error `c "test" tab_width ignore (read "#define") S.make_nil) with
		| lazy (`cons (_, `sharp_DEFINE,
			lazy (`cons (_, `end_of_line,
				lazy (`nil _))))) ->
			print_string "o";
			true
		| _ -> false);;
	assert (
		match lazy (S.scan error `c "test" tab_width ignore (read "*/**/*") S.make_nil) with
		| lazy (`cons (_, `asterisk,
			lazy (`cons (_, `asterisk,
				lazy (`nil _))))) ->
			print_string "o";
			true
		| _ -> false);;
	assert (
		match lazy (S.scan error `c "test" tab_width ignore (read "\"a\\nb\"") S.make_nil) with
		| lazy (`cons (_, `chars_literal "a\nb",
			lazy (`nil _))) ->
			print_string "o";
			true
		| _ -> false);;
	assert (
		match lazy (S.scan error `objc "test" tab_width ignore (read "@catch @finally") S.make_nil) with
		| lazy (`cons (_, `at_CATCH,
			lazy (`cons (_, `at_FINALLY,
				lazy (`nil _))))) ->
			print_string "o";
			true
		| _ -> false);;
	assert (
		match lazy (S.scan error `c "test" tab_width ignore (read "bool _Bool") S.make_nil) with
		| lazy (`cons (_, `ident "bool",
			lazy (`cons (_, `_BOOL,
				lazy (`nil _))))) ->
			print_string "o";
			true
		| _ -> false);;
	assert (
		match lazy (S.scan error `cxx "test" tab_width ignore (read "bool") S.make_nil) with
		| lazy (`cons (_, `BOOL,
			lazy (`nil _))) ->
			print_string "o";
			true
		| _ -> false);;
	print_newline ();;
end;;

print_string "---- ocaml ----\n";;

module Literals_ocaml = struct
	module Integer = Integer;;
	module Real = Real;;
	module WideString = WideString;;
	let integer_of_real = int_of_float;;
	let real_of_integer = float_of_int;;
	let round_to_float x = x;;
	let round_to_double x = x;;
end;;

module Test1 = Test (Literals_ocaml);;

print_string "---- ocaml (int64) ----\n";;

module Literals_ocaml64 = struct
	module Integer = Integer64;;
	module Real = Real;;
	module WideString = WideString;;
	let integer_of_real = Int64.of_float;;
	let real_of_integer = Int64.to_float;;
	let round_to_float x = x;;
	let round_to_double x = x;;
end;;

module Test2 = Test (Literals_ocaml64);;

print_string "---- gmp ----\n";;

module Literals_gmp = struct
	module Integer = Gmp.Z;;
	module Real = Gmp.F (struct let prec = 64 end);;
	module WideString = Unicode.UTF32;;
	let integer_of_real = Gmp.z_of_truncated_f;;
	let real_of_integer = Real.of_z;;
	let round_to_float = Gmp.f_of_f ~prec:24;;
	let round_to_double = Gmp.f_of_f ~prec:53;;
end;;

module Test3 = Test (Literals_gmp);;
