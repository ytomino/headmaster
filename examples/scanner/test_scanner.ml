open C_lexical;;
open C_literals;;
open C_scanner;;
open Position;;
open Value_ocaml;;

let tab_width = 3;;
let value_kind: [`ocaml64 | `gmp] ref = ref `ocaml64;;

let rec parse_args i = (
	if i < Array.length Sys.argv then (
		begin match Sys.argv.(i) with
		| "--ocaml64" ->
			value_kind := `ocaml64;
			parse_args (i + 1);
		| "--gmp" ->
			value_kind := `gmp;
			parse_args (i + 1);
		| _ ->
			parse_args (i + 1)
		end
	)
) in
parse_args 1;;

let read (s: string): TextFile.t = (
	TextFile.of_string ~random_access:false ~tab_width "<test>" s
);;

let error (ps: ranged_position) (m: string): unit = (
	let ((f, _, l, c), _) = ps in
	Printf.printf "%s:%d:%d: %s\n" f l c m
);;

module Literals_ocaml64 = struct
	module Integer = Integer64;;
	module Real = Real;;
	module WideString = String32;;
	let integer_of_real = Int64.of_float;;
	let real_of_integer = Int64.to_float;;
	let round ~prec x = (ignore prec; x);;
	let float_repr = `mantissa 24, `emin ~-125;;
	let double_repr = `mantissa 53, `emin ~-1021;;
	let long_double_repr = double_repr;;
end;;

module Literals_gmp = struct
	module Integer = Gmp.Z;;
	module Real = Gmp.F (struct let prec = 64 end);;
	module WideString = Unicode.UTF32;;
	let integer_of_real = Gmp.z_of_truncated_f;;
	let real_of_integer = Real.of_z;;
	let round = Gmp.f_of_f;;
	let float_repr = `mantissa 24, `emin ~-125;;
	let double_repr = `mantissa 53, `emin ~-1021;;
	let long_double_repr = `mantissa 64, `emin ~-16381;;
end;;

module Literals =
	(val
		match !value_kind with
		| `ocaml64 -> (module Literals_ocaml64 : LiteralsType)
		| `gmp -> (module Literals_gmp : LiteralsType));;

module LE = LexicalElement (Literals);;

(* C *)

module Sc = Scanner (Literals) (LE) (struct let lang = `c and gnu_inline = false end);;

let scan_c (s: string): Sc.t = (
	let xs = lazy (Sc.scan error (read s) Sc.make_nil) in
	(* force all elements for interactive *)
	let (_: (ranged_position, unit) LazyList.nil_prim) = LazyList.find_nil xs in
	xs
);;

(* Objective-C *)

module Sobjc = Scanner (Literals) (LE) (struct let lang = `objc and gnu_inline = false end);;

let scan_objc (s: string): Sobjc.t = (
	let xs = lazy (Sobjc.scan error (read s) Sobjc.make_nil) in
	let (_: (ranged_position, unit) LazyList.nil_prim) = LazyList.find_nil xs in
	xs
);;

(* C++ *)

module Scxx = Scanner (Literals) (LE) (struct let lang = `cxx and gnu_inline = false end);;

let scan_cxx (s: string): Scxx.t = (
	let xs = lazy (Scxx.scan error (read s) Scxx.make_nil) in
	let (_: (ranged_position, unit) LazyList.nil_prim) = LazyList.find_nil xs in
	xs
);;
