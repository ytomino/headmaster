open C_define_parser;;
open C_lexical;;
open C_parser;;
open C_preprocessor;;
open C_scanner;;
open C_syntax;;
open Environment;;
open Environment_gcc;;
open Position;;
open Value_ocaml;;

let source_filename = ref "../c-lib.h";;
let gcc_command = ref "gcc";;
let tab_width = 3;;

let rec parse_args i = (
	if i < Array.length Sys.argv then (
		begin match Sys.argv.(i) with
		| "--gcc" ->
			gcc_command := Sys.argv.(i + 1);
			parse_args (i + 2)
		| arg ->
			source_filename := arg;
			parse_args (i + 1)
		end
	)
) in
parse_args 1;;

let error (ps: ranged_position) (m: string): unit = (
	let ((f, _, l, c), _) = ps in
	Printf.printf "%s:%d:%d: %s\n" f l c m
);;

let env: environment = gcc_env !gcc_command `c;;
let env = {env with en_include = "." :: env.en_include};;

module Literals = struct
	module Integer = Integer;;
	module Real = Real;;
	module WideString = WideString;;
	let integer_of_real = int_of_float;;
	let real_of_integer = float_of_int;;
	let round_to_float x = x;;
	let round_to_double x = x;;
end;;

module LE = LexicalElement (Literals);;
module AST = Syntax (Literals);;
module S = Scanner (Literals) (LE);;
module PP = Preprocessor (Literals) (LE);;
module DP = DefineParser (Literals) (LE) (PP) (AST);;
module P = DP.Parser;;

let read_file (name: string): (ranged_position -> S.prim) -> S.prim = (
	let h = open_in name in
	S.scan error `c name tab_width (fun () -> close_in h) (input h)
);;

let read_include_file = make_include read_file env;;

let predefined_tokens: PP.in_t = lazy (S.scan
	error `c predefined_name tab_width ignore (read env.en_predefined) S.make_nil);;
let predefined_tokens': PP.out_t = lazy (PP.preprocess
	error `c read_include_file false StringMap.empty StringMap.empty predefined_tokens);;

let predefined = (
	begin match predefined_tokens' with
	| lazy (`nil (_, predefined)) ->
		predefined
	| lazy (`cons (_, _, xr)) ->
		print_string "extra token(s) exists in predefined!!\n";
		let `nil (_, predefined) = LazyList.find_nil xr in
		predefined
	end
);;

print_string "---- standard libraries ----\n";;

let lib_tokens: PP.in_t = lazy (read_file !source_filename S.make_nil);;
let lib_tokens': PP.out_t = lazy (PP.preprocess
	error `c read_include_file false predefined StringMap.empty lib_tokens);;

let (tu, typedefs, lazy (`nil (_, defined_tokens)): AST.translation_unit * P.typedef_set * (ranged_position, PP.define_map) LazyList.nil) = P.parse_translation_unit error `c lib_tokens';;

print_string "---- define ----\n";; flush stdout;;

let defines: DP.define AST.p StringMap.t = DP.map error `c typedefs defined_tokens;;
