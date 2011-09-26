open C_analyzer;;
open C_define_parser;;
open C_lexical;;
open C_parser;;
open C_preprocessor;;
open C_scanner;;
open C_semantics;;
open C_syntax;;
open Environment;;
open Environment_gcc;;
open Position;;
open Value_ocaml;;

let source_filename = ref "../c-lib.h";;
let gcc_command = ref "gcc";;
let tab_width = 3;;
let sys_include_dirs = ref [];;

if Filename.basename Sys.argv.(0) <> "ocaml" then (
	let rec parse_args i = (
		if i < Array.length Sys.argv then (
			begin match Sys.argv.(i) with
			| "--gcc" ->
				gcc_command := Sys.argv.(i + 1);
				parse_args (i + 2)
			| "--isystem" ->
				sys_include_dirs := Sys.argv.(i + 1) :: !sys_include_dirs;
				parse_args (i + 2)
			| arg ->
				source_filename := arg;
				parse_args (i + 1)
			end
		)
	) in
	parse_args 1
);;

let error (ps: ranged_position) (m: string): unit = (
	let ((f, _, l, c), _) = ps in
	Printf.printf "%s:%d:%d: %s\n" f l c m
);;

let env: environment = gcc_env !gcc_command `c;;
let env = {env with
	en_include = "." :: env.en_include;
	en_sys_include = List.rev_append !sys_include_dirs env.en_sys_include};;

module Literals = struct
	module Integer = Integer64;;
	module Real = Real;;
	module WideString = WideString;;
	let integer_of_real = Int64.of_float;;
	let real_of_integer = Int64.to_float;;
	let round_to_float x = x;;
	let round_to_double x = x;;
end;;

module LE = LexicalElement (Literals);;
module AST = Syntax (Literals);;
module SEM = Semantics (Literals);;
module S = Scanner (Literals) (LE);;
module PP = Preprocessor (Literals) (LE);;
module DP = DefineParser (Literals) (LE) (PP) (AST);;
module P = DP.Parser;;
module A = Analyzer (Literals) (AST) (SEM);;

let read_file (name: string): S.prim = (
	let h = open_in name in
	S.scan error `c name tab_width (fun () -> close_in h) (input h)
);;

let read_include_file = make_include read_file env;;

let predefined_tokens = lazy (S.scan error `c predefined_name tab_width ignore (read env.en_predefined));;
let predefined_tokens' = lazy (PP.preprocess error `c read_include_file false StringMap.empty StringMap.empty predefined_tokens);;

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

let lib_tokens = lazy (read_file !source_filename);;
let lib_tokens' = lazy (PP.preprocess error `c read_include_file false predefined StringMap.empty lib_tokens);;

let (tu: AST.translation_unit),
	(typedefs: P.typedef_set),
	(lazy (`nil (_, defined_tokens)): (ranged_position, PP.define_map) LazyList.nil) = P.parse_translation_unit error `c lib_tokens';;

let defines: DP.define AST.p StringMap.t = DP.map error `c typedefs defined_tokens;;

let (predefined_types: A.predefined_types),
	(derived_types: A.derived_types),
	(namespace: A.namespace),
	(sources: (SEM.source_item list * extra_info) StringMap.t),
	(mapping_options: SEM.mapping_options) = A.analyze error `c env.en_sizeof env.en_typedef env.en_builtin tu defines;;
