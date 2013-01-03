open C_filename;;
open C_lexical;;
open C_preprocessor;;
open C_scanner;;
open Environment;;
open Environment_gcc;;
open Known_errors;;
open Position;;
open Value_ocaml;;

let source_filename = ref "../c-lib.h";;
let gcc_command = ref "gcc";;
let tab_width = 3;;
let sys_include_dirs = ref [];;

let rec parse_args i = (
	if i < Array.length Sys.argv then (
		begin match Sys.argv.(i) with
		| "--gcc" ->
			gcc_command := Sys.argv.(i + 1);
			parse_args (i + 2)
		| arg when String.length arg > 2 && arg.[0] = '-' && arg.[1] = 'I' ->
			sys_include_dirs := (String.sub arg 2 (String.length arg - 2)) :: !sys_include_dirs;
			parse_args (i + 1)
		| arg ->
			source_filename := arg;
			parse_args (i + 1)
		end
	)
) in
parse_args 1;;

let error (ps: ranged_position) (m: string): unit = (
	let ((f, _, l, c), _) = ps in
	Printf.printf "%s:%d:%d: %s\n" (compact_filename f) l c m
);;

let env: environment = gcc_env !gcc_command `c;;
let env = {env with
	en_include = "." :: env.en_include;
	en_sys_include = List.rev_append !sys_include_dirs env.en_sys_include};;

module Literals = struct
	module Integer = Integer;;
	module Real = Real;;
	module WideString = String32;;
	let integer_of_real = int_of_float;;
	let real_of_integer = float_of_int;;
	let round_to_float x = x;;
	let round_to_double x = x;;
end;;

module LE = LexicalElement (Literals);;
module S = Scanner (Literals) (LE);;
module PP = Preprocessor (Literals) (LE) (S.NumericScanner);;

let remove_include_dir = make_remove_include_dir env;;
let is_known_error = make_is_known_error env.en_target remove_include_dir;;

let read_file (name: string): (ranged_position -> S.prim) -> S.prim = (
	let file = TextFile.of_file ~random_access:false ~tab_width name in
	S.scan error ignore `c file
);;

let read_include_file = make_include (fun name ->
	print_string "* ";
	print_string name;
	print_newline ();
	read_file name
) env;;

let print_defined: PP.define_map -> unit = (
	StringMap.iter (fun _ item ->
		print_string item.PP.df_name;
		if item.PP.df_args <> [] || item.PP.df_varargs then (
			print_string "(";
			let f = ref true in
			List.iter (fun (_, name) ->
				if not !f then (
					print_string ", "
				);
				f := false;
				print_string name
			) item.PP.df_args;
			if item.PP.df_varargs then (
				if not !f then print_string ", ";
				print_string "..."
			);
			print_string ")"
		);
		print_newline ()
	)
);;

let diff (xs: PP.define_map) (ys: PP.define_map): PP.define_map = (
	StringMap.fold (fun name item rs ->
		if StringMap.mem name ys then rs else
		StringMap.add name item rs
	) xs StringMap.empty
);;

print_string "---- predefined ----\n";;

let predefined_tokens: PP.in_t =
	let file = TextFile.of_string ~random_access:false ~tab_width predefined_name env.en_predefined in
	lazy (S.scan error ignore `c file S.make_nil);;
let predefined_tokens': PP.out_t = lazy (PP.preprocess
	error is_known_error `c read_include_file `top_level StringMap.empty StringMap.empty predefined_tokens);;

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

print_defined predefined;;

print_string "---- stddef ----\n";;

let stddef_tokens: PP.in_t = lazy (read_include_file ~current:"" `system "stddef.h" S.make_nil);;
let stddef_tokens': PP.out_t = lazy (PP.preprocess
	error is_known_error `c read_include_file `top_level predefined StringMap.empty stddef_tokens);;
let `nil (_, stddef_defined) = LazyList.find_nil stddef_tokens';;

print_defined (diff stddef_defined predefined);;

print_string "---- standard libraries ----\n";;

let lib_tokens: PP.in_t = lazy (read_file !source_filename S.make_nil);;
let lib_tokens': PP.out_t = lazy (PP.preprocess
	error is_known_error `c read_include_file `top_level predefined StringMap.empty lib_tokens);;
let `nil (lib_defined, _) = LazyList.find_nil lib_tokens';;

(* for interpreter *)
let v (name: string): PP.define_item = (
	let r = StringMap.find name stddef_defined in
	ignore (LazyList.find_nil r.PP.df_contents);
	r
);;
