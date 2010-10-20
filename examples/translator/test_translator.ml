open C_analyzer;;
open C_define_parser;;
open C_lexical;;
open C_parser;;
open C_preprocessor;;
open C_scanner;;
open C_semantics;;
open C_syntax;;
open Translator_to_ada;;
open Environment;;
open Environment_gcc;;

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
	let float_prec, double_prec, long_double_prec = env.en_precision;;
	module Integer = Gmp.Z;;
	module Long_double = struct let prec = long_double_prec end;;
	module FR_long_double = Mpfr.FR (Long_double);;
	module Real = FR_long_double.F (struct let rounding_mode = `N end);;
	module WideString = Unicode.UTF32;;
	let integer_of_real = Mpfr.z_of_truncated_fr;;
	let real_of_integer = Real.of_z;;
	let round_to_float = Mpfr.fr_of_fr ~prec:float_prec ~mode:`N;;
	let round_to_double = Mpfr.fr_of_fr ~prec:double_prec ~mode:`N;;
end;;

module LE = LexicalElement (Literals);;
module AST = Syntax (Literals);;
module SEM = Semantics (Literals);;
module S = Scanner (Literals) (LE);;
module PP = Preprocessor (Literals) (LE);;
module DP = DefineParser (Literals) (LE) (PP) (AST);;
module P = DP.Parser;;
module A = Analyzer (Literals) (AST) (SEM);;
module T = Translate (Literals) (SEM);;

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

let (tu, typedefs, lazy (`nil (_, defined_tokens)): AST.translation_unit * P.typedef_set * (ranged_position, PP.define_map) LazyList.nil) = P.parse_translation_unit error `c lib_tokens';;

let defines: DP.define AST.p StringMap.t = DP.map error `c typedefs defined_tokens;;

let (predefined_types: A.predefined_types),
	(derived_types: A.derived_types),
	(namespace: A.namespace),
	(sources: (SEM.source_item list * extra_info) StringMap.t),
	(language_mapping: SEM.language_mapping StringMap.t) = A.analyze error `c env.en_sizeof env.en_typedef env.en_builtin tu defines;;

let opaque_types = A.opaque_types namespace;;

print_string "---- language mapping ----\n";;

let ada_mapping = try StringMap.find "ADA" language_mapping with Not_found -> SEM.no_language_mapping;;

List.iter (fun (_, ada_type) ->
	(* pretty printer for C types is unimplemented... *)
	print_string " -> ";
	print_string ada_type;
	print_newline ()
) ada_mapping.SEM.lm_type;;

print_string "---- filename mapping ----\n";;

let filename_mapping = T.filename_mapping (remove_include_dir env) ada_mapping sources;;

StringMap.iter (fun k v ->
	print_string k;
	print_string " -> ";
	print_string v;
	print_newline ()
) filename_mapping;;

print_string "---- dir packages ----\n";;

let ada_sources = ref [];;

let dirs = T.dir_packages filename_mapping;;

List.iter (fun x ->
	let filename = Filename.concat "build" (T.spec_filename x) in
	print_string filename;
	print_newline ();
	let f = open_out filename in
	let ff = Format.make_formatter (output f) (fun () -> flush f) in
	T.pp_dir_package_spec ff ~name:x;
	Format.pp_print_flush ff ();
	close_out f;
	ada_sources := filename :: !ada_sources
) dirs;;

print_string "---- packages ----\n";;

let items_per_package = T.items_per_package (remove_include_dir env) ada_mapping filename_mapping sources;;
let name_mapping = T.name_mapping filename_mapping items_per_package;;

StringMap.iter (fun package items ->
	let ads_filename = Filename.concat "build" (T.spec_filename package) in
	print_string ads_filename;
	let f = open_out ads_filename in
	let ff = Format.make_formatter (output f) (fun () -> flush f) in
	begin try
		T.pp_translated_package_spec
			ff
			~language_mapping:ada_mapping
			~name_mapping
			~predefined_types
			~derived_types
			~opaque_types
			~name:package
			items;
		Format.pp_print_flush ff ();
		print_string "...ok";
		print_newline ();
		ada_sources := ads_filename :: !ada_sources
	with e ->
		Format.pp_print_flush ff ();
		print_string "...error!";
		print_newline ();
		prerr_string (Printexc.to_string e);
		prerr_newline ();
		Printexc.print_backtrace stderr;
		flush stderr
	end;
	close_out f;
	if T.body_required items then (
		let adb_filename = Filename.concat "build" (T.body_filename package) in
		print_string adb_filename;
		let f = open_out adb_filename in
		let ff = Format.make_formatter (output f) (fun () -> flush f) in
		begin try
			T.pp_translated_package_body
				ff
				~name_mapping
				~name:package
				items;
			Format.pp_print_flush ff ();
			print_string "...ok";
			print_newline ();
			ada_sources := adb_filename :: !ada_sources
		with e ->
			Format.pp_print_flush ff ();
			print_string "...error!";
			print_newline ();
			prerr_string (Printexc.to_string e);
			prerr_newline ();
			Printexc.print_backtrace stderr;
			flush stderr
		end;
		close_out f;
	)
) items_per_package;;

print_string "---- check ----\n";;

flush stdout;;

let command =
	List.fold_left (fun command filename ->
		command ^ " " ^ filename
	) "gnatmake -gnatc -gnatwa -gnaty -D build" !ada_sources;;

ignore (Sys.command command);;
