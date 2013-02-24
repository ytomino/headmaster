open Ada_translator;;
open C_analyzer;;
open C_define_analyzer;;
open C_define_parser;;
open C_filename;;
open C_lexical;;
open C_parser;;
open C_preprocessor;;
open C_scanner;;
open C_semantics;;
open C_syntax;;
open Environment;;
open Environment_gcc;;
open Known_errors;;
open Position;;

let source_filename = ref "../c-lib.h";;
let gcc_command = ref "gcc";;
let tab_width = 3;;
let destdir = "import";;
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
module PP = Preprocessor (Literals) (LE) (S.NumericScanner);;
module P = Parser (Literals) (LE) (AST);;
module DP = DefineParser (Literals) (LE) (PP) (AST) (P);;
module A = Analyzer (Literals) (AST) (SEM);;
module DA = DefineAnalyzer (Literals) (AST) (SEM) (A);;
module T = AdaTranslator (Literals) (SEM);;

let remove_include_dir = make_remove_include_dir env;;
let is_known_error = make_is_known_error env.en_target remove_include_dir;;

let read_file (name: string): (ranged_position -> S.prim) -> S.prim = (
	let file = TextFile.of_file ~random_access:false ~tab_width name in
	S.scan error ignore `c file
);;

let read_include_file = make_include read_file env;;

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

let lib_tokens: PP.in_t = lazy (read_file !source_filename S.make_nil);;
let lib_tokens': PP.out_t = lazy (PP.preprocess
	error is_known_error `c read_include_file `top_level predefined StringMap.empty lib_tokens);;

let (tu: AST.translation_unit),
	(typedefs: P.typedef_set),
	(lazy (`nil (_, defined_tokens)): (ranged_position, PP.define_map) LazyList.nil) = P.parse_translation_unit error `c lib_tokens';;

let defines: DP.define AST.p StringMap.t = DP.map error is_known_error `c typedefs defined_tokens;;

let (predefined_types: SEM.predefined_types),
	(derived_types: SEM.derived_types),
	(namespace: SEM.namespace),
	(sources: (SEM.source_item list * SEM.extra_info) StringMap.t),
	(mapping_options: SEM.mapping_options) = A.analyze error `c env.en_sizeof env.en_typedef env.en_builtin tu;;

let (derived_types: SEM.derived_types),
	(sources: (SEM.source_item list * SEM.extra_info) StringMap.t) = DA.map error is_known_error predefined_types derived_types namespace sources mapping_options defines;;

let (derived_types: SEM.derived_types),
	(sources: (SEM.source_item list * SEM.extra_info) StringMap.t) = A.rev derived_types sources;;

let opaque_mapping = A.opaque_mapping namespace;;

print_string "---- language mapping ----\n";;

let ada_mapping = SEM.find_langauge_mapping "ADA" mapping_options;;

List.iter (fun (_, ada_type) ->
	(* pretty printer for C types is unimplemented... *)
	print_string " -> ";
	print_string ada_type;
	print_newline ()
) ada_mapping.SEM.lm_type;;

print_string "---- filename mapping ----\n";;

let filename_mapping = T.filename_mapping remove_include_dir ada_mapping sources;;

StringMap.iter (fun k (rk, v) ->
	print_string k;
	print_string " (";
	print_string rk;
	print_string ") -> ";
	print_string v;
	print_newline ()
) filename_mapping;;

print_string "---- dir packages ----\n";;

let ada_sources = ref [];;

let dirs = T.dir_packages filename_mapping;;

List.iter (fun x ->
	let filename = Filename.concat destdir (T.spec_filename x) in
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

let items_per_package = T.items_per_package ada_mapping filename_mapping sources;;
let name_mapping = T.name_mapping filename_mapping opaque_mapping items_per_package;;

StringMap.iter (fun package items ->
	let ads_filename = Filename.concat destdir (T.spec_filename package) in
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
			~opaque_mapping
			~enum_of_element:namespace.SEM.ns_enum_of_element
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
		let adb_filename = Filename.concat destdir (T.body_filename package) in
		print_string adb_filename;
		let f = open_out adb_filename in
		let ff = Format.make_formatter (output f) (fun () -> flush f) in
		begin try
			T.pp_translated_package_body
				ff
				~opaque_mapping
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

let prefix = String.sub !gcc_command 0 (String.length !gcc_command - 3);;
let command =
	List.fold_left (fun command filename ->
		command ^ " " ^ filename
	) (prefix ^ "gnatmake -gnatc -gnatef -gnatwa -gnaty -D " ^ destdir) !ada_sources;;

ignore (Sys.command command);;
