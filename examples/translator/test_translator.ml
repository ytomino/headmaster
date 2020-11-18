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
let destdir = ref "import.noindex";;
let include_dirs = ref [];;

let rec parse_args i = (
	if i < Array.length Sys.argv then (
		begin match Sys.argv.(i) with
		| "--gcc" ->
			gcc_command := Sys.argv.(i + 1);
			parse_args (i + 2)
		| "--destdir" ->
			destdir := Sys.argv.(i + 1);
			parse_args (i + 2)
		| arg when String.length arg > 2 && arg.[0] = '-' && arg.[1] = 'I' ->
			include_dirs := (String.sub arg 2 (String.length arg - 2)) :: !include_dirs;
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

let env: environment = gcc_env !gcc_command ~nostdinc:false ~x:`c;;
let env =
	{env with en_include = List.rev_append !include_dirs env.en_include};;

module Literals = struct
	let float_repr, double_repr, long_double_repr = env.en_fp;;
	module Integer = Gmp.Z;;
	module FR_long_double = Mpfr.FR (
		struct
			let prec = let `mantissa prec, _ = long_double_repr in prec;;
		end);;
	module Real = FR_long_double.F (struct let rounding_mode = `N end);;
	module WideString = Unicode.UTF32;;
	let integer_of_real = Mpfr.z_of_truncated_fr;;
	let real_of_integer = Real.of_z;;
	let round = Mpfr.fr_of_fr ~mode:`N;;
end;;

module Language = struct
	let lang = `c;;
	let gnu_inline = env.en_gnu_inline;;
end;;

module LE = LexicalElement (Literals);;
module AST = Syntax (Literals);;
module SEM = Semantics (Literals);;
module S = Scanner (Literals) (LE) (Language);;
module PP = Preprocessor (Literals) (LE) (S.NumericScanner);;
module P = Parser (Literals) (LE) (AST) (Language);;
module DP = DefineParser (Literals) (LE) (PP) (AST) (Language) (P);;
module A = Analyzer (Literals) (AST) (SEM) (Language);;
module DA = DefineAnalyzer (Literals) (AST) (SEM) (Language) (A);;
module T = AdaTranslator (Literals) (SEM);;

let remove_include_dir = make_remove_include_dir env;;
let is_known_error = make_is_known_error env.en_target remove_include_dir;;

let read_file (name: string): (ranged_position -> S.prim) -> S.prim = (
	let file = TextFile.of_file ~random_access:false ~tab_width name in
	S.scan error file
);;

let read_include_file = make_include read_file env;;

let predefined =
	let predefined_tokens: PP.in_t =
		let file = TextFile.of_string ~random_access:false ~tab_width predefined_name env.en_predefined in
		lazy (S.scan error file S.make_nil)
	in
	let predefined_tokens': PP.out_t =
		lazy (PP.preprocess
			error is_known_error read_include_file `top_level StringMap.empty StringMap.empty predefined_tokens)
	in
	begin match predefined_tokens' with
	| lazy (`nil (_, predefined)) ->
		predefined
	| lazy (`cons (_, _, xr)) ->
		print_string "extra token(s) exists in predefined!!\n";
		let `nil (_, predefined) = LazyList.find_nil xr in
		predefined
	end;;

let (tu: AST.translation_unit),
		(typedefs: P.typedef_set),
		(lazy (`nil (_, defined_tokens)): (ranged_position, PP.define_map) LazyList.nil) =
	let lib_tokens: PP.in_t = lazy (read_file !source_filename S.make_nil) in
	let lib_tokens': PP.out_t =
		lazy (PP.preprocess
			error is_known_error read_include_file `top_level predefined StringMap.empty lib_tokens)
	in
	P.parse_translation_unit error lib_tokens';;

let defines: DP.define AST.p StringMap.t = DP.map error is_known_error typedefs defined_tokens;;

let (predefined_types: SEM.predefined_types),
		(derived_types: SEM.derived_types),
		(namespace: SEM.namespace),
		(sources: (SEM.source_item list * SEM.extra_info) StringMap.t),
		(mapping_options: SEM.mapping_options) =
	A.analyze error env.en_sizeof env.en_typedef env.en_builtin tu;;

let (derived_types: SEM.derived_types),
		(sources: (SEM.source_item list * SEM.extra_info) StringMap.t) =
	DA.map error is_known_error predefined_types derived_types namespace sources mapping_options defines;;

let (derived_types: SEM.derived_types),
		(sources: (SEM.source_item list * SEM.extra_info) StringMap.t) =
	A.rev derived_types sources;;

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
	let filename = Filename.concat !destdir (T.spec_filename x) in
	print_string filename;
	flush stdout;
	let f = open_out filename in
	let ff = Format.make_formatter (output_substring f) (fun () -> flush f) in
	T.pp_dir_package_spec ff ~name:x;
	Format.pp_print_flush ff ();
	close_out f;
	print_newline ();
	ada_sources := filename :: !ada_sources
) dirs;;

print_string "---- packages ----\n";;

let items_per_package = T.items_per_package ada_mapping filename_mapping sources;;
let name_mapping = T.name_mapping filename_mapping opaque_mapping items_per_package;;

StringMap.iter (fun package items ->
	let ads_filename = Filename.concat !destdir (T.spec_filename package) in
	print_string ads_filename;
	flush stdout;
	let context_clauses =
		T.context_clauses
			~language_mapping:ada_mapping
			~predefined_types
			~derived_types
			~opaque_mapping
			~name_mapping
			~name:package
			items
	in
	let f = open_out ads_filename in
	let ff = Format.make_formatter (output_substring f) (fun () -> flush f) in
	begin try
		T.pp_translated_package_spec
			ff
			~language_mapping:ada_mapping
			~predefined_types
			~derived_types
			~enum_of_element:namespace.SEM.ns_enum_of_element
			~opaque_mapping
			~name_mapping
			~name:package
			items
			context_clauses;
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
		let adb_filename = Filename.concat !destdir (T.body_filename package) in
		print_string adb_filename;
		flush stdout;
		let f = open_out adb_filename in
		let ff = Format.make_formatter (output_substring f) (fun () -> flush f) in
		begin try
			T.pp_translated_package_body
				ff
				~opaque_mapping
				~name_mapping
				~name:package
				items
				context_clauses;
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
		close_out f
	)
) items_per_package;;

print_string "---- check ----\n";;

flush stdout;;

let prefix, arguments =
	let length = String.length !gcc_command in
	let index = Option.value ~default:length (String.index_opt !gcc_command ' ') in
	let prefix = String.sub !gcc_command 0 (index - 3) in
	let arguments = String.sub !gcc_command index (length - index) in
	prefix, arguments;;

assert (String.length arguments = 0 || arguments.[0] = ' ');;

let command =
	let command = Buffer.create 256 in
	Buffer.add_string command prefix;
	Buffer.add_string command "gnatmake -gnatc -gnatef -gnatwa -gnaty -D ";
	Buffer.add_string command !destdir;
	List.iter (fun filename ->
		Buffer.add_char command ' ';
		Buffer.add_string command filename
	) !ada_sources;
	Buffer.add_string command " -cargs";
	Buffer.add_string command arguments;
	Buffer.contents command;;

let _: int = Sys.command command;;
