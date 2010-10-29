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
open Version;;

let print_exception (e: exn): unit = (
	print_newline ();
	prerr_string (Printexc.to_string e);
	prerr_newline ();
	Printexc.print_backtrace stderr;
	flush stderr
);;

type options = {
	source_filenames: string list;
	tab_width: int;
	gcc_command: string;
	include_dirs: string list;
	sys_include_dirs: string list;
	lang: language;
	to_lang: [`none | `ada];
	dest_dir: string;
	create_dest_dir: bool;
	usage: bool;
	version: bool};;

let initial_options = {
	source_filenames = [];
	tab_width = 1;
	gcc_command = "gcc";
	include_dirs = [];
	sys_include_dirs = [];
	lang = `c;
	to_lang = `none;
	dest_dir = ".";
	create_dest_dir = false;
	usage = false;
	version = false};;

let option_spec =
	let open CommandLine in [
	case "c"
		=> (fun options -> {options with lang = `c});
	case "c++"
		=> (fun options -> {options with lang = `cxx});
	case "D" ~long:"destdir" ~desc:"Specify destination directory"
		=>? (fun arg options -> {options with dest_dir = arg});
	case "f" ~long:"from" ~desc:"Specify source language (c/c++/objc/objc++)"
		=>? (fun arg options ->
			let lang =
				begin match arg with
				| "c" -> `c
				| "c++" -> `cxx
				| "objc" -> `objc
				| "objc++" -> `objcxx
				| _ -> failwith ("\"" ^ arg ^ "\" is not supported as source language.")
				end
			in
			{options with lang = lang}
		);
	case "gcc" ~desc:"Specify using gcc (default)"
		=>? (fun arg options -> {options with gcc_command = arg});
	case "h" ~long:"help" ~desc:"Display this information"
		=> (fun options -> {options with usage = true});
	case "I" ~long:"include" ~desc:"Add path to user header search path list"
		=>? (fun arg options -> {options with
			include_dirs = arg :: options.include_dirs}
		);
	case "isystem" ~desc:"Add path to system header search path list"
		=>? (fun arg options -> {options with
			sys_include_dirs = arg :: options.sys_include_dirs}
		);
	case "objc"
		=> (fun options -> {options with lang = `objc});
	case "objc++"
		=> (fun options -> {options with lang = `objcxx});
	case "p" ~desc:"Create missing destination-path"
		=> (fun options -> {options with create_dest_dir = true});
	case "tab" ~desc:"Count tab as N characters (default: 1)"
		=>? (fun arg options -> {options with tab_width = int_of_string arg});
	case "t" ~long:"to" ~desc:"Specify destination language (ada)"
		=>? (fun arg options ->
			let lang =
				begin match arg with
				| "ada" -> `ada
				| _ -> failwith ("\"" ^ arg ^ "\" is not supported as destination language.")
				end
			in
			{options with to_lang = lang}
		);
	case "v" ~long:"version" ~desc:"Print the version number"
		=> (fun options -> {options with version = true});
	otherwise
		(fun arg options -> {options with source_filenames = arg :: options.source_filenames})];;

let options = CommandLine.parse option_spec Sys.argv initial_options;;

if options.usage || options.version ||
	options.source_filenames = [] || options.to_lang = `none
then (
	let error = not options.usage && not options.version &&
		(options.source_filenames = [] || options.to_lang = `none) in
	if error && options.source_filenames = [] then (
		print_string "no input file was specified.";
		print_newline ()
	);
	if error && options.to_lang = `none then (
		print_string "no destination language was specified.";
		print_newline ()
	);
	if options.version then (
		print_string "headmaster ";
		print_string version;
		print_newline ()
	);
	if options.usage || error then (
		print_string "usage: headmaster [options] <input-file>";
		print_newline ();
		print_newline ();
		print_string "options are:";
		print_newline ();
		CommandLine.print_usage option_spec
	);
	exit (if error then 1 else 0)
);;

let has_error = ref false;;

let error (ps: ranged_position) (m: string): unit = (
	let ((f, _, l, c), _) = ps in
	Printf.eprintf "%s:%d:%d: %s\n" f l c m;
	has_error := true
);;

let env: environment = gcc_env options.gcc_command options.lang;;
let env = {env with
	en_include = List.rev_append options.include_dirs env.en_include;
	en_sys_include = List.rev_append options.sys_include_dirs env.en_sys_include};;

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
	let close_f () = close_in h in
	S.scan error options.lang name options.tab_width close_f (input h)
);;

let read_include_file = make_include read_file env;;

let predefined_tokens = lazy (S.scan
	error options.lang predefined_name options.tab_width
	ignore (read env.en_predefined));;
let predefined_tokens' = lazy (PP.preprocess
	error options.lang read_include_file
	false StringMap.empty StringMap.empty predefined_tokens);;

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

let source_tokens = lazy (LazyList.concat (List.map
	(fun name -> lazy (read_file name))
	(List.rev options.source_filenames)));;

let source_tokens' = lazy (PP.preprocess
	error options.lang read_include_file
	false predefined StringMap.empty source_tokens);;

let (tu: AST.translation_unit),
	(typedefs: P.typedef_set),
	(lazy (`nil (_, defined_tokens)): (ranged_position, PP.define_map) LazyList.nil) = P.parse_translation_unit error `c source_tokens';;

let defines: DP.define AST.p StringMap.t = DP.map error `c typedefs defined_tokens;;

let (predefined_types: A.predefined_types),
	(derived_types: A.derived_types),
	(namespace: A.namespace),
	(sources: (SEM.source_item list * extra_info) StringMap.t),
	(language_mapping: SEM.language_mapping StringMap.t) = A.analyze error `c env.en_sizeof env.en_typedef env.en_builtin tu defines;;

let opaque_types = A.opaque_types namespace;;

if options.create_dest_dir && not (Sys.file_exists options.dest_dir) then (
	Unix.mkdir options.dest_dir 0o755
);;

begin match options.to_lang with
| `ada ->
	let ada_mapping = try StringMap.find "ADA" language_mapping with Not_found -> SEM.no_language_mapping in
	let filename_mapping = T.filename_mapping (remove_include_dir env) ada_mapping sources in
	let dirs = T.dir_packages filename_mapping in
	List.iter (fun x ->
		let filename = Filename.concat options.dest_dir (T.spec_filename x) in
		print_string "generating ";
		print_string filename;
		print_string "...";
		let f = open_out filename in
		let ff = Format.make_formatter (output f) (fun () -> flush f) in
		begin try
			T.pp_notification ff version;
			T.pp_dir_package_spec ff ~name:x;
			Format.pp_print_flush ff ();
			print_string "ok";
			print_newline ()
		with e ->
			Format.pp_print_flush ff ();
			print_string "...error!";
			print_newline ();
			print_exception e;
			has_error := true
		end;
		close_out f;
	) dirs;
	let items_per_package = T.items_per_package (remove_include_dir env) ada_mapping filename_mapping sources in
	let name_mapping = T.name_mapping filename_mapping items_per_package in
	StringMap.iter (fun package items ->
		let ads_filename = Filename.concat options.dest_dir (T.spec_filename package) in
		print_string "generating ";
		print_string ads_filename;
		print_string "...";
		let f = open_out ads_filename in
		let ff = Format.make_formatter (output f) (fun () -> flush f) in
		begin try
			T.pp_notification ff version;
			T.pp_translated_package_spec
				ff
				~language_mapping:ada_mapping
				~name_mapping
				~predefined_types
				~derived_types
				~opaque_types
				~enum_of_element:namespace.A.ns_enum_of_element
				~name:package
				items;
			Format.pp_print_flush ff ();
			print_string "ok";
			print_newline ()
		with e ->
			Format.pp_print_flush ff ();
			print_string "...error!";
			print_newline ();
			print_exception e;
			has_error := true
		end;
		close_out f;
		if T.body_required items then (
			let adb_filename = Filename.concat options.dest_dir (T.body_filename package) in
			print_string "generating ";
			print_string adb_filename;
			print_string "...";
			let f = open_out adb_filename in
			let ff = Format.make_formatter (output f) (fun () -> flush f) in
			begin try
				T.pp_notification ff version;
				T.pp_translated_package_body
					ff
					~name_mapping
					~name:package
					items;
				Format.pp_print_flush ff ();
				print_string "ok";
				print_newline ()
			with e ->
				Format.pp_print_flush ff ();
				print_string "error!";
				print_newline ();
				print_exception e;
				has_error := true
			end;
			close_out f
		)
	) items_per_package
| `none ->
	assert false (* does not come here *)
end;;

if !has_error then exit 1;;
