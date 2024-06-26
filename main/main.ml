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
open C_version;;
open Environment;;
open Environment_gcc;;
open Known_errors;;
open Position;;
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
	iquote_dirs: string list;
	include_dirs: string list;
	isystem_dirs: string list;
	nostdinc: bool;
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
	iquote_dirs = [];
	include_dirs = [];
	isystem_dirs = [];
	nostdinc = false;
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
	case "x" ~long:"from" ~desc:"Specify source language (c/c++/objc/objc++)"
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
		=>? (fun arg options ->
			{options with include_dirs = arg :: options.include_dirs}
		);
	case "iquote" ~desc:"Add path to quote form header search path list"
		=>? (fun arg options ->
			{options with iquote_dirs = arg :: options.iquote_dirs}
		);
	case "isystem" ~desc:"Add path to system header search path list"
		=>? (fun arg options ->
			{options with isystem_dirs = arg :: options.isystem_dirs}
		);
	case "nostdinc" ~desc:"Do not search standard system include directories"
		=> (fun options -> {options with nostdinc = true});
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

if options.usage || options.version
	|| options.source_filenames = [] || options.to_lang = `none
then (
	let error = not options.usage && not options.version
		&& (options.source_filenames = [] || options.to_lang = `none)
	in
	if error && options.source_filenames = [] then (
		prerr_string "no input file was specified.";
		prerr_newline ()
	);
	if error && options.to_lang = `none then (
		prerr_string "no destination language was specified.";
		prerr_newline ()
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
	output_position stderr compact_filename (fst ps);
	output_string stderr ": ";
	output_string stderr m;
	output_char stderr '\n';
	has_error := true
);;

let env: environment =
	gcc_env options.gcc_command ~nostdinc:options.nostdinc ~x:options.lang;;
let env = {env with
	en_iquote = List.rev_append options.iquote_dirs env.en_iquote;
	en_include = List.rev_append options.include_dirs env.en_include;
	en_isystem = List.rev_append options.isystem_dirs env.en_isystem};;

module Literals = struct
	let float_repr, double_repr, long_double_repr = env.en_fp;;
	module Integer = Gmp.Z;;
	module Real =
		Mpfr.FR.Make (struct
			let prec = let `mantissa prec, _ = long_double_repr in prec;;
			let mode = `N;;
		end);;
	module WideChar = Unicode.Uint32;;
	module WideString = Unicode.UTF32;;
	let integer_of_real = Mpfr.z_of_truncated_fr;;
	let real_of_integer = Real.of_z;;
	let round = Mpfr.fr_of_fr ~mode:`N;;
end;;

module Language = struct
	let lang = options.lang;;
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

let remove_include_dir = make_remove_include_dir env;;
let is_known_error = make_is_known_error env.en_target remove_include_dir;;

let read_file (name: string): (ranged_position -> S.prim) -> S.prim = (
	let file = TextFile.of_file ~random_access:false ~tab_width:options.tab_width name in
	S.scan error file
);;

let read_include_file = make_include read_file env;;

let predefined =
	let predefined_tokens: PP.in_t =
		let file = TextFile.of_string ~random_access:false ~tab_width:options.tab_width predefined_name env.en_predefined in
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
	let source_tokens: PP.in_t =
		let dummy_position = "<dummy>", 0, 0, 0 in
		let dummy_ps = dummy_position, dummy_position in
		let rec loop (source_filenames: string list) (ps: ranged_position): PP.in_t = (
			begin match source_filenames with
			| source_filename :: source_filenames_r ->
				lazy (read_file source_filename (fun ps -> Lazy.force (loop source_filenames_r ps)))
			| [] ->
				assert (ps != dummy_ps);
				lazy (S.make_nil ps)
			end
		) in
		loop (List.rev options.source_filenames) dummy_ps
	in
	let source_tokens': PP.out_t = lazy (PP.preprocess
		error is_known_error read_include_file `top_level predefined StringMap.empty source_tokens)
	in
	P.parse_translation_unit error source_tokens';;

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

if options.create_dest_dir && not (Sys.file_exists options.dest_dir) then (
	Unix.mkdir options.dest_dir 0o755
);;

begin match options.to_lang with
| `ada ->
	let module T = AdaTranslator (Literals) (SEM) in
	let ada_mapping = SEM.find_langauge_mapping "ADA" mapping_options in
	let filename_mapping = T.filename_mapping remove_include_dir ada_mapping sources in
	let dirs = T.dir_packages filename_mapping in
	List.iter (fun x ->
		let filename = Filename.concat options.dest_dir (T.spec_filename x) in
		print_string "generating ";
		print_string filename;
		print_string "...";
		flush stdout;
		let f = open_out filename in
		let ff = Format.make_formatter (output_substring f) (fun () -> flush f) in
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
		close_out f
	) dirs;
	let items_per_package = T.items_per_package ada_mapping filename_mapping sources in
	let name_mapping = T.name_mapping filename_mapping opaque_mapping items_per_package in
	StringMap.iter (fun package items ->
		let ads_filename = Filename.concat options.dest_dir (T.spec_filename package) in
		print_string "generating ";
		print_string ads_filename;
		print_string "...";
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
			T.pp_notification ff version;
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
			flush stdout;
			let f = open_out adb_filename in
			let ff = Format.make_formatter (output_substring f) (fun () -> flush f) in
			begin try
				T.pp_notification ff version;
				T.pp_translated_package_body
					ff
					~opaque_mapping
					~name_mapping
					~name:package
					items
					context_clauses;
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
