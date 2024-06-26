open C_filename;;
open C_lexical;;
open C_lexical_output;;
open C_preprocessor;;
open C_scanner;;
open C_version;;
open Environment;;
open Environment_gcc;;
open Known_errors;;
open Position;;
open Version;;
open! Nonpolymorphic;;

type options = {
	source_filename: string;
	tab_width: int;
	gcc_command: string;
	iquote_dirs: string list;
	include_dirs: string list;
	isystem_dirs: string list;
	nostdinc: bool;
	lang: language;
	usage: bool;
	version: bool};;

let initial_options = {
	source_filename = "";
	tab_width = 1;
	gcc_command = "gcc";
	iquote_dirs = [];
	include_dirs = [];
	isystem_dirs = [];
	nostdinc = false;
	lang = `c;
	usage = false;
	version = false};;

let option_spec =
	let open CommandLine in [
	case "c"
		=> (fun options -> {options with lang = `c});
	case "c++"
		=> (fun options -> {options with lang = `cxx});
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
	case "tab" ~desc:"Count tab as N characters (default: 1)"
		=>? (fun arg options -> {options with tab_width = int_of_string arg});
	case "v" ~long:"version" ~desc:"Print the version number"
		=> (fun options -> {options with version = true});
	otherwise
		(fun arg options -> {options with source_filename = arg})];;

let options = CommandLine.parse option_spec Sys.argv initial_options;;

if options.usage || options.version || options.source_filename = "" then (
	let no_input = not options.usage && not options.version && options.source_filename = "" in
	if no_input then (
		prerr_string "no input file was specified.";
		prerr_newline ()
	);
	if options.version then (
		print_string "headmaster cpp ";
		print_string version;
		print_newline ()
	);
	if options.usage || no_input then (
		print_string "usage: hmcpp [options] <input-file>";
		print_newline ();
		print_newline ();
		print_string "options are:";
		print_newline ();
		CommandLine.print_usage option_spec
	);
	exit (if no_input then 1 else 0)
);;

let error (ps: ranged_position) (m: string): unit = (
	output_position stderr compact_filename (fst ps);
	output_string stderr ": ";
	output_string stderr m;
	output_char stderr '\n'
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
		Gmp.F.Make (struct
			let prec = let `mantissa prec, _ = long_double_repr in prec;;
		end);;
	module WideChar = Unicode.Uint32;;
	module WideString = Unicode.UTF32;;
	let integer_of_real = Gmp.z_of_truncated_f;;
	let real_of_integer = Real.of_z;;
	let round = Gmp.f_of_f;;
end;;

module Language = struct
	let lang = options.lang;;
	let gnu_inline = env.en_gnu_inline;;
end;;

module LE = LexicalElement (Literals);;
module S = Scanner (Literals) (LE) (Language);;
module PP = Preprocessor (Literals) (LE) (S.NumericScanner);;
module O = LexicalOutput (Literals) (LE);;

let remove_include_dir = make_remove_include_dir env;;
let is_known_error = make_is_known_error env.en_target remove_include_dir;;

let read_file (name: string): (ranged_position -> S.prim) -> S.prim = (
	let file = TextFile.of_file ~random_access:false ~tab_width:options.tab_width name in
	S.scan error file
);;

let read_include_file = make_include read_file env;;

let predefined_tokens: PP.in_t =
	let file = TextFile.of_string ~random_access:false ~tab_width:options.tab_width predefined_name env.en_predefined in
	lazy (S.scan error file S.make_nil);;
let predefined_tokens': PP.out_t = lazy (PP.preprocess
	error is_known_error read_include_file `top_level StringMap.empty StringMap.empty predefined_tokens);;

let predefined =
	begin match predefined_tokens' with
	| lazy (`nil (_, predefined)) ->
		predefined
	| lazy (`cons (_, _, xr)) ->
		print_string "extra token(s) exists in predefined!!\n";
		let `nil (_, predefined) = LazyList.find_nil xr in
		predefined
	end;;

let source_tokens: PP.in_t = lazy (read_file options.source_filename S.make_nil);;
let source_tokens': PP.out_t = lazy (PP.preprocess
	error is_known_error read_include_file `top_level predefined StringMap.empty source_tokens);;

type state = [`home | `word | `symbol of string];;
let state: (position * state) ref = ref (("", 0, 0, 0), `home);;

let print_end_of_line ps () = (
	begin match snd !state with
	| `home -> ()
	| `word | `symbol _ -> print_newline ()
	end;
	state := snd ps, `home
);;

let print_word ps s = (
	let (_, _, line1, column1) = fst !state in
	let (_, _, line2, column2) = fst ps in
	if line1 <> line2 then (
		print_end_of_line ps ();
		if column2 > 1 then (
			print_string (String.make (min 8 (column2 - 1)) ' ')
		)
	) else (
		begin match snd !state with
		| `word | `symbol ("," | ")") -> print_char ' '
		| `home | `symbol _ -> if column1 + 1 < column2 then print_char ' '
		end
	);
	print_string s;
	state := snd ps, `word
);;

let print_symbol ps s = (
	let (_, _, line1, column1) = fst !state in
	let (_, _, line2, column2) = fst ps in
	if line1 <> line2 then (
		print_end_of_line ps ();
		if column2 > 1 then (
			print_string (String.make (min 8 (column2 - 1)) ' ')
		)
	) else (
		begin match snd !state with
		| `symbol "," ->
			print_char ' '
		| `home | `word | `symbol _ ->
			if column1 + 1 < column2 then print_char ' '
		end
	);
	print_string s;
	state := snd ps, `symbol s
);;

let rec loop (xs: PP.out_t): unit = (
	begin match xs with
	| lazy (`cons (ps, token, xr)) ->
		begin match token with
		| `end_of_line ->
			print_end_of_line ps ()
		| #reserved_word (* implies #extended_word *)
		| #objc_directive
		| #preprocessor_directive
		| `directive_parameter _
		| `ident _
		| `numeric_literal _ ->
			O.print_element (print_word ps) token
		| _ ->
			O.print_element (print_symbol ps) token
		end;
		loop xr
	| lazy (`nil (ps, _)) ->
		print_end_of_line ps ()
	end
) in
loop source_tokens';;
