open C_filename;;
open C_literals;;
open C_semantics;;
open C_semantics_naming;;

let make_mapmap (list: (string * (string * string) list) list): string StringMap.t StringMap.t = (
	List.fold_left (fun map (filename, xs) ->
		let set = List.fold_left (fun rs (k, v)-> StringMap.add k v rs) StringMap.empty xs in
		StringMap.add filename set map
	) StringMap.empty list
);;

let set_of_fst (xs: (string * 'a) list): StringSet.t = (
	List.fold_left (fun r (x, _) -> StringSet.add (String.uppercase x) r)
		StringSet.empty xs
);;

(* special mappings *)

(* header filename (removed dir) -> package name *)
let special_package_name_mapping =
	let list = [
		"float.h", "float_h"] (* conflicated with float type *)
	in
	List.fold_right (fun (k, v) r -> StringMap.add k v r) list StringMap.empty;;

(* package name -> C identifier -> Ada Identifier *)
let special_name_mapping = make_mapmap [
	"cairo.cairo", [
		"cairo_version", "get_cairo_version"; (* cairo / conflicated with CAIRO_VERSION *)
		"cairo_version_string", "get_cairo_version_string"]; (* cairo / conflicated with CAIRO_VERSION_STRING *)
	"ctype", [
		"___runetype", "qqqrunetype"]; (* darwin10 / conflicated with runetype.h *)
	"iconv", [
		"_LIBICONV_VERSION", "LIBICONV_VERSION"]; (* iconv / conflicted with extern const *)
	"inttypes", [
		"PRIX16", "PRIX16_uppercase"; (* darwin9 / conflicted with PRIx16 *)
		"PRIX32", "PRIX32_uppercase"; (* darwin9 / conflicted with PRIx32 *)
		"PRIX64", "PRIX64_uppercase"; (* darwin9 / conflicted with PRIx64 *)
		"PRIX8", "PRIX8_uppercase"; (* darwin9 / conflicted with PRIx8 *)
		"PRIXFAST16", "PRIXFAST16_uppercase"; (* darwin9 / conflicted with PRIxFAST16 *)
		"PRIXFAST32", "PRIXFAST32_uppercase"; (* darwin9 / conflicted with PRIxFAST32 *)
		"PRIXFAST64", "PRIXFAST64_uppercase"; (* darwin9 / conflicted with PRIxFAST64 *)
		"PRIXFAST8", "PRIXFAST8_uppercase"; (* darwin9 / conflicted with PRIxFAST8 *)
		"PRIXLEAST16", "PRIXLEAST16_uppercase"; (* darwin9 / conflicted with PRIxLEAST16 *)
		"PRIXLEAST32", "PRIXLEAST32_uppercase"; (* darwin9 / conflicted with PRIxLEAST32 *)
		"PRIXLEAST64", "PRIXLEAST64_uppercase"; (* darwin9 / conflicted with PRIxLEAST64 *)
		"PRIXLEAST8", "PRIXLEAST8_uppercase"; (* darwin9 / conflicted with PRIxLEAST8 *)
		"PRIXMAX", "PRIXMAX_uppercase"; (* darwin9 / conflicted with PRIxMAX *)
		"PRIXPTR", "PRIXPTR_uppercase"]; (* darwin9 / conflicted with PRIxPTR *)
	"libxml.tree", [
		"xmlBufferWriteCHAR", "xmlBufferWriteCHAR_uppercase"]; (* libxml2 / conflicted with xmlBufferWriteChar *)
	"mach.i386.thread_status", [
		"I386_EXCEPTION_STATE_COUNT", "I386_EXCEPTION_STATE_COUNT_uppercase"; (* darwin9 / conflicated with i386_EXCEPTION_STATE_COUNT *)
		"X86_DEBUG_STATE32_COUNT", "X86_DEBUG_STATE32_COUNT_uppercase"; (* darwin9 / conflicated with x86_DEBUG_STATE32_COUNT *)
		"X86_DEBUG_STATE64_COUNT", "X86_DEBUG_STATE64_COUNT_uppercase"; (* darwin9 / conflicated with x86_DEBUG_STATE32_COUNT *)
		"X86_EXCEPTION_STATE64_COUNT", "X86_EXCEPTION_STATE64_COUNT_uppercase"]; (* darwin9 / conflicted with x86_EXCEPTION_STATE64_COUNT *)
	"mach.message", [
		"MACH_MSG_OVERWRITE", "MACH_MSG_OVERWRITE_option"]; (* darwin9 / confilicted with mach_msg_overwrite *)
	"mpfr", [
		"mpfr_version", "get_mpfr_version"]; (* mpfr / conflicted with MPFR_VERSION *)
	"png", [
		"png_libpng_ver", "get_png_libpng_ver"; (* libpng / conflicted with PNG_LIBPNG_VER *)
		"png_unknown_chunk_ptr", "png_unknown_chunk_funcptr"]; (* libpng / conflicted with pointer of png_unknown_chunk *)
	"psdk_inc.qfd_types", [
		"FD_SET", "FD_SET_uppercase"]; (* mingw-w64 / conflicted with fd_set *)
	"qctype", [
		"___runetype", "qqqrunetype"]; (* freebsd7 / hiding C.runetype (shold use Standard prefix...) *)
	"readline.keymaps", [
		"KEYMAP_ENTRY_ARRAY", "Fixed_KEYMAP_ENTRY_ARRAY"]; (* readline / conflicted with unconstrained array of KEYMAP_ENTRY *)
	"readline.readline", [
		"KEYMAP_ENTRY_ARRAY", "Fixed_KEYMAP_ENTRY_ARRAY"; (* libedit / conflicted with unconstrained array of KEYMAP_ENTRY *)
		"FUNMAP", "FUNMAP_t"; (* readline / conflicted with funmap variable *)
		"rl_readline_version", "get_rl_readline_version"; (* libedit / conflicted with RL_READLINE_VERSION *)
		"rl_vi_bWord", "rl_vi_bWord_uppercase"; (* readline / conflicted with rl_vi_bword *)
		"rl_vi_eWord", "rl_vi_eWord_uppercase"; (* readline / conflicted with rl_vi_eword *)
		"rl_vi_fWord", "rl_vi_fWord_uppercase"]; (* readline / conflicted with rl_vi_fword *)
	"signal", [
		"sv_onstack", "sigvec_sv_onstack"]; (* linux / conflicted with SV_ONSTACK *)
	"sys.signal", [
		"sv_onstack", "sigvec_sv_onstack"]; (* darwin9 / conflicted with SV_ONSTACK *)
	"stdlib", [
		"exit", "C_exit"; (* w64-mingw32 / conflicted with _Exit *)
		"_exit", "C_qexit"; (* w64-mingw32 / conflicted with _Exit *)
		"_Exit", "C_qExit2"; (* w64-mingw32 / conflicted with _exit *)
		"system", "C_system"]; (* darwin9 / hiding System package (should use Standard prefix...) *)
	"unistd", [
		"_exit", "C_qexit"; (* darwin9 / conflicted with _Exit *)
		"_Exit", "C_qExit2"; (* darwin9 / conflicted with _exit *)
		"execvP", "execvP2"]; (* darwin9 / conflicted with execvp *)
	"windef", [
		"FLOAT", "C_FLOAT"]; (* mingw-w64 / conflicted with float *)
	"wingdi", [
		"ABORTDOC", "C_ABORTDOC"; (* mingw-w64 / conflicted with AbortDoc *)
		"ENDDOC", "C_ENDDOC"; (* mingw-w64 / conflicted with EndDoc *)
		"EXTTEXTOUT", "C_EXTTEXTOUT"; (* mingw-w64 / conflicted with ExtTextOut *)
		"SETABORTPROC", "C_SETABORTPROC"; (* mingw-w64 / conflicted with SetAbortProc *)
		"SETMITERLIMIT", "C_SETMITERLIMIT"; (* mingw-w64 / conflicted with SetMiterLimit *)
		"STARTDOC", "C_STARTDOC"; (* mingw-w64 / conflicted with StartDoc *)
		"STRETCHBLT", "C_STRETCHBLT"]; (* mingw-w64 / conflicted with StretchBlt *)
	"winnt", [
		"CHAR", "C_CHAR"; (* mingw-w64 / conflicted with char *)
		"LUID_AND_ATTRIBUTES_ARRAY", "Fixed_LUID_AND_ATTRIBUTES_ARRAY"; (* mingw-w64 conflicted with unconstrained array of LUID_AND_ATTRIBUTES *)
		"SID_AND_ATTRIBUTES_ARRAY", "Fixed_SID_AND_ATTRIBUTES_ARRAY"]; (* mingw-w64 conflicted with unconstrained array of SID_AND_ATTRIBUTES *)
	"winuser", [
		"TRACKMOUSEEVENT", "TRACKMOUSEEVENT_t"]; (* mingw-w64 / conflicted with TrackMouseEvent *)
	"zlib", [
		"zlib_version", "get_zlib_version"]; (* zlib / conflicted with ZLIB_VERSION *)
	"", [ (* predefined *)
		"i386", "defined_i386"; (* darwin9 / conflicted with include dir <i386/...> *)
		"linux", "defined_linux"; (* linux / conflicted with include dir <linux/...> *)
		"__MACH__", "defined_MACH"; (* darwin9 / conflicted with include dir <mach/...> *)
		"__PIC__", "PIC"; (* darwin9 / confilicted with __pic__ on gcc-4.4 *)
		"WINNT", "defined_WINNT"]];; (* mingw-w64 / conflicted with winnt.h *)

let ada_reserved_words =
	let list = [
		"abort";
		"abs";
		"accept";
		"access";
		"aliased";
		"all";
		"and";
		"array";
		"at";
		"begin";
		"body";
		(* "case"; *)
		"constant";
		"declare";
		"delay";
		"delta";
		"digits";
		(* "do"; *)
		(* "else"; *)
		"elsif";
		"end";
		"entry";
		"exception";
		"exit";
		(* "for"; *)
		"function";
		"generic";
		(* "goto"; *)
		(* "if"; *)
		"in";
		"interface";
		"is";
		"limited";
		"loop";
		"mod";
		"new";
		"not";
		"null";
		"of";
		"or";
		"others";
		"out";
		"overriding";
		"package";
		"pragma";
		"private";
		"procedure";
		"protected";
		"raise";
		"range";
		"record";
		"rem";
		"renames";
		"requeue";
		"return";
		"reverse";
		"select";
		"separate";
		"some";
		"subtype";
		"synchronized";
		"tagged";
		"task";
		"terminate";
		"then";
		"type";
		"until";
		"use";
		"when";
		(* "while"; *)
		"with";
		"xor"]
	in List.fold_right StringSet.add list StringSet.empty;;

(* identifier *)

let ada_name_by_short (s: string): string = (
	let s_length = String.length s in
	let b = Buffer.create s_length in
	let rec loop s s_length i b state = (
		if i >= s_length then (
			Buffer.contents b
		) else (
			begin match s.[i] with
			| '_' ->
				begin match state with
				| `first ->
					loop s s_length (i + 1) b `first
				| `normal | `underline ->
					loop s s_length (i + 1) b `underline
				end
			| c ->
				if state = `underline then Buffer.add_char b '_';
				Buffer.add_char b c;
				loop s s_length (i + 1) b `normal
			end
		)
	) in
	loop s s_length 0 b `first
);;

let ada_name_by_substitute (s: string): string = (
	let substitute = 'q' in
	let s_length = String.length s in
	let b = Buffer.create s_length in
	let rec loop s s_length i b state = (
		if i >= s_length then (
			if state = `underline then Buffer.add_char b substitute;
			Buffer.contents b
		) else (
			begin match s.[i] with
			| '_' ->
				begin match state with
				| `substitute ->
					Buffer.add_char b substitute;
					loop s s_length (i + 1) b `substitute
				| `normal ->
					loop s s_length (i + 1) b `underline
				| `underline ->
					Buffer.add_char b substitute;
					Buffer.add_char b substitute;
					loop s s_length (i + 1) b `substitute
				end
			| '.' ->
				if state = `underline then Buffer.add_char b substitute;
				Buffer.add_char b '.';
				loop s s_length (i + 1) b `substitute
			| c ->
				if state = `underline then Buffer.add_char b '_';
				Buffer.add_char b c;
				loop s s_length (i + 1) b `normal
			end
		)
	) in
	loop s s_length 0 b `substitute
);;

let escape_ada_reserved_word ~(prefix: string) ~(postfix: string) (s: string): string = (
	if StringSet.mem (String.lowercase s) ada_reserved_words then (
		prefix ^ s ^ postfix
	) else (
		s
	)
);;

(* package name *)

let take_package_name (s: string): string * string = (
	begin try
		let i = String.index s '.' in
		let j = i + 1 in
		String.sub s 0 i, String.sub s j (String.length s - j)
	with Not_found ->
		s, ""
	end
);;

let strip_package_name (name: string): string = (
	begin try
		let p = String.rindex name '.' + 1 in
		String.sub name p (String.length name - p)
	with Not_found ->
		name
	end
);;

let ada_package_name (h: string): string = (
	if is_special_filename h then "" else
	begin try
		StringMap.find h special_package_name_mapping
	with Not_found ->
		let p = String.copy (Filename.chop_suffix h ".h") in
		for i = 0 to String.length p - 1 do
			if p.[i] = '/' then p.[i] <- '.' else
			if p.[i] = '-' then p.[i] <- '_'
		done;
		let p = ada_name_by_substitute p in
		(* resolving for confliction with reserved word *)
		let rec nested_package_loop p s = (
			let s, sr = take_package_name s in
			let s = escape_ada_reserved_word
				~prefix:""
				~postfix:(if sr = "" then "_h" else "_dir")
				s
			in
			if sr = "" then p ^ s else
			nested_package_loop (p ^ s ^ ".") sr
		) in
		nested_package_loop "" p
	end
);;

let hidden_packages (with_packages: (string * 'a) list) ~(current: string): StringSet.t = (
	let rec c_loop current_parent current_relative result = (
		let moving, current_relative' =
			let cp, cn = take_package_name current_relative in
			if cn = current_relative then current_relative, "" else cp, cn
		in
		let result =
			if List.mem_assoc ("C." ^ moving) with_packages then (
				StringSet.add moving result (* from a viewpoint of C.sys.signal, C.signal is hidden *)
			) else (
				result
			)
		in
		if current_relative' = "" then result else
		let current_parent' =
			if String.length current_parent = 0 then moving else current_parent ^ "." ^ moving
		in
		let current_parent'_length = String.length current_parent' in
		let result =
			List.fold_left (fun result (with_name, _) ->
				let rec w_loop w_parent w_relative result = (
					let wp, wn = take_package_name w_relative in
					if wn = w_relative then result else
					let w_parent' =
						if String.length w_parent = 0 then wp else w_parent ^ "." ^ wp
					in
					let result =
						if String.length w_parent' >= current_parent'_length
							&& String.sub w_parent' 0 current_parent'_length = current_parent'
							&& List.mem_assoc ("C." ^ wn) with_packages
						then (
							StringSet.add wn result (* from a viewpoint of C.sys.ucontext, C.sys.signal hides C.signal *)
						) else (
							result
						)
					in
					w_loop w_parent' wn result
				) in
				let wp, wn = take_package_name with_name in
				if wp <> "C" then result else
				w_loop "" wn result
			) result with_packages
		in
		c_loop current_parent' current_relative' result
	) in
	c_loop "" current StringSet.empty
);;

(* entity name *)

let ada_name_by_short ~(prefix: string) ~(postfix: string) (s: string): string = (
	escape_ada_reserved_word ~prefix ~postfix (ada_name_by_short s)
);;

let ada_name_by_substitute ~(prefix: string) ~(postfix: string) (s: string): string = (
	escape_ada_reserved_word ~prefix ~postfix (ada_name_by_substitute s)
);;

let ada_name_of_int_prec (p: int_prec): string = (
	begin match p with
	| `signed_char -> "signed_char"
	| `unsigned_char -> "unsigned_char"
	| `signed_short -> "signed_short"
	| `unsigned_short -> "unsigned_short"
	| `signed_int -> "signed_int"
	| `unsigned_int -> "unsigned_int"
	| `signed_long -> "signed_long"
	| `unsigned_long -> "unsigned_long"
	| `signed_long_long -> "signed_long_long"
	| `unsigned_long_long -> "unsigned_long_long"
	end
);;

let ada_name_of_float_prec (p: float_prec): string = (
	begin match p with
	| `float -> "float"
	| `double -> "double"
	| `long_double -> "long_double"
	end
);;

let ada_name_of_anonymous_argument (i: int): string = (
	"a" ^ string_of_int i
);;

let ada_name_of_anonymous_component (i: int): string = (
	"anonymous_" ^ string_of_int i
);;

let hash_name (type t) (a: t): string = (
	let rec hash_loop (s: string) (i: int) (r: int32): int32 = (
		if i >= String.length s then r else
		let rotate_bits = 5 in
		let r = Int32.add (Int32.shift_left r rotate_bits) (Int32.shift_right r (32 - rotate_bits)) in
		let r = Int32.add r (Int32.of_int (int_of_char s.[i])) in
		hash_loop s (i + 1) r
	) in
	let image = Marshal.to_string a [] in (* is it unique really ??? *)
	let v = hash_loop image 0 0l in
	Hexadecimal.x8l v
);;
