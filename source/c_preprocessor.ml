open C_lexical;;
open Value;;

type position = (string * int * int * int);;
type ranged_position = position * position;;

let predefined_name = "<predefined>";;

type include_from = [`user | `system];;

module StringSet = Set.Make (String);;
module StringMap = Map.Make (String);;

let make_setmap (list: (string * string list) list): StringSet.t StringMap.t = (
	List.fold_left (fun map (filename, macros) ->
		let set = List.fold_right StringSet.add macros StringSet.empty in
		StringMap.add filename set map
	) StringMap.empty list
);;

let setmap_mem (ps: ranged_position) (e: string) (setmap: StringSet.t StringMap.t): bool = (
	let filename, _, _, _ = fst ps in
	try
		let s = StringMap.find (Filename.basename filename) setmap in
		StringSet.mem e s
	with Not_found -> false
);;

let no_error_report_macros = make_setmap [
	"cdefs.h", ["__FreeBSD_cc_version"]; (* freebsd7 *)
	"complex.h", ["__WANT_LONG_DOUBLE_FORMAT__"]; (* darwin9 *)
	"_mingw.h", ["__GNUC_STDC_INLINE__"]; (* mingw32 *)
	"shellapi.h", ["_WIN32_IE"]; (* mingw32 *)
	"stdint.h", ["__LP64__"]; (* darwin9 *)
	"stdio.h", [
		"_FORTIFY_SOURCE"; (* darwin9 *)
		"__MINGW_FEATURES__"]; (* mingw32 *)
	"string.h", ["_FORTIFY_SOURCE"]; (* darwin9 *)
	"types.h", ["__LP64__"]; (* darwin9 *)
	"winbase.h", [
		"__USE_NTOSKRNL__"; (* mingw32 *)
		"_WIN32_WINDOWS"]; (* mingw32 *)
	"wingdi.h", ["_WIN32_WINDOWS"]; (* mingw32 *)
	"winsock2.h", ["__INSIDE_MSYS__"]; (* mingw32 *)
	"winuser.h", ["_WIN32_WINDOWS"]];; (* mingw32 *)

module PreprocessorType
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType (Literals).S) =
struct
	module type S = sig
		
		type in_t = (ranged_position, LexicalElement.t, unit) LazyList.t
		and in_prim = (ranged_position, LexicalElement.t, unit) LazyList.prim
		and out_t = (ranged_position, LexicalElement.t, define_map) LazyList.t
		and out_prim = (ranged_position, LexicalElement.t, define_map) LazyList.prim
		and define_map = define_item StringMap.t
		and define_item = private {
			df_name: string;
			df_position: ranged_position;
			df_has_arguments: bool;
			df_args: (ranged_position * string) list;
			df_varargs: bool;
			df_contents: in_t};;
		
		val preprocess:
			(ranged_position -> string -> unit) ->
			language ->
			(current:string -> ?next:bool -> include_from -> string -> in_prim) ->
			bool ->
			define_map ->
			out_t StringMap.t ->
			in_t ->
			out_prim;;
		
	end;;
end;;

module Preprocessor
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType (Literals).S)
	: PreprocessorType (Literals) (LexicalElement).S =
struct
	open Literals;;
	
	let __STDC__ = `int_literal (`signed_int, Integer.one);;
	let __STDC_VERSION__ = `int_literal (`signed_long, Integer.of_int 199901);;
	
	type in_t = (ranged_position, LexicalElement.t, unit) LazyList.t
	and in_prim = (ranged_position, LexicalElement.t, unit) LazyList.prim
	and out_t = (ranged_position, LexicalElement.t, define_map) LazyList.t
	and out_prim = (ranged_position, LexicalElement.t, define_map) LazyList.prim
	and define_map = define_item StringMap.t
	and define_item = {
		df_name: string;
		df_position: ranged_position;
		df_has_arguments: bool;
		df_args: (ranged_position * string) list;
		df_varargs: bool;
		df_contents: in_t};;
		
	let leinteger_of_bool (n: bool): Integer.t = (
		if n then (
			Integer.one
		) else (
			Integer.zero
		)
	);;
	
	let bool_of_leinteger (n: Integer.t): bool = (
		Integer.compare n Integer.zero <> 0
	);;
	
	let rec take_line (xs: in_t): in_t * in_t = (
		begin match xs with
		| lazy (`cons (ps, token, xr)) ->
			if token = `end_of_line then (
				lazy (`nil (ps, ())), xr
			) else (
				let cs, xr' = take_line xr in
				lazy (`cons (ps, token, cs)), xr'
			)
		| lazy (`nil _) as eof ->
			eof, eof
		end
	);;
	
	let rec take_assignment_expression (level: int) (xs: in_t): in_t * in_t = (
		begin match xs with
		| lazy (`cons (ps, `l_paren, xr)) ->
			let cs, xr' = take_assignment_expression (level + 1) xr in
			lazy (`cons (ps, `l_paren, cs)), xr'
		| lazy (`cons (ps, `r_paren, xr)) ->
			if level > 0 then (
				let cs, xr' = take_assignment_expression (level - 1) xr in
				lazy (`cons (ps, `r_paren, cs)), xr'
			) else (
				lazy (`nil (ps, ())), xs
			)
		| lazy (`cons (ps, `comma, _)) when level = 0 ->
			lazy (`nil (ps, ())), xs
		| lazy (`cons (ps, token, xr)) ->
			let cs, xr' = take_assignment_expression level xr in
			lazy (`cons (ps, token, cs)), xr'
		| lazy (`nil _) as eof ->
			eof, eof
		end
	);;
	
	let rec take_structure_while
		(f: LexicalElement.t -> bool)
		(level: int)
		(xs: in_t): in_t * in_t =
	(
		begin match xs with
		| lazy (`cons (ps, ((`sharp_IF | `sharp_IFDEF | `sharp_IFNDEF) as token), xr)) ->
			let cs, xr' = take_structure_while f (level + 1) xr in
			lazy (`cons (ps, token, cs)), xr'
		| lazy (`cons (ps, `sharp_ENDIF, xr)) when level > 0 ->
			let cs, xr' = take_structure_while f (level - 1) xr in
			lazy (`cons (ps, `sharp_ENDIF, cs)), xr'
		| lazy (`cons (ps, token, xr)) ->
			if level > 0 || f token then (
				let cs, xr' = take_structure_while f level xr in
				lazy (`cons (ps, token, cs)), xr'
			) else (
				lazy (`nil (ps, ())), xs
			)
		| lazy (`nil _) as eof ->
			eof, eof
		end
	);;
	
	let rec calc_expr
		(error: ranged_position -> string -> unit)
		(xs: out_t)
		: Integer.t =
	(
		let rec calc_term (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			begin match xs with
			| lazy (`cons (_, `l_paren, xr)) ->
				let value, xr = calc_cond shortcircuit xr in
				begin match xr with
				| lazy (`cons (_, `r_paren, xr)) ->
					value, xr
				| lazy (`cons (ps, _, _) | `nil (ps, _)) ->
					error ps "parenthesis mismatched.";
					value, xr
				end
			| lazy (`cons (_, `exclamation, xr)) ->
				let value, xr = calc_term shortcircuit xr in
				let value' = leinteger_of_bool (not (bool_of_leinteger value)) in
				value', xr
			| lazy (`cons (_, `int_literal (_, value), xr)) ->
				value, xr
			| lazy (`cons (_, `char_literal value, xr)) ->
				let x =
					(* treat char as signed int *)
					let c = int_of_char value in
					if c < 0x80 then c else c lor (-1 lxor 0xff)
				in
				Integer.of_int x, xr
			| lazy (`cons (ps, `ident name, xr)) ->
				if not shortcircuit && not (setmap_mem ps name no_error_report_macros) then (
					error ps (name ^ " is undefined.")
				);
				Integer.zero, xr
			| lazy (`cons (ps, _, _) | `nil (ps, _)) ->
				error ps "term is required.";
				Integer.zero, xs
			end
		) and calc_sum (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let (value1, xr) as result = calc_term shortcircuit xs in
			begin match xr with
			| lazy (`cons (_, `plus, xr)) ->
				let (value2, xr) = calc_term shortcircuit xr in
				(Integer.add value1 value2), xr
			| lazy (`cons (_, `minus, xr)) ->
				let (value2, xr) = calc_term shortcircuit xr in
				(Integer.sub value1 value2), xr
			| _ -> result
			end
		) and calc_shift (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let (value1, xr) as result = calc_sum shortcircuit xs in
			begin match xr with
			| lazy (`cons (_, `l_shift, xr)) ->
				let (value2, xr) = calc_sum shortcircuit xr in
				(Integer.shift_left value1 (Integer.to_int value2)), xr
			| lazy (`cons (_, `r_shift, xr)) ->
				let (value2, xr) = calc_sum shortcircuit xr in
				(Integer.shift_right value1 (Integer.to_int value2)), xr
			| _ -> result
			end
		) and calc_relation (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let (value1, xr) as result = calc_shift shortcircuit xs in
			begin match xr with
			| lazy (`cons (_, `eq, xr)) ->
				let (value2, xr) = calc_shift shortcircuit xr in
				let r = leinteger_of_bool
					(Integer.compare value1 value2 = 0)
				in
				r, xr
			| lazy (`cons (_, `ne, xr)) ->
				let (value2, xr) = calc_shift shortcircuit xr in
				let r = leinteger_of_bool
					(Integer.compare value1 value2 <> 0)
				in
				r, xr
			| lazy (`cons (_, `lt, xr)) ->
				let (value2, xr) = calc_shift shortcircuit xr in
				let r = leinteger_of_bool
					(Integer.compare value1 value2 < 0)
				in
				r, xr
			| lazy (`cons (_, `le, xr)) ->
				let (value2, xr) = calc_shift shortcircuit xr in
				let r = leinteger_of_bool
					(Integer.compare value1 value2 <= 0)
				in
				r, xr
			| lazy (`cons (_, `gt, xr)) ->
				let (value2, xr) = calc_shift shortcircuit xr in
				let r = leinteger_of_bool
					(Integer.compare value1 value2 > 0)
				in
				r, xr
			| lazy (`cons (_, `ge, xr)) ->
				let (value2, xr) = calc_shift shortcircuit xr in
				let r = leinteger_of_bool
					(Integer.compare value1 value2 >= 0)
				in
				r, xr
			| _ -> result
			end
		) and calc_bit_and (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let (value1, xr) as result = calc_relation shortcircuit xs in
			begin match xr with
			| lazy (`cons (_, `ampersand, xr)) ->
				let (value2, xr) = calc_relation shortcircuit xr in
				let r = Integer.logand value1 value2 in
				r, xr
			| _ -> result
			end
		) and calc_and_then (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let (value1, xr) as result = calc_bit_and shortcircuit xs in
			begin match xr with
			| lazy (`cons (_, `and_then, xr)) ->
				let b1 = bool_of_leinteger value1 in
				let (value2, xr) = calc_and_then (shortcircuit || not b1) xr in
				let r = leinteger_of_bool (b1 && bool_of_leinteger value2) in
				r, xr
			| _ -> result
			end
		) and calc_or_else (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let (value1, xr) as result = calc_and_then shortcircuit xs in
			begin match xr with
			| lazy (`cons (_, `or_else, xr)) ->
				let b1 = bool_of_leinteger value1 in
				let (value2, xr) = calc_or_else (shortcircuit || b1) xr in
				let r = leinteger_of_bool (b1 || bool_of_leinteger value2) in
				r, xr
			| _ -> result
			end
		) and calc_cond (shortcircuit: bool) (xs: out_t): Integer.t * out_t = (
			let (value_c, xr) as result = calc_or_else shortcircuit xs in
			begin match xr with
			| lazy (`cons (_, `question, xr)) ->
				let cond = bool_of_leinteger value_c in
				let (value1, xr) = calc_or_else (shortcircuit || not cond) xr in
				begin match xr with
				| lazy (`cons (_, `colon, xr)) ->
					let (value2, xr) = calc_or_else (shortcircuit || cond) xr in
					(if cond then value1 else value2), xr
				| lazy (`cons (ps, _, _) | `nil (ps, _)) ->
					error ps "colon is required.";
					(if cond then value1 else Integer.zero), xr
				end
			| _ -> result
			end
		) in
		let result, xr = calc_cond false xs in
		begin match xr with
		| lazy (`cons (ps, _, _)) ->
			error ps "extra token(s) exists in the expression.";
		| lazy (`nil _) ->
			()
		end;
		result
	);;
	
	let rec preprocess
		(error: ranged_position -> string -> unit)
		(lang: language)
		(read: current:string -> ?next:bool -> include_from -> string -> in_prim)
		(in_macro_expr: bool)
		(predefined: define_map)
		(macro_arguments: out_t StringMap.t)
		(xs: in_t): out_prim =
	(
		let rec process_if (cond: bool) (xs: in_t): out_prim = (
			let is_not_endif (x: LexicalElement.t): bool = (
				x <> `sharp_ENDIF
			) in
			let is_not_sep (x: LexicalElement.t): bool = (
				begin match x with
				| `sharp_ELIF | `sharp_ELSE | `sharp_ENDIF -> false
				| _ -> true
				end
			) in
			if cond then (
				let cs, xr = take_structure_while is_not_sep 0 xs in
				let cs' = lazy (preprocess error lang read false predefined StringMap.empty cs) in
				let _, xr = take_structure_while is_not_endif 0 xr in
				let xr =
					begin match xr with
					| lazy (`cons (_, `sharp_ENDIF, xr)) ->
						let _, xr = take_line xr in
						xr
					| lazy (`cons (ps, _, _)) | lazy (`nil (ps, _)) ->
						error ps "#endif mismatched.";
						xr
					end
				in
				LazyList.append_f cs' (fun (_, predefined) ->
					preprocess error lang read false predefined StringMap.empty xr)
			) else (
				let _, xr = take_structure_while is_not_sep 0 xs in
				begin match xr with
				| lazy (`cons (_, `sharp_ELIF, xr)) ->
					let expr, xr = take_line xr in
					let expr' = lazy (preprocess error lang read true predefined StringMap.empty expr) in
					let cond = bool_of_leinteger (calc_expr error expr') in
					process_if cond xr
				| lazy (`cons (_, `sharp_ELSE, xr)) ->
					let _, xr = take_line xr in
					process_if true xr
				| lazy (`cons (_, `sharp_ENDIF, xr)) ->
					let _, xr = take_line xr in
					preprocess error lang read false predefined StringMap.empty xr
				| lazy (`cons (ps, _, _)) | lazy (`nil (ps, _)) ->
					error ps "#endif mismatched.";
					preprocess error lang read false predefined StringMap.empty xr
				end
			)
		) in
		let process_replace ps token name xs: out_prim = (
			let item = StringMap.find name predefined in
			if item.df_has_arguments then (
				begin match xs with
				| lazy (`cons (_, `l_paren, xr)) ->
					let xr, arguments =
						begin match xr with
						| lazy (`cons (_, `r_paren, xr)) ->
							xr, []
						| _ ->
							let rec take_arguments xs rs = (
								begin match xs with
								| lazy (`cons (ps, `r_paren, xr)) ->
									xr, List.rev (lazy (`nil (ps, ())) :: rs)
								| _ ->
									let arg, xr = take_assignment_expression 0 xs in
									let rs = arg :: rs in
									begin match xr with
									| lazy (`cons (_, `r_paren, xr)) ->
										xr, List.rev rs
									| lazy (`cons (_, `comma, xr)) ->
										take_arguments xr rs
									| lazy (`cons (ps, _, _)) | lazy (`nil (ps, ())) ->
										error ps "parenthesis mismatched.";
										xs, rs
									end
								end
							) in
							take_arguments xr []
						end
					in
					let (arguments: out_t list) = List.map (fun args ->
						lazy (preprocess error lang read false predefined macro_arguments args)) arguments
					in
					let the_macro_arguments =
						let rec loop names (arguments: out_t list) (defined: out_t StringMap.t): out_t StringMap.t = (
							begin match names, arguments with
							| [], [] ->
								defined
							| [], _ ->
								if item.df_varargs then (
									let a = lazy (LazyList.concat arguments) in
									let n = string_of_rw `__VA_ARGS__ in
									StringMap.add n a defined
								) else (
									error ps "too many arguments.";
									defined
								)
							| _ :: _, [] ->
								error ps "too few arguments.";
								defined
							| n :: nr, a :: ar ->
								loop nr ar (StringMap.add (snd n) a defined)
							end
						) in
						loop item.df_args arguments StringMap.empty
					in
					let yr' =
						let removed = StringMap.remove name predefined in
						let yr' = lazy (preprocess error lang read in_macro_expr removed the_macro_arguments item.df_contents) in
						(* replace position-info to current *)
						let ((filename, _, line, _), _) = ps in
						lazy (
							LazyList.map_a (fun ps' ->
								let ((filename', _, line', _), _) = ps' in
								if filename' = filename && line' >= line then ps' else ps
							) yr')
					in
					LazyList.append yr' (lazy (
						preprocess error lang read in_macro_expr predefined macro_arguments xr))
				| _ ->
					(* not expainding when the function macro used without arguments *)
					`cons (ps, token, lazy (
						preprocess error lang read in_macro_expr predefined macro_arguments xs))
				end
			) else (
				let removed = StringMap.remove name predefined in
				let yr' = lazy (preprocess error lang read in_macro_expr removed StringMap.empty item.df_contents) in
				begin match yr' with
				| lazy (`cons (_, `ident replaced, lazy (`nil _))) when StringMap.mem replaced removed ->
					(* re-expanding function-macro with following arguments *)
					(* replace position-info to current *)
					let yr' = lazy (LazyList.map_a (fun _ -> ps) yr') in
					preprocess error lang read in_macro_expr predefined macro_arguments (lazy (
						LazyList.append yr' xs))
				| _ ->
					(* replace position-info to current *)
					let yr' = lazy (LazyList.map_a (fun _ -> ps) yr') in
					LazyList.append yr' (lazy (
						preprocess error lang read in_macro_expr predefined macro_arguments xs))
				end
			)
		) in
		let process_d_sharp ps1 (`ident name1 as it1) ds_p xs: out_prim = (
			begin match xs with
			| lazy (`cons (_, `ident name2, xr)) when StringMap.mem name2 macro_arguments ->
				let yr = StringMap.find name2 macro_arguments in
				begin match yr with
				| lazy (`cons (ps2, `ident name2, yr)) ->
					let merged_token = `ident (name1 ^ name2) in
					let merged_ps = fst ps1, snd ps2 in
					`cons (merged_ps, merged_token, lazy (
						LazyList.append yr (lazy (
							preprocess error lang read in_macro_expr predefined macro_arguments xr))))
				| lazy (`cons (ps2, (`chars_literal s as it2), yr)) ->
					if name1 = "L" then (
						let s = Array.init (String.length s) (fun i -> Int32.of_int (int_of_char s.[i])) in
						let merged_token = `wchars_literal (WideString.of_array s) in
						let merged_ps = fst ps1, snd ps2 in
						`cons (merged_ps, merged_token, lazy (
							LazyList.append yr (lazy (
								preprocess error lang read in_macro_expr predefined macro_arguments xr))))
					) else (
						error ds_p "## requires two identifiers.";
						`cons (ps1, it1, lazy (
							`cons (ps2, it2, lazy (
								LazyList.append yr (lazy (
									preprocess error lang read in_macro_expr predefined macro_arguments xr))))))
					)
				| _ ->
					error ds_p "## requires two identifiers.";
					`cons (ps1, it1, lazy (
						LazyList.append yr (lazy (
							preprocess error lang read in_macro_expr predefined macro_arguments xr))))
				end
			| lazy (`cons (ps2, `ident name2, xr)) ->
				let merged_token = `ident (name1 ^ name2) in
				let merged_ps = fst ps1, snd ps2 in
				`cons (merged_ps, merged_token, lazy (
					preprocess error lang read in_macro_expr predefined macro_arguments xr))
			| _ ->
				error ds_p "## requires two identifiers.";
				(* skip ## *)
				`cons (ps1, `ident name1, lazy (
					preprocess error lang read in_macro_expr predefined macro_arguments xs))
			end
		) in
		begin match xs with
		| lazy (`cons (ps, token, xr)) ->
			begin match token with
			| `sharp_DEFINE ->
				assert (not in_macro_expr);
				begin match xr with
				| lazy (`cons (name_ps, `ident name, xr)) ->
					let (_, (name_file, name_lpos, _, _)) = name_ps in
					begin match xr with
					| lazy (`cons (((lp_file, lp_fpos, _, _), _), `l_paren, xr))
						when lp_file = name_file && lp_fpos = name_lpos + 1 ->
						(* function macro *)
						let rec loop xr = (
							begin match xr with
							| lazy (`cons (arg_p, `ident arg,
								lazy (`cons (_, `comma, xr)))) ->
								let args, varargs, xr = loop xr in
								((arg_p, arg) :: args), varargs, xr
							| lazy (`cons (arg_p, `ident arg,
								lazy (`cons (_, `r_paren, xr)))) ->
								[arg_p, arg], false, xr
							| lazy (`cons (_, `varargs,
								lazy (`cons (_, `r_paren, xr)))) ->
								[], true, xr
							| lazy (`cons (_, `r_paren, xr)) ->
								[], false, xr
							| lazy (`cons (ps, _, _)) | lazy (`nil (ps, _)) ->
								error ps "parenthesis mismatched.";
								[], false, xr
							end
						) in
						let args, varargs, xr = loop xr in
						let cs, xr = take_line xr in
						let predefined = StringMap.add name {
							df_name = name;
							df_position = name_ps;
							df_has_arguments = true;
							df_args = args;
							df_varargs = varargs;
							df_contents = cs} predefined
						in
						preprocess error lang read false predefined StringMap.empty xr
					| _ ->
						(* simple macro *)
						let cs, xr = take_line xr in
						let predefined = StringMap.add name {
							df_name = name;
							df_position = name_ps;
							df_has_arguments = false;
							df_args = [];
							df_varargs = false;
							df_contents = cs} predefined
						in
						preprocess error lang read false predefined StringMap.empty xr
					end
				| lazy (`cons (name_ps, (#extended_word as ew), xs)) ->
					begin match ew with
					| `__int64 -> ()
					| _ -> error name_ps "extended keyword be re-defined.";
					end;
					let _, xs = take_line xs in
					preprocess error lang read false predefined StringMap.empty xs
				| lazy (`cons (name_ps, #compiler_macro, xs)) ->
					let filename, _, _, _ = fst name_ps in
					if filename <> predefined_name then (
						error name_ps "compiler macro be re-defined."
					);
					let _, xs = take_line xs in
					preprocess error lang read false predefined StringMap.empty xs
				| _ ->
					error ps "#define requires name of macro.";
					let _, xr = take_line xr in
					preprocess error lang read false predefined StringMap.empty xr
				end
			| `sharp_UNDEF ->
				assert (not in_macro_expr);
				begin match xr with
				| lazy (`cons (_, `ident name,
					lazy (`cons (_, `end_of_line, xr)))) ->
					let predefined = StringMap.remove name predefined in
					preprocess error lang read in_macro_expr predefined StringMap.empty xr
				| lazy (`cons (_, (#extended_word as ew),
					lazy (`cons (_, `end_of_line, xr)))) ->
					let name = string_of_ew ew in
					let predefined = StringMap.remove name predefined in
					preprocess error lang read in_macro_expr predefined StringMap.empty xr
				| _ ->
					error ps "#undef requires single name of macro.";
					let _, xr = take_line xr in
					preprocess error lang read false predefined StringMap.empty xr
				end
			| `sharp_IF ->
				let expr, xr = take_line xr in
				let expr' = lazy (preprocess error lang read true predefined StringMap.empty expr) in
				let cond = bool_of_leinteger (calc_expr error expr') in
				process_if cond xr
			| `sharp_IFDEF ->
				assert (not in_macro_expr);
				begin match xr with
				| lazy (`cons (_, `ident name,
					lazy (`cons (_, `end_of_line, xr)))) ->
					let cond = StringMap.mem name predefined in
					process_if cond xr
				| lazy (`cons (_, (#extended_word as ew),
					lazy (`cons (_, `end_of_line, xr)))) ->
					let name = string_of_ew ew in
					let cond = StringMap.mem name predefined in
					process_if cond xr
				| lazy (`cons (_, #compiler_macro,
					lazy (`cons (_, `end_of_line, xr)))) ->
					process_if true xr
				| _ ->
					error ps "#ifdef requires single name of macro.";
					let _, xr = take_line xr in
					preprocess error lang read false predefined StringMap.empty xr
				end
			| `sharp_IFNDEF ->
				assert (not in_macro_expr);
				begin match xr with
				| lazy (`cons (_, `ident name,
					lazy (`cons (_, `end_of_line, xr)))) ->
					let cond = not (StringMap.mem name predefined) in
					process_if cond xr
				| lazy (`cons (_, (#extended_word as ew),
					lazy (`cons (_, `end_of_line, xr)))) ->
					let name = string_of_ew ew in
					let cond = not (StringMap.mem name predefined) in
					process_if cond xr
				| lazy (`cons (_, #compiler_macro,
					lazy (`cons (_, `end_of_line, xr)))) ->
					process_if false xr
				| _ ->
					error ps "#ifndef requires single name of macro.";
					let _, xr = take_line xr in
					preprocess error lang read false predefined StringMap.empty xr
				end
			| `sharp_ELIF ->
				assert (not in_macro_expr);
				error ps "#elif without #if.";
				let _, xr = take_line xr in
				preprocess error lang read false predefined StringMap.empty xr
			| `sharp_ELSE ->
				assert (not in_macro_expr);
				error ps "#else without #if.";
				let _, xr = take_line xr in
				preprocess error lang read false predefined StringMap.empty xr
			| `sharp_ENDIF ->
				assert (not in_macro_expr);
				error ps "#endif without #if.";
				let _, xr = take_line xr in
				preprocess error lang read false predefined StringMap.empty xr
			| `sharp_INCLUDE
			| `sharp_INCLUDE_NEXT as token ->
				assert (not in_macro_expr);
				begin match xr with
				| lazy (`cons (_, `directive_parameter s,
					lazy (`cons (_, `end_of_line, xr)))) ->
					let s_length = String.length s in
					let from =
						if s_length <= 2 then (
							assert false
						) else if s.[0] = '<' && s.[s_length - 1] = '>' then (
							`system
						) else if s.[0] = '\"' && s.[s_length - 1] = '\"' then (
							`user
						) else (
							assert false
						)
					in
					begin try
						let filename = String.sub s 1 (s_length - 2) in
						let included =
							let (current, _, _, _), _ = ps in
							begin match token with
							| `sharp_INCLUDE ->
								lazy (read ~current from filename)
							| `sharp_INCLUDE_NEXT ->
								lazy (read ~current ~next:true from filename)
							end
						in
						let included' = lazy (preprocess error lang read false predefined StringMap.empty included) in
						LazyList.append_f included' (fun (_, predefined) ->
							preprocess error lang read false predefined StringMap.empty xr)
					with Not_found ->
						error ps (s ^ " is not found.");
						preprocess error lang read false predefined StringMap.empty xr
					end
				| _ -> assert false
				end
			| `sharp_ERROR ->
				assert (not in_macro_expr);
				begin match xr with
				| lazy (`cons (_, `directive_parameter message,
					lazy (`cons (_, `end_of_line, xr)))) ->
					error ps ("#error " ^ message);
					preprocess error lang read false predefined StringMap.empty xr
				| _ -> assert false
				end
			| `sharp_WARNING ->
				assert (not in_macro_expr);
				begin match xr with
				| lazy (`cons (_, `directive_parameter message,
					lazy (`cons (_, `end_of_line, xr)))) ->
					error ps ("#warning " ^ message);
					preprocess error lang read false predefined StringMap.empty xr
				| _ -> assert false
				end
			| `sharp_LINE -> (* does not handle #line *)
				assert (not in_macro_expr);
				let _, xr = take_line xr in
				preprocess error lang read false predefined StringMap.empty xr
			| `sharp_PRAGMA -> (* #pragma should be handled by compiler, not preprocessor *)
				assert (not in_macro_expr);
				`cons (ps, token, lazy (
					preprocess error lang read in_macro_expr predefined StringMap.empty xr))
			| `sharp ->
				begin match xr with
				| lazy (`cons (_, `ident name2, xr)) when StringMap.mem name2 macro_arguments ->
					let yr = StringMap.find name2 macro_arguments in
					begin match yr with
					| lazy (`cons (ps2, `ident name2, yr)) ->
						let chars_token = `chars_literal name2 in
						let merged_ps = fst ps, snd ps2 in
						`cons (merged_ps, chars_token, lazy (
							LazyList.append yr (lazy (
								preprocess error lang read in_macro_expr predefined macro_arguments xr))))
					| _ ->
						error ps "# requires one identifier.";
						LazyList.append yr (lazy (
							preprocess error lang read in_macro_expr predefined macro_arguments xr))
					end
				| lazy (`cons (ps2, `ident name2, xr)) ->
					let chars_token = `chars_literal name2 in
					let merged_ps = fst ps, snd ps2 in
					`cons (merged_ps, chars_token, lazy (
						preprocess error lang read in_macro_expr predefined macro_arguments xr))
				| _ ->
					error ps "# requires one identifier.";
					preprocess error lang read in_macro_expr predefined macro_arguments xr
				end
			| `ident "defined" when in_macro_expr ->
				begin match xr with
				| lazy (`cons (_, `l_paren,
					lazy (`cons (_, `ident name,
						lazy (`cons ((_, p2), `r_paren, xr))))))
				| lazy (`cons ((_, p2), `ident name, xr)) ->
					let (p1, _) = ps in
					let cond = StringMap.mem name predefined in
					let value = `int_literal (`signed_int, leinteger_of_bool cond) in
					`cons ((p1, p2), value, lazy (
						preprocess error lang read true predefined macro_arguments xr))
				| lazy (`cons (_, `l_paren,
					lazy (`cons (_, (#extended_word as ew),
						lazy (`cons ((_, p2), `r_paren, xr))))))
				| lazy (`cons ((_, p2), (#extended_word as ew), xr)) ->
					let (p1, _) = ps in
					let name = string_of_ew ew in
					let cond = StringMap.mem name predefined in
					let value = `int_literal (`signed_int, leinteger_of_bool cond) in
					`cons ((p1, p2), value, lazy (
						preprocess error lang read true predefined macro_arguments xr))
				| lazy (`cons (_, `l_paren,
					lazy (`cons (_, #compiler_macro,
						lazy (`cons ((_, p2), `r_paren, xr))))))
				| lazy (`cons ((_, p2), #compiler_macro, xr)) ->
					let (p1, _) = ps in
					let value = `int_literal (`signed_int, Integer.one) in
					`cons ((p1, p2), value, lazy (
						preprocess error lang read true predefined macro_arguments xr))
				| _ ->
					error ps "defined requires single name of macro.";
					preprocess error lang read true predefined macro_arguments xr
				end
			| `__STDC__ ->
				`cons (ps, __STDC__, lazy (
					preprocess error lang read in_macro_expr predefined macro_arguments xr))
			| `__STDC_VERSION__ ->
				`cons (ps, __STDC_VERSION__, lazy (
					preprocess error lang read in_macro_expr predefined macro_arguments xr))
			| `ident name when StringMap.mem name macro_arguments ->
				let yr = StringMap.find name macro_arguments in
				begin match yr with
				| lazy (`cons (ps1, (`ident _ as name1), lazy (`nil _))) ->
					begin match xr with
					| lazy (`cons (ds_p, `d_sharp, xr)) ->
						process_d_sharp ps1 name1 ds_p xr
					| _ ->
						LazyList.append yr (lazy (
							preprocess error lang read in_macro_expr predefined macro_arguments xr))
					end
				| _ ->
					LazyList.append yr (lazy (
						preprocess error lang read in_macro_expr predefined macro_arguments xr))
				end
			| `ident name when StringMap.mem name predefined ->
				process_replace ps token name xr
			| #extended_word as ew when StringMap.mem (string_of_ew ew) predefined ->
				process_replace ps token (string_of_ew ew) xr
			| `ident _ as name1 ->
				begin match xr with
				| lazy (`cons (ds_p, `d_sharp, xr)) ->
					process_d_sharp ps name1 ds_p xr
				| _ ->
					`cons (ps, token, lazy (
						preprocess error lang read in_macro_expr predefined macro_arguments xr))
				end
			| _ ->
				`cons (ps, token, lazy (
					preprocess error lang read in_macro_expr predefined macro_arguments xr))
			end
		| lazy (`nil (ps, _)) ->
			`nil (ps, predefined)
		end
	);;
	
end;;
