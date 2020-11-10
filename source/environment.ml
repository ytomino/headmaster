open C_literals;;
open! Nonpolymorphic;;

type sizeof =
	[`sizeof_bool of int] *
	[`sizeof_short of int] *
	[`sizeof_int of int] *
	[`sizeof_long of int] *
	[`sizeof_long_long of int] *
	[`sizeof_float of int] *
	[`sizeof_double of int] *
	[`sizeof_long_double of int] *
	[`sizeof_intptr of int];;

type language_typedef =
	[`typedef_ptrdiff_t of signed_int_prec] *
	[`typedef_size_t of unsigned_int_prec] *
	[`typedef_wchar_t of int_prec];;

type type_for_builtin = [
	| int_prec
	| `float
	| `double
	| `long_double
	| `bool
	| `void
	| `pointer of [`void | `char | `const of [`void | `char]]
	| `size_t];;

type fp_repr =
	[`mantissa of int] * (* mantissa bits *)
	[`emin of int];; (* minimum exponent *)

type environment = {
	en_target: string;
	en_sizeof: sizeof;
	en_typedef: language_typedef;
	en_fp: fp_repr * fp_repr * fp_repr; (* float, double, and long double *)
	en_predefined: string;
	en_builtin: (string * type_for_builtin list * type_for_builtin) list;
	en_iquote: string list;
	en_include: string list;
	en_isystem: string list;
	en_gnu_inline: bool};;

open struct
	let is_dir_sep c = c = '/' || c = '\\';;

	let last_dir_sep_index (filename: string): int = (
		let rec last_dir_sep i = (
			if i < 0 || is_dir_sep filename.[i] then i else last_dir_sep (i - 1)
		) in
		last_dir_sep (String.length filename - 1)
	);;

	let include_dir_sep_index (env: environment) (filename: string): int = (
		let fn_length = String.length filename in
		let rec loop (r: int) (xs: string list): int = (
			begin match xs with
			| [] ->
				r
			| x :: xr ->
				let x_length = String.length x in
				let sep_index =
					if is_dir_sep x.[x_length - 1] then x_length - 1 else x_length
				in
				if sep_index < fn_length then (
					if String.sub filename 0 x_length = x && is_dir_sep filename.[sep_index] then (
						loop (max r sep_index) xr
					) else (
						loop r xr
					)
				) else (
					loop r xr
				)
			end
		) in
		let r = loop (-1) env.en_iquote in
		let r = loop r env.en_include in
		let r = loop r env.en_isystem in
		if r < 0 then (
			(* specified in command line *)
			last_dir_sep_index filename
		) else (
			r
		)
	);;

	let find_include
		(env: environment)
		~(quote: bool)
		~(current: string)
		?(next: bool = false)
		(name: string)
		: string option =
	(
		let rec find_loop (xs: string list): string option = (
			begin match xs with
			| [] ->
				None
			| x :: xr ->
				let p = Filename.concat x name in
				if Sys.file_exists p then Some p else
				find_loop xr
			end
		) in
		let current_dir =
			let sep_index = include_dir_sep_index env current in
			if sep_index < 0 then "." else String.sub current 0 sep_index
		in
		let next_filter (xs: string list): string list = (
			if next then (
				let rec loop xs = (
					begin match xs with
					| [] ->
						[]
					| x :: xr ->
						if x = current_dir then xr else
						loop xr
					end
				) in
				loop xs
			) else (
				xs
			)
		) in
		begin match
			if quote then (
				begin match find_loop (next_filter env.en_iquote) with
				| Some _ as result ->
					result
				| None ->
					if next then None else
					let current_dir =
						let sep_index = last_dir_sep_index current in
						if sep_index < 0 then current_dir else String.sub current 0 sep_index
					in
					find_loop (current_dir :: [])
				end
			) else None
		with
		| Some _ as result ->
			result
		| None ->
			begin match find_loop (next_filter env.en_include) with
			| Some _ as result ->
				result
			| None ->
				find_loop (next_filter env.en_isystem)
			end
		end
	);;
end;;

let make_include
	(f: string -> 'a -> 'b)
	(env: environment)
	~(quote: bool)
	~(current: string)
	?(next: bool = false)
	(name: string)
	(a: 'a)
	: 'b option =
(
	begin match find_include env ~quote ~current ~next name with
	| Some name ->
		Some (f name a)
	| None ->
		None
	end
);;

let make_remove_include_dir (env: environment) (filename: string): string = (
	let sep_index = include_dir_sep_index env filename in
	let fn_length = String.length filename in
	assert (sep_index < 0 || is_dir_sep filename.[sep_index]);
	let i = sep_index + 1 in
	String.sub filename i (fn_length - i)
);;

let compact_filename (filename: string): string = (
	let concat dir base = (
		if dir = Filename.current_dir_name then base else
		if base = "" then dir else
		Filename.concat dir base
	) in
	let rec up level path = (
		if level = 0 then path else
		let result =
			let result = Filename.dirname path in
			if result = Filename.current_dir_name then concat path Filename.parent_dir_name else
			result
		in
		up (level - 1) result
	) in
	let rec loop level parent_path child_path = (
		if parent_path = Filename.current_dir_name then concat (up level parent_path) child_path else
		if parent_path = Filename.parent_dir_name then concat (up (level + 1) parent_path) child_path else
		let basename = Filename.basename parent_path in
		if basename = parent_path then concat (up level parent_path) child_path else
		let dirname = Filename.dirname parent_path in
		if basename = Filename.current_dir_name then loop level dirname child_path else
		if basename = Filename.parent_dir_name then loop (level + 1) dirname child_path else
		if level > 0 then loop (level - 1) dirname child_path else
		loop 0 dirname (concat basename child_path)
	) in
	if filename = "" then "." else
	loop 0 filename ""
);;

(* for each compiler *)

exception Process_failure of string * Unix.process_status;;
