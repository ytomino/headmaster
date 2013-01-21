open C_literals;;

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

type environment = {
	en_target: string;
	en_sizeof: sizeof;
	en_typedef: language_typedef;
	en_precision: int * int * int; (* mantissa-bits of float, double, long double *)
	en_predefined: string;
	en_builtin: (string * type_for_builtin list * type_for_builtin) list;
	en_include: string list;
	en_sys_include: string list};;

type include_from = [`user | `system];;

let find_include
	(env: environment)
	~(current: string)
	?(next: bool = false)
	(from: include_from)
	(name: string)
	: string =
(
	let rec find_loop (xs: string list): string = (
		begin match xs with
		| x :: xr ->
			let p = Filename.concat x name in
			if Sys.file_exists p then p else
			find_loop xr
		| [] ->
			raise Not_found
		end
	) in
	let current_dir = Filename.dirname current in
	let next_filter (xs: string list): string list = (
		if next then (
			let rec loop xs = (
				begin match xs with
				| x :: xr ->
					if x = current_dir then xr else
					loop xr
				| [] ->
					raise Not_found
				end
			) in
			loop xs
		) else (
			xs
		)
	) in
	begin match from with
	| `user ->
		begin try find_loop (next_filter env.en_include) with
		| Not_found ->
			begin try
				if next then raise Not_found else
				find_loop (current_dir :: [])
			with
			| Not_found -> find_loop (next_filter env.en_sys_include)
			end
		end
	| `system ->
		find_loop (next_filter env.en_sys_include)
	end
);;

let make_include
	(f: string -> 'a)
	(env: environment)
	~(current: string)
	?(next: bool = false)
	(from: include_from)
	(name: string)
	: 'a =
(
	let name = find_include env ~current ~next from name in
	f name
);;

let make_remove_include_dir (env: environment) (filename: string): string = (
	let rec loop (r: string option) (xs: string list): string option = (
		begin match xs with
		| x :: xr ->
			let x_length = String.length x in
			let sep_index = if x.[x_length - 1] = '/' then x_length - 1 else x_length in
			let fn_length = String.length filename in
			if sep_index < fn_length then (
				if String.sub filename 0 x_length = x
					&& (filename.[sep_index] = '/' || filename.[sep_index] = '\\')
				then (
					let i = sep_index + 1 in
					let h_part = String.sub filename i (fn_length - i) in
					begin match r with
					| Some prev when String.length prev < String.length h_part ->
						loop r xr
					| _ ->
						loop (Some h_part) xr
					end
				) else (
					loop r xr
				)
			) else (
				loop r xr
			)
		| [] ->
			r
		end
	) in
	let r = loop None env.en_include in
	let r = loop r env.en_sys_include in
	begin match r with
	| Some r ->
		r
	| None ->
		Filename.basename filename (* specified in command line *)
	end
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
