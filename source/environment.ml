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
	| `pointer of [`void | `char | `const of [`void | `char]]];;

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

let remove_include_dir (env: environment) (filename: string): string = (
	let rec loop (r: string option) (xs: string list): string option = (
		begin match xs with
		| x :: xr ->
			let x_length = String.length x in
			let fn_length = String.length filename in
			if x_length < fn_length then (
				let parent_part = String.sub filename 0 x_length in
				if parent_part = x
					&& (filename.[x_length] = '/' || filename.[x_length] = '\\')
				then (
					let i = x_length + 1 in
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
