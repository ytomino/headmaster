open C_literals;;
open Environment;;

let gcc_lang (lang: [< language]): string = (
	begin match lang with
	| `c -> "c"
	| `cxx -> "c++"
	| `objc -> "objective-c"
	| `objcxx -> "objective-c++"
	end
);;

let float_of_stringl (s: string): float = (
	let h1 = String.length s - 1 in
	if s.[h1] = 'L' then (
		let h2 = h1 - 1 in
		if s.[h2] = 'L' then (
			float_of_string (String.sub s 0 h2)
		) else (
			float_of_string (String.sub s 0 h1)
		)
	) else (
		float_of_string s
	)
);;

let log2 = log 2.0;;

let bytes_of_max (x: float): int = (
	(int_of_float (ceil (log x /. log2)) + 7) / 8
);;

let bytes_of_digits (d: int): int = (
	if d <= 6 then 4 else
	if d <= 15 then 8 else
	if d <= 18 then 12 else
	16
);;

let gcc_env (command: string) (lang: [< language]): environment = (
	let target = ref "" in
	let sizeof_short = ref 0 in
	let sizeof_int = ref 0 in
	let sizeof_long = ref 0 in
	let sizeof_long_long = ref 0 in
	let sizeof_float = ref 0 in
	let sizeof_double = ref 0 in
	let sizeof_long_double = ref 0 in
	let sizeof_intptr = ref (ref 0) in
	let typedef_ptrdiff_t = ref None in
	let typedef_size_t = ref None in
	let typedef_wchar_t = ref None in
	let float_mantissa = ref 0 in
	let double_mantissa = ref 0 in
	let long_double_mantissa = ref 0 in
	let predefined = Buffer.create 4096 in
	let include_path = ref [] in
	let sys_include_path = ref [] in
	(* execute cpp *)
	let command = command ^ " -E -dM -v -x " ^ gcc_lang lang ^ " /dev/null" in
	let (p_in, _, p_err) as ps = Unix.open_process_full command (Unix.environment ()) in
	let in_eof = ref false in
	let err_eof = ref false in
	let state: [`none | `in_include | `in_sys_include] ref = ref `none in
	while not !in_eof || not !err_eof do
		if not !in_eof then (
			begin try
				let line = input_line p_in in
				let directive, line_r = Triming.take_word Triming.is_space line in
				if directive = "#define" then (
					let name, value = Triming.take_word Triming.is_space line_r in
					begin match name with
					| "__SHRT_MAX__" ->
						sizeof_short := bytes_of_max (float_of_stringl value)
					| "__INT_MAX__" ->
						sizeof_int := bytes_of_max (float_of_stringl value)
					| "__LONG_MAX__" ->
						sizeof_long := bytes_of_max (float_of_stringl value)
					| "__LONG_LONG_MAX__" ->
						sizeof_long_long := bytes_of_max (float_of_stringl value)
					| "__FLT_DIG__" ->
						sizeof_float := bytes_of_digits (int_of_string value)
					| "__DBL_DIG__" ->
						sizeof_double := bytes_of_digits (int_of_string value)
					| "__LDBL_DIG__" ->
						sizeof_long_double := bytes_of_digits (int_of_string value)
					| "__PTRDIFF_TYPE__" ->
						begin match value with
						| "int" ->
							sizeof_intptr := sizeof_int;
							typedef_ptrdiff_t := Some `signed_int
						| "long int" ->
							sizeof_intptr := sizeof_long;
							typedef_ptrdiff_t := Some `signed_long
						| _ ->
							assert false
						end
					| "__SIZE_TYPE__" ->
						begin match value with
						| "unsigned int" ->
							typedef_size_t := Some `unsigned_int
						| "long unsigned int" ->
							typedef_size_t := Some `unsigned_long
						| _ ->
							assert false
						end
					| "__WCHAR_TYPE__" ->
						begin match value with
						| "short int" ->
							typedef_wchar_t := Some `signed_short
						| "short unsigned int" ->
							typedef_wchar_t := Some `unsigned_short
						| "int" ->
							typedef_wchar_t := Some `signed_int
						| "unsigned int" ->
							typedef_wchar_t := Some `unsigned_int
						| "long int" ->
							typedef_wchar_t := Some `signed_long
						| _ ->
							assert false
						end
					| "__FLT_MANT_DIG__" ->
						float_mantissa := int_of_string value
					| "__DBL_MANT_DIG__" ->
						double_mantissa := int_of_string value
					| "__LDBL_MANT_DIG__" ->
						long_double_mantissa := int_of_string value
					| _ ->
						()
					end
				);
				Buffer.add_string predefined line;
				Buffer.add_char predefined '\n'
			with
			| End_of_file -> in_eof := true
			end
		);
		if not !err_eof then (
			begin try
				let line = input_line p_err in
				let line_length = String.length line in
				if (
					let nr = "not recognized" in
					let nr_length = String.length nr in
					line_length > nr_length
					&& String.sub line (line_length - nr_length) nr_length = nr
				) then (
					let (_: Unix.process_status) = Unix.close_process_full ps in
					raise (Failure command)
				) else if line = "End of search list." then (
					state := `none
				) else if line = "#include \"...\" search starts here:" then (
					state := `in_include
				) else if line = "#include <...> search starts here:" then (
					state := `in_sys_include
				) else (
					begin match !state with
					| `none ->
						let tg = "Target: " in
						let tg_length = String.length tg in
						if line_length > tg_length
							&& String.sub line 0 tg_length = tg
						then (
							target := String.sub line tg_length (line_length - tg_length)
						)
					| `in_include ->
						assert (line.[0] = ' ');
						let item = String.sub line 1 (String.length line - 1) in
						include_path := item :: !include_path
					| `in_sys_include ->
						assert (line.[0] = ' ');
						let item = String.sub line 1 (String.length line - 1) in
						let item_length = String.length item in
						let fw = " (framework directory)" in
						let fw_length = String.length fw in
						if item_length >= fw_length
							&& String.sub item (item_length - fw_length) fw_length = fw
						then (
							(* not handling frameworks *)
						) else (
							sys_include_path := item :: !sys_include_path
						)
					end
				)
			with
			| End_of_file -> err_eof := true
			end
		)
	done;
	begin match Unix.close_process_full ps with
	| Unix.WEXITED 0 -> ()
	| _ -> raise (Failure command)
	end;
	assert (!target <> "");
	let target =
		let target_length = String.length !target in
		let i686 = "i686-" in
		let i686_length = String.length i686 in
		let x86_64 = "x86_64-" in
		let x86_64_length = String.length x86_64 in
		if target_length > i686_length
			&& String.sub !target 0 i686_length = i686
			&& ! !sizeof_intptr = 8
		then (
			x86_64 ^ String.sub !target i686_length (target_length - i686_length)
		) else if target_length > x86_64_length
			&& String.sub !target 0 x86_64_length = x86_64
			&& ! !sizeof_intptr = 4
		then (
			i686 ^ String.sub !target x86_64_length (target_length - x86_64_length)
		) else (
			!target
		)
	in
	assert (!sizeof_short > 0);
	assert (!sizeof_int > 0);
	assert (!sizeof_long > 0);
	assert (!sizeof_long_long > 0);
	assert (!sizeof_float > 0);
	assert (!sizeof_double > 0);
	assert (!sizeof_long_double > 0);
	assert (! !sizeof_intptr > 0);
	let sizeof: sizeof =
		`sizeof_bool 1,
		`sizeof_short !sizeof_short,
		`sizeof_int !sizeof_int,
		`sizeof_long !sizeof_long,
		`sizeof_long_long !sizeof_long_long,
		`sizeof_float !sizeof_float,
		`sizeof_double !sizeof_double,
		`sizeof_long_double !sizeof_long_double,
		`sizeof_intptr ! !sizeof_intptr
	in
	assert (!typedef_ptrdiff_t <> None);
	assert (!typedef_size_t <> None);
	assert (!typedef_wchar_t <> None);
	let typedef: language_typedef =
		begin match !typedef_ptrdiff_t, !typedef_size_t, !typedef_wchar_t with
		| Some typedef_ptrdiff_t, Some typedef_size_t, Some typedef_wchar_t ->
			`typedef_ptrdiff_t typedef_ptrdiff_t,
			`typedef_size_t typedef_size_t,
			`typedef_wchar_t typedef_wchar_t
		| _ ->
			assert false
		end
	in
	assert (!float_mantissa > 0);
	assert (!double_mantissa > 0);
	assert (!long_double_mantissa > 0);
	let precision =
		!float_mantissa,
		!double_mantissa,
		!long_double_mantissa
	in
	let builtin = [
		"__builtin_alloca", [`size_t], `pointer `char;
		"__builtin_bswap32", [`unsigned_int], `unsigned_int;
		"__builtin_bswap64", [`unsigned_long_long], `unsigned_long_long;
		"__builtin_bzero", [`pointer `char; `size_t], `void;
		"__builtin_fabsf", [`float], `float;
		"__builtin_fabs", [`double], `double;
		"__builtin_fabsl", [`long_double], `long_double;
		"__builtin_flt_rounds", [], `signed_int;
		"__builtin_huge_valf", [], `float;
		"__builtin_huge_val", [], `double;
		"__builtin_huge_vall", [], `long_double;
		"__builtin_inff", [], `float;
		"__builtin_inf", [], `double;
		"__builtin_infl", [], `long_double;
		"__builtin_llabs", [`signed_long_long], `signed_long_long;
		"__builtin_memcmp", [`pointer `char; `pointer `char; `size_t], `signed_int;
		"__builtin_memset", [`pointer `char; `signed_int; `size_t], `pointer `char;
		"__builtin___memcpy_chk", [`pointer `char; `pointer (`const `char); `size_t; `size_t], `pointer `char;
		"__builtin___memmove_chk", [`pointer `char; `pointer (`const `char); `size_t; `size_t], `pointer `char;
		"__builtin___memset_chk", [`pointer `char; `signed_int; `size_t; `size_t], `pointer `char;
		"__builtin_nanf", [`pointer (`const `char)], `float;
		"__builtin_nan", [`pointer (`const `char)], `double;
		"__builtin_nanl", [`pointer (`const `char)], `long_double;
		"__builtin_return_address", [`unsigned_int], `pointer `char;
		"__builtin___stpcpy_chk", [`pointer `char; `pointer (`const `char); `size_t], `pointer `char;
		"__builtin___stpncpy_chk", [`pointer `char; `pointer (`const `char); `size_t; `size_t], `pointer `char;
		"__builtin___strcat_chk", [`pointer `char; `pointer (`const `char); `size_t], `pointer `char;
		"__builtin___strcpy_chk", [`pointer `char; `pointer (`const `char); `size_t], `pointer `char;
		"__builtin___strncat_chk", [`pointer `char; `pointer (`const `char); `size_t; `size_t], `pointer `char;
		"__builtin___strncpy_chk", [`pointer `char; `pointer (`const `char); `size_t; `size_t], `pointer `char];
	in
	let result = {
		en_target = target;
		en_sizeof = sizeof;
		en_typedef = typedef;
		en_precision = precision;
		en_predefined = Buffer.contents predefined;
		en_builtin = builtin;
		en_include = List.rev !include_path;
		en_sys_include = List.rev !sys_include_path}
	in
	result
);;
