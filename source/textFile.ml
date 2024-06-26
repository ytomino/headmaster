open Position;;
open !Nonpolymorphic;;

type big_string = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;;

let big_string_of_string (s: string): big_string = (
	let length = String.length s in
	let result = Bigarray.Array1.create Bigarray.char Bigarray.c_layout length in
	for i = 0 to length - 1 do
		Bigarray.Array1.unsafe_set result i (String.unsafe_get s i)
	done;
	result
);;

open struct
	let line_compaction_width = 3;;

	type line_info = {
		li_index: int;
		li_line: int;
		li_prev_column: int};;
end;;

type t = {
	tf_filename: string;
	tf_contents: big_string;
	tf_random_access: bool;
	tf_tab_width: int;
	tf_length: int;
	mutable tf_lines: line_info list;
	mutable tf_index: int;
	mutable tf_line: int;
	mutable tf_column: int; (* 1 origin, tab is expanded *)
	mutable tf_prev_index: int;
	mutable tf_prev_line: int;
	mutable tf_prev_column: int};;

let of_big_string
	~(random_access: bool)
	~(tab_width: int)
	(filename: string)
	(contents: big_string)
	: t =
(
	let length = Bigarray.Array1.dim contents in
	let result = {
		tf_filename = filename;
		tf_contents = contents;
		tf_random_access = random_access;
		tf_tab_width = tab_width;
		tf_length = length;
		tf_lines = {li_index = 0; li_line = 1; li_prev_column = 1} :: [];
		tf_index = 0;
		tf_line = 1;
		tf_column = 1;
		tf_prev_index = -1;
		tf_prev_line = 0;
		tf_prev_column = 1}
	in
	result
);;

let of_string
	~(random_access: bool)
	~(tab_width: int)
	(filename: string)
	(contents: string)
	: t =
(
	of_big_string ~random_access ~tab_width filename (big_string_of_string contents)
);;

let of_file
	~(random_access: bool)
	~(tab_width: int)
	(filename: string)
	: t =
(
	let fd = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
	let length = Unix.lseek fd 0 Unix.SEEK_END in
	let contents =
		Bigarray.array1_of_genarray
			(Unix.map_file fd ~pos:0L Bigarray.char Bigarray.c_layout false [| length |])
	in
	Unix.close fd;
	of_big_string ~random_access ~tab_width filename contents
);;

let length (s: t): int = (
	s.tf_length
);;

let get (s: t) (index: int): char = (
	if index < 0 then invalid_arg "TextFile.get" else
	if index >= s.tf_length then '\x1a' else
	Bigarray.Array1.unsafe_get s.tf_contents index
);;

open struct
	let internal_succ (s: t) (index: int): int = (
		assert (index = s.tf_index);
		let set_succ_line (s: t): unit = (
			let next_line = s.tf_line + 1 in
			if s.tf_random_access && (List.hd s.tf_lines).li_index + line_compaction_width < s.tf_index then (
				s.tf_lines <- {li_index = s.tf_index; li_line = next_line; li_prev_column = s.tf_column} :: s.tf_lines
			);
			s.tf_line <- next_line;
			s.tf_column <- 1
		) in
		(* save previous position *)
		s.tf_prev_index <- s.tf_index;
		s.tf_prev_line <- s.tf_line;
		s.tf_prev_column <- s.tf_column;
		(* next position *)
		let next_index = index + 1 in
		s.tf_index <- next_index;
		begin match get s index with
		| '\n' | '\x0c' ->
			set_succ_line s
		| '\r' ->
			if get s next_index = '\n' then (
				s.tf_column <- s.tf_column + 1
			) else (
				set_succ_line s
			)
		| '\t' ->
			s.tf_column <- s.tf_column + (s.tf_tab_width - (s.tf_column - 1) mod s.tf_tab_width)
		| _ ->
			s.tf_column <- s.tf_column + 1
		end;
		next_index
	);;

	let seek (s: t) (to_index: int): unit = (
		if to_index < 0 then invalid_arg "TextFile.succ" else
		if s.tf_random_access then (
			if to_index < max s.tf_index (List.hd s.tf_lines).li_index then (
				let rec find (xs: line_info list) (to_index: int): line_info = (
					let x = List.hd xs in
					if to_index >= x.li_index then x else
					find (List.tl xs) to_index
				) in
				let line = find s.tf_lines to_index in
				s.tf_index <- line.li_index;
				s.tf_line <- line.li_line;
				s.tf_column <- 1;
				s.tf_prev_index <- line.li_index - 1;
				s.tf_prev_line <- line.li_line - 1;
				s.tf_prev_column <- line.li_prev_column
			)
		) else if to_index <> s.tf_index then (
			failwith "TextFile.succ"
		);
		(* seek within the line or unscanned area *)
		let rec loop (s: t) (to_index: int): unit = (
			if to_index > s.tf_index then (
				let (_: int) = internal_succ s s.tf_index in
				loop s to_index
			)
		) in
		loop s to_index
	);;
end;;

let position (s: t) (index: int): position = (
	if index <> s.tf_index then seek s index;
	s.tf_filename, s.tf_index, s.tf_line, s.tf_column
);;

let prev_position (s: t) (index: int): position = (
	if index <> s.tf_index then seek s index;
	s.tf_filename, s.tf_prev_index, s.tf_prev_line, s.tf_prev_column
);;

let succ (s: t) (index: int): int = (
	if index <> s.tf_index then seek s index;
	internal_succ s index
);;

let succ_eol (s: t) (index: int): int = (
	begin match get s index with
	| '\n' | '\x0c' ->
		succ s index
	| '\r' ->
		let index = succ s index in
		if get s index = '\n' then (
			internal_succ s index
		) else (
			index
		)
	| _ ->
		failwith "TextFile.succ_eol"
	end
);;

open struct
	let add_nothing _ (_: char) = ();;
end;;

let make_succ_while (type a) (add: a -> char -> unit) (a: a) (f: char -> bool)
	(s: t) (index: int): int =
(
	if index <> s.tf_index then seek s index;
	let rec internal_succ_while add a f s index = (
		let c = get s index in
		if not (f c) then index else (
			add a c;
			let index = internal_succ s index in
			internal_succ_while add a f s index
		)
	) in
	internal_succ_while add a f s index
);;

let succ_while = make_succ_while add_nothing ();;
let succ_while_to_buffer = make_succ_while Buffer.add_char;;

let make_succ_until_eol (type a) (add: a -> char -> unit) (a: a)
	(is_escape: char -> bool) (s: t) (index: int): int =
(
	if index <> s.tf_index then seek s index;
	let rec internal_succ_until_eol add a is_escape s index = (
		begin match get s index with
		| '\n' | '\r' | '\x0c' | '\x1a' ->
			index
		| _ as c ->
			let index =
				let index = internal_succ s index in
				if is_escape c
					&& (
						match get s index with
						| '\n' | '\r' | '\x0c' -> true
						| _ -> false)
				then (
					succ_eol s index (* skip pair of escape and escaped *)
				) else (
					add a c;
					index
				)
			in
			internal_succ_until_eol add a is_escape s index
		end
	) in
	internal_succ_until_eol add a is_escape s index
);;

let succ_until_eol = make_succ_until_eol add_nothing ();;
let succ_until_eol_to_buffer = make_succ_until_eol Buffer.add_char;;
