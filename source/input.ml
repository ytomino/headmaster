type position = (string * int * int * int);;
type ranged_position = position * position;;

module TextStream = struct
	
	type t = {
		ts_filename: string;
		ts_buffer: string;
		ts_tab_width: int;
		mutable ts_index: int;
		mutable ts_length: int;
		mutable ts_position: int;
		mutable ts_line: int;
		mutable ts_column: int;
		mutable ts_prev_position: int;
		mutable ts_prev_line: int;
		mutable ts_prev_column: int;
		mutable ts_cr: bool;
		mutable ts_hd: char;
		ts_input: string -> int -> int -> int};;
	
	let fill (s: t): unit = (
		if s.ts_index >= s.ts_length then (
			s.ts_index <- 0;
			s.ts_length <- s.ts_input s.ts_buffer 0 (String.length s.ts_buffer)
		);
		s.ts_hd <-
			if s.ts_index < s.ts_length then s.ts_buffer.[s.ts_index] else
			'\x1a'
	);;
	
	let create
		(filename: string)
		(tab_width: int)
		(input: string -> int -> int -> int): t =
	(
		let result = {
			ts_filename = filename;
			ts_buffer = String.create 256;
			ts_tab_width = tab_width;
			ts_index = 0;
			ts_length = 0;
			ts_position = 0;
			ts_line = 1;
			ts_column = 1;
			ts_prev_position = 0;
			ts_prev_line = 1;
			ts_prev_column = 1;
			ts_cr = false;
			ts_input = input;
			ts_hd = '\x00'}
		in
		fill result;
		result
	);;
	
	let position (s: t): position = (
		s.ts_filename, s.ts_position, s.ts_line, s.ts_column
	);;
	
	let prev_position (s: t): position = (
		s.ts_filename, s.ts_prev_position, s.ts_prev_line, s.ts_prev_column
	);;
	
	let is_empty (s: t): bool = s.ts_index >= s.ts_length;;
	
	let hd (s: t): char = s.ts_hd;;
	
	let junk (s: t): unit = (
		let c = hd s in
		s.ts_prev_position <- s.ts_position;
		s.ts_prev_line <- s.ts_line;
		s.ts_prev_column <- s.ts_column;
		s.ts_index <- s.ts_index + 1;
		s.ts_position <- s.ts_position + 1;
		begin match c with
		| '\n' | '\x0c' ->
			s.ts_cr <- false;
			s.ts_line <- s.ts_line + 1;
			s.ts_column <- 1
		| '\r' ->
			fill s;
			if hd s = '\n' then (
				s.ts_column <- s.ts_column + 1;
				s.ts_cr <- true
			) else (
				s.ts_line <- s.ts_line + 1;
				s.ts_column <- 1
			)
		| '\t' ->
			s.ts_column <- s.ts_column + s.ts_tab_width -
				(s.ts_column - 1) mod s.ts_tab_width
		| _ ->
			s.ts_column <- s.ts_column + 1
		end;
		fill s
	);;
	
	let junk_with_position (s: t): position = (
		let p = position s in
		junk s;
		p
	);;
	
	let junk_newline (s: t): unit = (
		begin match hd s with
		| '\n' | '\x0c' ->
			junk s
		| '\r' ->
			junk s;
			if hd s = '\n' then (
				junk s
			)
		| _ ->
			raise (Failure "junk_newline")
		end
	);;
	
	let junk_newline_with_ranged_position (s: t): ranged_position = (
		let p1 = position s in
		let p2 =
			begin match hd s with
			| '\n' | '\x0c' ->
				junk s;
				p1
			| '\r' ->
				junk s;
				if hd s = '\n' then (
					let p2 = position s in
					junk s;
					p2
				) else (
					p1
				)
			| _ ->
				raise (Failure "junk_newline_with_ranged_position")
			end
		in
		(p1, p2)
	);;
	
end;;
