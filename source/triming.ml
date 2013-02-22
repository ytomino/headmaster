let rec trim_left
	(sub: string -> int -> int -> 'a)
	(f: char -> bool)
	(s: string) (start: int) (next: int): 'a =
(
	if start < next && f s.[start] then trim_left sub f s (start + 1) next else
	sub s start (next - start)
);;

let rec trim_right
	(sub: string -> int -> int -> 'a)
	(f: char -> bool)
	(s: string) (start: int) (next: int): 'a =
(
	let p_next = next - 1 in
	if start < next && f s.[p_next] then trim_right sub f s start p_next else
	sub s start (next - start)
);;

let trim
	(f: char -> bool)
	(s: string): string =
(
	trim_right (trim_left String.sub f) f s 0 (String.length s)
);;

let take_word
	(is_separator: char -> bool)
	(s: string): string * string =
(
	let is_not_separator c = not (is_separator c) in
	trim_left
		(fun s start len ->
			let hd = String.sub s 0 start in
			let tl = trim_left String.sub is_separator s start (start + len) in
			hd, tl)
		is_not_separator
		s
		0
		(String.length s)
);;

let is_space (c: char): bool = (
	c = ' ' || c = '\t'
);;
