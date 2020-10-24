let rec trim_left
	(sub: string -> int -> int -> 'a)
	(f: char -> bool)
	(s: string) (start: int) (len: int): 'a =
(
	if len <= 0 || not (f s.[start]) then sub s start len else
	trim_left sub f s (start + 1) (len - 1)
);;

let rec trim_right
	(sub: string -> int -> int -> 'a)
	(f: char -> bool)
	(s: string) (start: int) (len: int): 'a =
(
	let pred_len = len - 1 in
	if len <= 0 || not (f s.[start + pred_len]) then sub s start len else
	trim_right sub f s start pred_len
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
	trim_left (fun s start len ->
		let hd = String.sub s 0 start in
		let tl = trim_left String.sub is_separator s start len in
		hd, tl
	) is_not_separator s 0 (String.length s)
);;

let is_space (c: char): bool = (
	c = ' ' || c = '\t'
);;
