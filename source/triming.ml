let rec trim_left (s: string) (first: int) (last_plus_1: int): string = (
	if first >= last_plus_1 then "" else
	begin match s.[first] with
	| ' ' | '\t' ->
		trim_left s (first + 1) last_plus_1
	| _ ->
		String.sub s first (last_plus_1 - first)
	end
);;

let trim (s: string): string = (
	let rec rloop s first last_plus_1 = (
		if first >= last_plus_1 then "" else
		begin match s.[last_plus_1 - 1] with
		| ' ' | '\t' ->
			rloop s first (last_plus_1 - 1)
		| _ ->
			trim_left s first last_plus_1
		end
	) in
	rloop s 0 (String.length s)
);;

let take_word (separator: char -> bool) (s: string): string * string = (
	let rec loop s i = (
		let length = String.length s in
		if i >= length then (
			s, ""
		) else if separator s.[i] then (
			let hd = String.sub s 0 i in
			let tl = trim_left s (i + 1) (String.length s) in
			hd, tl
		) else (
			loop s (i + 1)
		)
	) in
	loop s 0
);;

let is_space (c: char): bool = (
	c = ' ' || c = '\t'
);;
