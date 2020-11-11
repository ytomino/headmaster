(* suffix q means physical equality *)
(* suffix s means slicing *)

(* set *)

let rec finds (p: 'a -> bool) (xs: 'a list): 'a list = (
	begin match xs with
	| [] ->
		[]
	| a :: xr ->
		if p a then xs else
		finds p xr
	end
);;

let rec mem (equal: 'a -> 'a -> bool) (a: 'a) (xs: 'a list): bool = (
	begin match xs with
	| [] ->
		false
	| x :: xr ->
		if equal x a then true else
		mem equal a xr
	end
);;

let add (equal: 'a -> 'a -> bool) (a: 'a) (xs: 'a list): 'a list = (
	if mem equal a xs then xs else
	a :: xs
);;

let addq (a: 'a) (xs: 'a list): 'a list = (
	if List.memq a xs then xs else
	a :: xs
);;

let unionq (xs: 'a list) (ys: 'a list): 'a list = (
	if ys = [] then xs else
	let rec loop xs ys = (
		begin match xs with
		| [] ->
			ys
		| x :: xr ->
			let rs = (if List.memq x ys then ys else x :: ys) in
			loop xr rs
		end
	) in
	loop xs ys
);;

let removeq (a: 'a) (xs: 'a list): 'a list = (
	let rec loop a ys xs orig_xs = (
		begin match xs with
		| [] ->
			orig_xs
		| x :: xr ->
			if x == a then (
				List.rev_append ys xr
			) else (
				loop a (x :: ys) xr orig_xs
			)
		end
	) in
	loop a [] xs xs
);;

(* map *)

let rec assocs (equal: 'a -> 'a -> bool) (key: 'a) xs = (
	begin match xs with
	| [] ->
		[]
	| (a, _) :: xr ->
		if equal a key then xs else
		assocs equal key xr
	end
);;

let rec assqs key xs = (
	begin match xs with
	| [] ->
		[]
	| (a, _) :: xr ->
		if a == key then xs else
		assqs key xr
	end
)
