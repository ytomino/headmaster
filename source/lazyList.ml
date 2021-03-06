type ('a, 'b, 'c) t = ('a, 'b, 'c) prim lazy_t
and ('a, 'b, 'c) prim = [
	| `nil of 'a * 'c
	| `cons of 'a * 'b * ('a, 'b, 'c) t];;

type ('a, 'c) nil = ('a, 'c) nil_prim lazy_t
and ('a, 'c) nil_prim = [`nil of 'a * 'c];;

let rec append (xs: ('a, 'b, 'c1) t) (ys: ('a, 'b, 'c2) t): ('a, 'b, 'c2) prim = (
	begin match xs with
	| lazy (`nil _) ->
		Lazy.force ys
	| lazy (`cons (a, b, xr)) ->
		`cons (a, b, lazy (append xr ys))
	end
);;

let rec append_f (xs: ('a, 'b, 'c1) t) (make_ys: 'a * 'c1 -> ('a, 'b, 'c2) prim): ('a, 'b, 'c2) prim = (
	begin match xs with
	| lazy (`nil ac) ->
		make_ys ac
	| lazy (`cons (a, b, xr)) ->
		`cons (a, b, lazy (append_f xr make_ys))
	end
);;

let rec concat (xs: ('a, 'b, 'c) t list): ('a, 'b, 'c) prim = (
	begin match xs with
	| x :: [] ->
		Lazy.force x
	| x :: xr ->
		append x (lazy (concat xr))
	| [] ->
		failwith "LazyList.concat"
	end
);;

let rec find_nil (xs: ('a, 'b, 'c) t): ('a, 'c) nil_prim = (
	begin match xs with
	| lazy (`nil _ as result) ->
		result
	| lazy (`cons (_, _, xr)) ->
		find_nil xr
	end
);;

let hd (xs: ('a, 'b, 'c) t): 'b = (
	begin match xs with
	| lazy (`cons (_, b, _)) ->
		b
	| lazy (`nil _) ->
		failwith "LazyList.hd"
	end
);;

let hd_a (xs: ('a, 'b, 'c) t): 'a = (
	let lazy (`nil (a, _) | `cons (a, _, _)) = xs in
	a
);;

let tl (xs: ('a, 'b, 'c) t): ('a, 'b, 'c) t = (
	begin match xs with
	| lazy (`cons (_, _, xr)) ->
		xr
	| lazy (`nil _) ->
		failwith "LazyList.tl"
	end
);;

let is_empty (xs: ('a, 'b, 'c) t): bool = (
	begin match xs with
	| lazy (`cons _) ->
		false
	| lazy (`nil _) ->
		true
	end
);;

let rec map (f: 'b1 -> 'b2) (xs: ('a, 'b1, 'c) t): ('a, 'b2, 'c) prim = (
	begin match xs with
	| lazy (`nil _ as result) ->
		result
	| lazy (`cons (a, b, xr)) ->
		`cons (a, f b, lazy (map f xr))
	end
);;

let rec map_a (f: 'a1 -> 'a2) (xs: ('a1, 'b, 'c) t): ('a2, 'b, 'c) prim = (
	begin match xs with
	| lazy (`nil (a, c)) ->
		`nil (f a, c)
	| lazy (`cons (a, b, xr)) ->
		`cons (f a, b, lazy (map_a f xr))
	end
);;

let rec map_nil (f: 'c1 -> 'c2) (xs: ('a, 'b, 'c1) t): ('a, 'b, 'c2) prim = (
	begin match xs with
	| lazy (`nil (a, c)) ->
		`nil (a, f c)
	| lazy (`cons (a, b, xr)) ->
		`cons (a, b, lazy (map_nil f xr))
	end
);;
