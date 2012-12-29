(* open Value;; *)

let rev_string s = (
	let length = String.length s in
	let r = String.create length in
	let h = length - 1 in
	for i = 0 to h do
		r.[i] <- s.[h - i]
	done;
	r
);;

module Integer = struct
	type t = int;;
	let zero = 0;;
	let one = 1;;
	let of_int x = x
	let of_based_string ~base s = (
		let rec loop base s i r = (
			if i >= String.length s then (
				r
			) else (
				let n = String.index Hexadecimal.uppercase s.[i] in
				loop base s (succ i) (r * base + n)
			)
		) in
		loop base (String.uppercase s) 0 0
	);;
	let to_based_string ~base x = (
		let rec loop base buf x = (
			let d = x mod base in
			let u = x / base in
			Buffer.add_char buf Hexadecimal.uppercase.[abs d];
			if u = 0 then (
				if d < 0 then Buffer.add_char buf '-';
				rev_string (Buffer.contents buf)
			) else (
				loop base buf u
			)
		) in
		loop base (Buffer.create 32) x
	);;
	let compare: t -> t -> int = compare;;
	let neg = ( ~- );;
	let add = ( + );;
	let sub = ( - );;
	let mul = ( * );;
	let div = ( / );;
	let rem = ( mod );;
	let scale fraction ~base ~exponent = fraction * int_of_float (float_of_int base ** float_of_int exponent);;
	let to_int x = x;;
	let logand = ( land );;
	let logor = ( lor );;
	let logxor = ( lxor );;
	let lognot = lnot;;
	let shift_left left right = left lsl right;;
	let shift_right left right = left asr right;;
end;;

module Integer64 = struct
	include Int64;;
	let of_based_string ~base s = (
		let rec loop base s i r = (
			if i >= String.length s then (
				r
			) else (
				let n = String.index Hexadecimal.uppercase s.[i] in
				loop base s (Pervasives.succ i) (Int64.add (Int64.mul r base) (Int64.of_int n))
			)
		) in
		loop (Int64.of_int base) (String.uppercase s) 0 0L
	);;
	let to_based_string ~base x = (
		let rec loop base buf x = (
			let d = Int64.to_int (Int64.rem x base) in
			let u = Int64.div x base in
			Buffer.add_char buf Hexadecimal.uppercase.[Pervasives.abs d];
			if u = 0L then (
				if d < 0 then Buffer.add_char buf '-';
				rev_string (Buffer.contents buf)
			) else (
				loop base buf u
			)
		) in
		loop (Int64.of_int base) (Buffer.create 32) x
	);;
	let scale fraction ~base ~exponent = Int64.mul fraction (Int64.of_float (float_of_int base ** float_of_int exponent));;
end;;

module Real = struct
	type t = float;;
	let zero = 0.0;;
	let one = 1.0;;
	let of_int = float_of_int;;
	let of_based_string ~base s = (
		if base = 10 then float_of_string s else (
			let p = String.index s '.' in
			let integer_part = String.sub s 0 p in
			let decimal_part = String.sub s (succ p) (String.length s - p - 1) in
			let n = Integer.of_based_string ~base (integer_part ^ decimal_part) in
			let m = (float_of_int base) ** float_of_int (String.length decimal_part) in
			float_of_int n /. m
		)
	);;
	let to_based_string ~base x = (
		if base = 10 then string_of_float x else
		assert false
	);;
	let compare: t -> t -> int = compare;;
	let neg = ( ~-. );;
	let add = ( +. );;
	let sub = ( -. );;
	let mul = ( *. );;
	let div = ( /. );;
	let scale fraction ~base ~exponent = fraction *. (float_of_int base ** float_of_int exponent);;
	let frexp = frexp;;
end;;

module String16 = struct
	type elm = int;;
	type t = int array;;
	let length = Array.length;;
	let empty = [| |];;
	let of_array a = a;;
	let get a i = a.(i);;
end;;

module String32 = struct
	type elm = Int32.t;;
	type t = Int32.t array;;
	let length = Array.length;;
	let empty = [| |];;
	let of_array a = a;;
	let get a i = a.(i);;
end;;
