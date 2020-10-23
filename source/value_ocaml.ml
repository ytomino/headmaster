(* open Value;; *)

open struct
	let rev_string s = (
		let length = String.length s in
		let r = Bytes.create length in
		let h = length - 1 in
		for i = 0 to h do
			Bytes.set r i s.[h - i]
		done;
		Bytes.unsafe_to_string r
	);;
end;;

module Integer64 = struct
	include Int64;;
	let of_based_string ~base s = (
		let rec loop base s i r = (
			if i >= String.length s then (
				r
			) else (
				let n = String.index Hexadecimal.uppercase s.[i] in
				loop base s (Stdlib.succ i) (Int64.add (Int64.mul r base) (Int64.of_int n))
			)
		) in
		loop (Int64.of_int base) (String.uppercase_ascii s) 0 0L
	);;
	let to_based_string ~base x = (
		let rec loop base buf x = (
			let d = Int64.to_int (Int64.rem x base) in
			let u = Int64.div x base in
			Buffer.add_char buf Hexadecimal.uppercase.[Stdlib.abs d];
			if u = 0L then (
				if d < 0 then Buffer.add_char buf '-';
				rev_string (Buffer.contents buf)
			) else (
				loop base buf u
			)
		) in
		loop (Int64.of_int base) (Buffer.create 32) x
	);;
	let compare_int x y = Int64.compare x (Int64.of_int y);;
	let scale fraction ~base ~exponent = Int64.mul fraction (Int64.of_float (float_of_int base ** float_of_int exponent));;
	let test_bit x b = if Int64.logand x (Int64.shift_left 1L b) <> 0L then 1 else 0;;
end;;

module Real = struct
	include Float;;
	let of_based_string ~base s = (
		if base = 10 then float_of_string s else (
			let p = String.index s '.' in
			let integer_part = String.sub s 0 p in
			let decimal_part = String.sub s (Stdlib.succ p) (String.length s - p - 1) in
			let n = Integer64.of_based_string ~base (integer_part ^ decimal_part) in
			let m = (float_of_int base) ** float_of_int (String.length decimal_part) in
			Int64.to_float n /. m
		)
	);;
	let to_based_string ~base x = (
		if base = 10 then string_of_float x else
		assert false
	);;
	let scale fraction ~base ~exponent = fraction *. (float_of_int base ** float_of_int exponent);;
	let of_float x = x;;
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
