open C_lexical;;
open Value;;

module NumericScanner
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType (Literals).S) =
struct
	open Literals;;
	open LexicalElement;;
	
	let not_10_based_float_literal =
		"floating-point literal should be 10-based.";;
	let bad_decimal_suffix =
		"the suffix of decimal literal should be one of DF, DD, DL.";;
	let not_numeric_literal =
		"not numeric literal";;
	
	let scan_numeric_literal
		(error: ('p * 'p) -> string -> unit)
		(position: 's -> 'i -> 'p)
		(prev_position: 's -> 'i -> 'p)
		(get: 's -> 'i -> char)
		(succ: 's -> 'i -> 'i)
		(source: 's)
		(index: 'i)
		: [> `numeric_literal of string * numeric_literal] * 'i =
	(
		(* reading functions *)
		let rec read_digits_to_buffer ~(base: int) (b: Buffer.t) (index: 'i): 'i = (
			let h = get source index in
			if (h >= '0' && h <= '9' && Char.code h - Char.code '0' < base)
				|| (((h >= 'A' && h <= 'F') || (h >= 'a' && h <= 'f')) && base = 16)
			then (
				Buffer.add_char b h;
				let index = succ source index in
				read_digits_to_buffer ~base b index
			) else (
				index
			)
		) in
		let read_sign_to_buffer (b: Buffer.t) (index: 'i): 'i = (
			begin match get source index with
			| '+' as h ->
				Buffer.add_char b h;
				let index = succ source index in
				index
			| '-' as h ->
				Buffer.add_char b h;
				let index = succ source index in
				index
			| _ ->
				index
			end
		) in
		let read_exponent (b: Buffer.t) (index: 'i): int * 'i = (
			let exponent_start = Buffer.length b in
			let index = read_sign_to_buffer b index in
			let index = read_digits_to_buffer ~base:10 b index in
			let image =
				(* int_of_string could not handle '+' *)
				let exponent_start =
					if Buffer.nth b exponent_start = '+' then exponent_start + 1 else
					exponent_start
				in
				Buffer.sub b exponent_start (Buffer.length b - exponent_start)
			in
			let result = int_of_string image in
			result, index
		) in
		let read_base_prefix (b: Buffer.t) (index: 'i): int * 'i = (
			if get source index = '0' then (
				Buffer.add_char b '0';
				let index = succ source index in
				begin match get source index with
				| 'x' | 'X' as h ->
					Buffer.add_char b h;
					let index = succ source index in
					16, index (* "0x..." *)
				| '.' ->
					10, index (* "0.123..." *)
				| _ ->
					8, index (* "0123..." including just "0" *)
				end
			) else (
				10, index (* "123..." *)
			)
		) in
		(* body *)
		let b = Buffer.create 16 in
		let wrap (literal: numeric_literal) (index: 'i) = (
			`numeric_literal ((Buffer.contents b), literal), index
		) in
		begin match get source index with
		| '0'..'9' ->
			let p1 = position source index in
			let base, index = read_base_prefix b index in
			let start = Buffer.length b in
			let index = read_digits_to_buffer ~base b index in
			begin match get source index with
			| '.' | 'e' | 'E' | 'p' | 'P' as h -> (* real *)
				let index =
					if h = '.' then (
						Buffer.add_char b h;
						let index = succ source index in
						read_digits_to_buffer  ~base:10 b index
					) else (
						index
					)
				in
				let mantissa =
					let image = Buffer.sub b start (Buffer.length b - start) in
					Real.of_based_string ~base:10 image
				in
				if base <> 10 then (
					let p2 = prev_position source index in
					error (p1, p2) not_10_based_float_literal
				);
				let value, index =
					begin match get source index with
					| 'e' | 'E' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						let exponent, index = read_exponent b index in
						let value = Real.scale mantissa ~base:10 ~exponent in
						value, index
					| 'p' | 'P' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						let exponent, index = read_exponent b index in
						let value = Real.scale mantissa ~base:2 ~exponent in
						value, index
					| _ ->
						mantissa, index
					end
				in
				begin match get source index with
				| 'd' | 'D' as h ->
					Buffer.add_char b h;
					let index = succ source index in
					begin match get source index with
					| 'f' | 'F' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						wrap (`float_literal (`decimal32, value)) index
					| 'd' | 'D' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						wrap (`float_literal (`decimal64, value)) index
					| 'l' | 'L' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						wrap (`float_literal (`decimal128, value)) index
					| _ ->
						let p2 = prev_position source index in
						error (p1, p2) bad_decimal_suffix;
						wrap (`float_literal (`decimal32, value)) index
					end
				| 'f' | 'F' as h ->
					Buffer.add_char b h;
					let index = succ source index in
					let value = round_to_float value in
					begin match get source index with
					| 'i' | 'I' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						wrap (`imaginary_literal (`float, value)) index
					| _ ->
						wrap (`float_literal (`float, value)) index
					end
				| 'i' | 'I' as h -> (* imaginary literal is gcc's extended?? *)
					Buffer.add_char b h;
					let index = succ source index in
					begin match get source index with
					| 'f' | 'F' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						let value = round_to_float value in
						wrap (`imaginary_literal (`float, value)) index
					| 'l' | 'L' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						wrap (`imaginary_literal (`long_double, value)) index
					| _ ->
						let value = round_to_double value in
						wrap (`imaginary_literal (`double, value)) index
					end
				| 'l' | 'L' as h ->
					Buffer.add_char b h;
					let index = succ source index in
					begin match get source index with
					| 'i' | 'I' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						wrap (`imaginary_literal (`long_double, value)) index
					| _ ->
						wrap (`float_literal (`long_double, value)) index
					end
				| _ ->
					let value = round_to_double value in
					wrap (`float_literal (`double, value)) index
				end
			| _ as h -> (* integer *)
				let value =
					let length = Buffer.length b - start in
					if length = 0 then Integer.zero else
					let image = Buffer.sub b start length in
					Integer.of_based_string ~base image
				in
				begin match h with
				| 'U' | 'u' as h ->
					Buffer.add_char b h;
					let index = succ source index in
					begin match get source index with
					| 'L' | 'l' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						begin match get source index with
						| 'L' | 'l' as h ->
							Buffer.add_char b h;
							let index = succ source index in
							wrap (`int_literal (`unsigned_long_long, value)) index
						| _ ->
							wrap (`int_literal (`unsigned_long, value)) index
						end
					| _ ->
						wrap (`int_literal (`unsigned_int, value)) index
					end
				| 'L' | 'l' as h ->
					Buffer.add_char b h;
					let index = succ source index in
					begin match get source index with
					| 'L' | 'l' as h ->
						Buffer.add_char b h;
						let index = succ source index in
						wrap (`int_literal (`signed_long_long, value)) index
					| _ ->
						wrap (`int_literal (`signed_long, value)) index
					end
				| _ ->
					wrap (`int_literal (`signed_int, value)) index
				end
			end
		| _ ->
			let p1 = position source index in
			let p2 = prev_position source index in
			error (p1, p2) not_numeric_literal;
			wrap (`int_literal (`signed_int, Integer.zero)) index
		end
	);;
	
end;;
