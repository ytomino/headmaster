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
		(position: 's -> 'p)
		(prev_position: 's -> 'p)
		(hd: 's -> char)
		(junk: 's -> unit)
		(stream: 's)
		: [> `numeric_literal of string * numeric_literal] =
	(
		let rec read_digits_to_buffer ~(base: int) (b: Buffer.t): unit = (
			let h = hd stream in
			if (h >= '0' && h <= '9' && Char.code h - Char.code '0' < base)
				|| (((h >= 'A' && h <= 'F') || (h >= 'a' && h <= 'f')) && base = 16)
			then (
				Buffer.add_char b h;
				junk stream;
				read_digits_to_buffer ~base b
			)
		) in
		let read_sign (b: Buffer.t): int = (
			begin match hd stream with
			| '+' as h ->
				Buffer.add_char b h;
				junk stream;
				1
			| '-' as h ->
				Buffer.add_char b h;
				junk stream;
				-1
			| _ ->
				1
			end
		) in
		let read_exponent (b: Buffer.t): int = (
			let exponent_sign = read_sign b in
			let exponent_start = Buffer.length b in
			read_digits_to_buffer ~base:10 b;
			let image = Buffer.sub b exponent_start (Buffer.length b - exponent_start) in
			exponent_sign * int_of_string image
		) in
		let b = Buffer.create 16 in
		let literal =
			begin match hd stream with
			| '0' .. '9' as h ->
				let p1 = position stream in
				let base =
					if h = '0' then (
						Buffer.add_char b h;
						junk stream;
						begin match hd stream with
						| 'x' | 'X' as h ->
							Buffer.add_char b h;
							junk stream;
							16 (* "0x..." *)
						| '.' ->
							10 (* "0.123..." *)
						| _ ->
							8 (* "0123..." including just "0" *)
						end
					) else (
						10 (* "123..." *)
					)
				in
				let start = Buffer.length b in
				read_digits_to_buffer ~base b;
				begin match hd stream with
				| '.' | 'e' | 'E' | 'p' | 'P' as h -> (* real *)
					if h = '.' then (
						Buffer.add_char b h;
						junk stream;
						read_digits_to_buffer  ~base:10 b
					);
					let mantissa =
						let image = Buffer.sub b start (Buffer.length b - start) in
						Real.of_based_string ~base:10 image
					in
					if base <> 10 then (
						let p2 = prev_position stream in
						error (p1, p2) not_10_based_float_literal
					);
					let value =
						begin match hd stream with
						| 'e' | 'E' as h ->
							Buffer.add_char b h;
							junk stream;
							let exponent = read_exponent b in
							Real.scale mantissa ~base:10 ~exponent
						| 'p' | 'P' as h ->
							Buffer.add_char b h;
							junk stream;
							let exponent = read_exponent b in
							Real.scale mantissa ~base:2 ~exponent
						| _ ->
							mantissa
						end
					in
					begin match hd stream with
					| 'd' | 'D' as h ->
						Buffer.add_char b h;
						junk stream;
						begin match hd stream with
						| 'f' | 'F' as h ->
							Buffer.add_char b h;
							junk stream;
							`float_literal (`decimal32, value)
						| 'd' | 'D' as h ->
							Buffer.add_char b h;
							junk stream;
							`float_literal (`decimal64, value)
						| 'l' | 'L' as h ->
							Buffer.add_char b h;
							junk stream;
							`float_literal (`decimal128, value)
						| _ ->
							let p2 = prev_position stream in
							error (p1, p2) bad_decimal_suffix;
							`float_literal (`decimal32, value)
						end
					| 'f' | 'F' as h ->
						Buffer.add_char b h;
						junk stream;
						let value = round_to_float value in
						begin match hd stream with
						| 'i' | 'I' as h ->
							Buffer.add_char b h;
							junk stream;
							`imaginary_literal (`float, value)
						| _ ->
							`float_literal (`float, value)
						end
					| 'i' | 'I' as h -> (* imaginary literal is gcc's extended?? *)
						Buffer.add_char b h;
						junk stream;
						begin match hd stream with
						| 'f' | 'F' as h ->
							Buffer.add_char b h;
							junk stream;
							let value = round_to_float value in
							`imaginary_literal (`float, value)
						| 'l' | 'L' as h ->
							Buffer.add_char b h;
							junk stream;
							`imaginary_literal (`long_double, value)
						| _ ->
							let value = round_to_double value in
							`imaginary_literal (`double, value)
						end
					| 'l' | 'L' as h ->
						Buffer.add_char b h;
						junk stream;
						begin match hd stream with
						| 'i' | 'I' as h ->
							Buffer.add_char b h;
							junk stream;
							`imaginary_literal (`long_double, value)
						| _ ->
							`float_literal (`long_double, value)
						end
					| _ ->
						let value = round_to_double value in
						`float_literal (`double, value)
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
						junk stream;
						begin match hd stream with
						| 'L' | 'l' as h ->
							Buffer.add_char b h;
							junk stream;
							begin match hd stream with
							| 'L' | 'l' as h ->
								Buffer.add_char b h;
								junk stream;
								`int_literal (`unsigned_long_long, value)
							| _ ->
								`int_literal (`unsigned_long, value)
							end
						| _ ->
							`int_literal (`unsigned_int, value)
						end
					| 'L' | 'l' as h ->
						Buffer.add_char b h;
						junk stream;
						begin match hd stream with
						| 'L' | 'l' as h ->
							Buffer.add_char b h;
							junk stream;
							`int_literal (`signed_long_long, value)
						| _ ->
							`int_literal (`signed_long, value)
						end
					| _ ->
						`int_literal (`signed_int, value)
					end
				end
			| _ ->
				let p1 = position stream in
				let p2 = prev_position stream in
				error (p1, p2) not_numeric_literal;
				`int_literal (`signed_int, Integer.zero)
			end
		in
		`numeric_literal ((Buffer.contents b), literal)
	);;
	
end;;
