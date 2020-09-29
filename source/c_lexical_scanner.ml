open C_lexical;;
open C_literals;;

module type NumericScannerType = sig
	module Literals: LiteralsType
	module LexicalElement: LexicalElementType
		with module Literals := Literals
	
	val scan_numeric_literal:
		(('p * 'p) -> string -> unit) ->
		Buffer.t ->
		('s -> 'i -> 'p) ->
		('s -> 'i -> 'p) ->
		('s -> 'i -> char) ->
		('s -> 'i -> 'i) ->
		('s) ->
		'i ->
		[> `numeric_literal of string * LexicalElement.numeric_literal] * 'i
	
end;;

module NumericScanner
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType
		with module Literals := Literals)
	: NumericScannerType
		with module Literals := Literals
		with module LexicalElement := LexicalElement =
struct
	open Literals;;
	
	(* error messages *)
	
	let bad_8_based_int_literal =
		"invalid digit in the octal int literal.";;
	let exponent_is_required =
		"exponent is required for the hexadecimal float literal.";;
	let bad_fN_suffix =
		"the suffix of sized float literal should be one of f32, f64, f128.";;
	let bad_fNx_suffix =
		"the suffix of half sized float literal should be one of f32x, f64x.";;
	let bad_decimal_suffix =
		"the suffix of decimal literal should be one of DF, DD, DL.";;
	let not_numeric_literal =
		"not numeric literal";;
	
	(* char handling *)
	
	let is_digit (base: int) (c: char): bool = (
		(c >= '0' && c <= '9' && Char.code c - Char.code '0' < base)
		|| (((c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')) && base = 16)
	);;
	
	(* scanner *)
	
	let scan_numeric_literal
		(error: ('p * 'p) -> string -> unit)
		(buf: Buffer.t)
		(position: 's -> 'i -> 'p)
		(prev_position: 's -> 'i -> 'p)
		(get: 's -> 'i -> char)
		(succ: 's -> 'i -> 'i)
		(source: 's)
		(index: 'i)
		: [> `numeric_literal of string * LexicalElement.numeric_literal] * 'i =
	(
		(* reading functions *)
		let rec read_digits_to_buffer ~(base: int) (buf: Buffer.t) (index: 'i): 'i = (
			let h = get source index in
			if is_digit base h then (
				Buffer.add_char buf h;
				let index = succ source index in
				read_digits_to_buffer ~base buf index
			) else (
				index
			)
		) in
		let read_sign_to_buffer (buf: Buffer.t) (index: 'i): 'i = (
			begin match get source index with
			| '+' as h ->
				Buffer.add_char buf h;
				let index = succ source index in
				index
			| '-' as h ->
				Buffer.add_char buf h;
				let index = succ source index in
				index
			| _ ->
				index
			end
		) in
		let read_exponent (buf: Buffer.t) (index: 'i): int * 'i = (
			let exponent_start = Buffer.length buf in
			let index = read_sign_to_buffer buf index in
			let index = read_digits_to_buffer ~base:10 buf index in
			let image =
				(* int_of_string could not handle '+' *)
				let exponent_start =
					if Buffer.nth buf exponent_start = '+' then exponent_start + 1 else
					exponent_start
				in
				Buffer.sub buf exponent_start (Buffer.length buf - exponent_start)
			in
			let result = int_of_string image in
			result, index
		) in
		let read_base_prefix (buf: Buffer.t) (index: 'i): int * 'i = (
			if get source index = '0' then (
				Buffer.add_char buf '0';
				let index = succ source index in
				begin match get source index with
				| 'x' | 'X' as h ->
					Buffer.add_char buf h;
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
		Buffer.reset buf;
		let integer_of_based ~(base: int) (start: int) (length: int) = (
			if length = 0 then Integer.zero else
			let image = Buffer.sub buf start length in
			Integer.of_based_string ~base image
		) in
		let wrap (literal: LexicalElement.numeric_literal) (index: 'i) = (
			`numeric_literal ((Buffer.contents buf), literal), index
		) in
		begin match get source index with
		| '0'..'9' ->
			let p1 = position source index in
			let base, index = read_base_prefix buf index in
			let start = Buffer.length buf in
			let index = read_digits_to_buffer ~base buf index in
			let i_end = Buffer.length buf in
			begin match get source index with
			| '8' | '9' | '.' | 'e' | 'E' | 'p' | 'P' as h -> (* real *)
				let m_base, p3, h, index =
					begin match h with
					| '8' | '9' ->
						let p3 = position source index in
						let index = read_digits_to_buffer ~base:10 buf index in
						let h = get source index in
						10, p3, h, index
					| _ ->
						base, p1, h, index
					end
				in
				if base = 8 && m_base = 10 && h <> '.' then (
					let p4 = prev_position source index in
					error (p3, p4) bad_8_based_int_literal;
					let value = integer_of_based ~base start (i_end - start) in
					wrap (`int_literal (`signed_int, value)) index
				) else
				let index =
					if h = '.' then (
						Buffer.add_char buf h;
						let index = succ source index in
						read_digits_to_buffer ~base:m_base buf index
					) else (
						index
					)
				in
				let mantissa =
					let image = Buffer.sub buf start (Buffer.length buf - start) in
					Real.of_based_string ~base:m_base image
				in
				let e_base, exponent, index =
					begin match get source index with
					| 'e' | 'E' | 'p' | 'P' as h ->
						let index =
							begin match h with
							| 'e' | 'E' ->
								Buffer.add_char buf h;
								succ source index
							| _ ->
								index
							end
						in
						let e_base, index =
							begin match get source index with
							| 'p' | 'P' as h ->
								Buffer.add_char buf h;
								2, succ source index
							| _ ->
								10, index
							end
						in
						let exponent, index = read_exponent buf index in
						e_base, exponent, index
					| _ ->
						if m_base <> 10 then (
							let p2 = prev_position source index in
							error (p1, p2) exponent_is_required
						);
						10, 0, index
					end
				in
				begin match get source index with
				| 'd' | 'D' as h ->
					Buffer.add_char buf h;
					let index = succ source index in
					let prec, index =
						begin match get source index with
						| 'f' | 'F' as h ->
							Buffer.add_char buf h;
							let index = succ source index in
							`_Decimal32, index
						| 'd' | 'D' as h ->
							Buffer.add_char buf h;
							let index = succ source index in
							`_Decimal64, index
						| 'l' | 'L' as h ->
							Buffer.add_char buf h;
							let index = succ source index in
							`_Decimal128, index
						| _ ->
							let p2 = prev_position source index in
							error (p1, p2) bad_decimal_suffix;
							`_Decimal32, index
						end
					in
					wrap (`decimal_literal prec) index
				| 'f' | 'F' as h ->
					Buffer.add_char buf h;
					let index = succ source index in
					let value = Real.scale mantissa ~base:e_base ~exponent in
					begin match get source index with
					| '0'..'9' -> (* fN, fNx *)
						let index = read_digits_to_buffer ~base:10 buf index in
						let bit_size = 32 in
						let prec, index =
							begin match get source index with
							| 'x' as h -> (* lowercase only? *)
								Buffer.add_char buf h;
								let index = succ source index in
								let prec =
									begin match bit_size with
									| 32 ->
										`_Float32x
									| 64 ->
										`_Float64x
									| _ ->
										let p2 = prev_position source index in
										error (p1, p2) bad_fNx_suffix;
										`_Float64x (* fallback *)
									end
								in
								prec, index
							| _ ->
								let prec =
									begin match bit_size with
									| 32 ->
										`_Float32
									| 64 ->
										`_Float64
									| 128 ->
										`_Float128
									| _ ->
										let p2 = prev_position source index in
										error (p1, p2) bad_fN_suffix;
										`_Float128 (* fallback *)
									end
								in
								prec, index
							end
						in
						let value = round ~prec:bit_size value in
						wrap (`float_literal (prec, value)) index
					| 'i' | 'I' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						let `mantissa float_prec, _ = float_repr in
						let value = round ~prec:float_prec value in
						wrap (`imaginary_literal (`float, value)) index
					| _ ->
						let `mantissa float_prec, _ = float_repr in
						let value = round ~prec:float_prec value in
						wrap (`float_literal (`float, value)) index
					end
				| 'i' | 'I' as h -> (* imaginary literal is gcc's extended?? *)
					Buffer.add_char buf h;
					let index = succ source index in
					let value = Real.scale mantissa ~base:e_base ~exponent in
					begin match get source index with
					| 'f' | 'F' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						let `mantissa float_prec, _ = float_repr in
						let value = round ~prec:float_prec value in
						wrap (`imaginary_literal (`float, value)) index
					| 'l' | 'L' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						wrap (`imaginary_literal (`long_double, value)) index
					| _ ->
						let `mantissa double_prec, _ = double_repr in
						let value = round ~prec:double_prec value in
						wrap (`imaginary_literal (`double, value)) index
					end
				| 'l' | 'L' as h ->
					Buffer.add_char buf h;
					let index = succ source index in
					let value = Real.scale mantissa ~base:e_base ~exponent in
					begin match get source index with
					| 'i' | 'I' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						wrap (`imaginary_literal (`long_double, value)) index
					| _ ->
						wrap (`float_literal (`long_double, value)) index
					end
				| _ ->
					let `mantissa double_prec, _ = double_repr in
					let value = Real.scale mantissa ~base:e_base ~exponent in
					let value = round ~prec:double_prec value in
					wrap (`float_literal (`double, value)) index
				end
			| _ as h -> (* integer *)
				let value = integer_of_based ~base start (i_end - start) in
				begin match h with
				| 'U' | 'u' as h ->
					Buffer.add_char buf h;
					let index = succ source index in
					begin match get source index with
					| 'L' | 'l' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						begin match get source index with
						| 'L' | 'l' as h ->
							Buffer.add_char buf h;
							let index = succ source index in
							wrap (`int_literal (`unsigned_long_long, value)) index
						| _ ->
							wrap (`int_literal (`unsigned_long, value)) index
						end
					| _ ->
						wrap (`int_literal (`unsigned_int, value)) index
					end
				| 'L' | 'l' as h ->
					Buffer.add_char buf h;
					let index = succ source index in
					begin match get source index with
					| 'L' | 'l' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						begin match get source index with
						| 'U' | 'u' as h ->
							Buffer.add_char buf h;
							let index = succ source index in
							wrap (`int_literal (`unsigned_long_long, value)) index
						| _ ->
							wrap (`int_literal (`signed_long_long, value)) index
						end
					| 'U' | 'u' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						wrap (`int_literal (`unsigned_long, value)) index
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
