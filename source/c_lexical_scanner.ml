open C_lexical;;
open C_literals;;

module type NumericScannerType = sig
	module Literals: LiteralsType;;
	module LexicalElement: LexicalElementType
		with module Literals := Literals;;
	
	val scan_numeric_literal:
		(('p * 'p) -> string -> unit) ->
		Buffer.t ->
		('s -> 'i -> 'p) ->
		('s -> 'i -> 'p) ->
		('s -> 'i -> char) ->
		('s -> 'i -> 'i) ->
		('s) ->
		'i ->
		[> `numeric_literal of string * LexicalElement.numeric_literal] * 'i;;
	
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
	
	let not_10_based_float_literal =
		"floating-point literal should be 10-based.";;
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
		let wrap (literal: LexicalElement.numeric_literal) (index: 'i) = (
			`numeric_literal ((Buffer.contents buf), literal), index
		) in
		begin match get source index with
		| '0'..'9' ->
			let p1 = position source index in
			let base, index = read_base_prefix buf index in
			let start = Buffer.length buf in
			let index = read_digits_to_buffer ~base buf index in
			begin match get source index with
			| '.' | 'e' | 'E' | 'p' | 'P' as h -> (* real *)
				let index =
					if h = '.' then (
						Buffer.add_char buf h;
						let index = succ source index in
						(* reading only 10-based digits becaue 'E' means exponent *)
						read_digits_to_buffer ~base:10 buf index
					) else (
						index
					)
				in
				let mantissa =
					let image = Buffer.sub buf start (Buffer.length buf - start) in
					Real.of_based_string ~base image
				in
				if base <> 10
					&& Real.compare mantissa Real.one <> 0 (* accept 0x1.0p2047 *)
				then (
					let p2 = prev_position source index in
					error (p1, p2) not_10_based_float_literal
				);
				let value, index =
					begin match get source index with
					| 'e' | 'E' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						let exponent, index = read_exponent buf index in
						let value = Real.scale mantissa ~base:10 ~exponent in
						value, index
					| 'p' | 'P' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						let exponent, index = read_exponent buf index in
						let value = Real.scale mantissa ~base:2 ~exponent in
						value, index
					| _ ->
						mantissa, index
					end
				in
				begin match get source index with
				| 'd' | 'D' as h ->
					Buffer.add_char buf h;
					let index = succ source index in
					begin match get source index with
					| 'f' | 'F' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						wrap (`float_literal (`decimal32, value)) index
					| 'd' | 'D' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						wrap (`float_literal (`decimal64, value)) index
					| 'l' | 'L' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						wrap (`float_literal (`decimal128, value)) index
					| _ ->
						let p2 = prev_position source index in
						error (p1, p2) bad_decimal_suffix;
						wrap (`float_literal (`decimal32, value)) index
					end
				| 'f' | 'F' as h ->
					Buffer.add_char buf h;
					let index = succ source index in
					let value = round_to_float value in
					begin match get source index with
					| 'i' | 'I' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						wrap (`imaginary_literal (`float, value)) index
					| _ ->
						wrap (`float_literal (`float, value)) index
					end
				| 'i' | 'I' as h -> (* imaginary literal is gcc's extended?? *)
					Buffer.add_char buf h;
					let index = succ source index in
					begin match get source index with
					| 'f' | 'F' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						let value = round_to_float value in
						wrap (`imaginary_literal (`float, value)) index
					| 'l' | 'L' as h ->
						Buffer.add_char buf h;
						let index = succ source index in
						wrap (`imaginary_literal (`long_double, value)) index
					| _ ->
						let value = round_to_double value in
						wrap (`imaginary_literal (`double, value)) index
					end
				| 'l' | 'L' as h ->
					Buffer.add_char buf h;
					let index = succ source index in
					begin match get source index with
					| 'i' | 'I' as h ->
						Buffer.add_char buf h;
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
					let length = Buffer.length buf - start in
					if length = 0 then Integer.zero else
					let image = Buffer.sub buf start length in
					Integer.of_based_string ~base image
				in
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
