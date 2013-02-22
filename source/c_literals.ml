open Value;;

(* language *)

type language = [`c | `cxx | `objc | `objcxx];;

let cxx (lang: language): bool = (
	begin match lang with
	| `c | `objc -> false
	| `cxx | `objcxx -> true
	end
);;

let objc (lang: language): bool = (
	begin match lang with
	| `c | `cxx -> false
	| `objc | `objcxx -> true
	end
);;

(* precision *)

type signed_int_prec = [
	| `signed_char
	| `signed_short
	| `signed_int
	| `signed_long
	| `signed_long_long];;

type unsigned_int_prec = [
	| `unsigned_char
	| `unsigned_short
	| `unsigned_int
	| `unsigned_long
	| `unsigned_long_long];;

type int_prec = [signed_int_prec | unsigned_int_prec];;

type float_prec = [
	| `float
	| `double
	| `long_double];;

type real_prec = [
	| float_prec
	| `decimal32 (* gcc's _Decimal32 *)
	| `decimal64 (* gcc's _Decimal64 *)
	| `decimal128];; (* gcc's _Decimal128 *)

(* for __attribute__((__mode__)) *)

type bit_width_mode = [
	| `__QI__ (* 8 *)
	| `__HI__ (* 16 *)
	| `__SI__ (* 32 *)
	| `__DI__ (* 64 *)
	| `__pointer__
	| `__unwind_word__ (* pointer size ? *)
	| `__word__];;

(* operators in iso646.h *)

type iso646_operator = [
	| `ampersand
	| `and_assign
	| `and_then
	| `caret
	| `exclamation
	| `ne
	| `or_assign
	| `or_else
	| `tilde
	| `vertical
	| `xor_assign];;

(* representation *)

module type LiteralsType = sig
	module Integer: IntegerType;;
	module Real: RealType;;
	module WideString: StringType with type elm = Int32.t;;
	val integer_of_real: Real.t -> Integer.t;;
	val real_of_integer: Integer.t -> Real.t;;
	val round_to_float: Real.t -> Real.t;;
	val round_to_double: Real.t -> Real.t;;
end;;
