open Value;;

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

type int_prec = [signed_int_prec | unsigned_int_prec
	| `__int128_t
	| `__uint128_t];;

type float_prec = [
	| `float
	| `double
	| `long_double];;

type extended_float_proc = [ (* ISO/IEC TS 18661-3:2015 *)
	| `_Float32
	| `_Float64
	| `_Float128
	| `_Float32x
	| `_Float64x];;

type extended_decimal_proc = [ (* ISO/IEC WDTR24732 *)
	| `_Decimal32
	| `_Decimal64
	| `_Decimal128];;

type real_prec = [float_prec | extended_float_proc | extended_decimal_proc];;

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
	module Integer: IntegerType
	module Real: RealType
	module WideString: StringType with type elm = Int32.t
	val integer_of_real: Real.t -> Integer.t
	val real_of_integer: Integer.t -> Real.t
	val round: prec:int -> Real.t -> Real.t
	val float_prec: int
	val double_prec: int
end;;
