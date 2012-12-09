module type NumericType = sig
	type t;;
	val zero: t;;
	val one: t;;
	val of_int: int -> t;;
	val of_based_string: base: int -> string -> t;;
	val to_based_string: base: int -> t -> string;;
	val compare: t -> t -> int;;
	val neg: t -> t;;
	val add: t -> t -> t;;
	val sub: t -> t -> t;;
	val mul: t -> t -> t;;
	val div: t -> t -> t;;
	val scale: t -> base:int -> exponent:int -> t;;
end;;

module type IntegerType = sig
	include NumericType;;
	val to_int: t -> int;;
	val rem: t -> t -> t;;
	val logand: t -> t -> t;;
	val logor: t -> t -> t;;
	val logxor: t -> t -> t;;
	val lognot: t -> t;;
	val shift_left: t -> int -> t;;
	val shift_right: t -> int -> t;;
end;;

module type RealType = sig
	include NumericType;;
	val frexp: t -> t * int;;
end;;

module type StringType = sig
	type elm;;
	type t;;
	val length: t -> int;;
	val empty: t;;
	val of_array: elm array -> t;;
	val get: t -> int -> elm;;
end;;
