open C_literals;;
open C_semantics;;
open C_semantics_build_type;;
open C_version;;

open struct
	let rec list_make n e =
		if n = 0 then [] else
		e :: list_make (n - 1) e
end;;

module type ExpressingType = sig
	module Literals: LiteralsType
	module Semantics: SemanticsType
		with module Literals := Literals
	module Language: LanguageType
	module Typing: TypingType
		with module Literals := Literals
		with module Semantics := Semantics
		with module Language := Language
	
	(* typing for unary operators *)
	
	val real_type_of:
		Semantics.all_type ->
		Semantics.predefined_types ->
		Semantics.all_type option
	
	(* typing for binary operators *)
	
	val int_prec: int_prec -> int_prec -> int_prec
	val float_prec: float_prec -> float_prec -> float_prec
	val extended_float_prec: extended_float_prec -> extended_float_prec -> extended_float_prec
	
	val result_type_of:
		[`add | `sub | `multiplicative | `bit | `conditional] ->
		Semantics.all_type ->
		Semantics.all_type ->
		Semantics.predefined_types ->
		Semantics.derived_types ->
		Semantics.all_type option * Semantics.derived_types
	
	(* build new expressions *)
	
	val implicit_conv:
		Semantics.all_type ->
		Semantics.expression ->
		Semantics.expression
	
	val int_conv:
		Semantics.predefined_types ->
		int_prec ->
		Literals.Integer.t ->
		Semantics.expression
	
	val float_conv:
		extended_float_prec ->
		Literals.Real.t ->
		Semantics.expression
	
	val zero:
		Semantics.all_type ->
		Semantics.expression option
	
end;;

module Expressing
	(Literals: LiteralsType)
	(Semantics: SemanticsType
		with module Literals := Literals)
	(Language: LanguageType)
	(Typing: TypingType
		with module Literals := Literals
		with module Semantics := Semantics
		with module Language := Language)
	: ExpressingType
		with module Literals := Literals
		with module Semantics := Semantics
		with module Typing := Typing
		with module Language := Language =
struct
	open Literals;;
	open Semantics;;
	
	(* typing for unary operators *)
	
	(* for __real__, __imag__ *)
	let rec real_type_of (t: all_type) (predefined_types: predefined_types): all_type option = (
		begin match t with
		| #int_prec | #extended_float_prec | #extended_decimal_prec | `char | `bool
			as t ->
			Some t
		| `imaginary prec | `complex prec ->
			Some (find_predefined_type prec predefined_types)
		| `volatile t ->
			real_type_of (t :> all_type) predefined_types
		| `const t ->
			real_type_of (t :> all_type) predefined_types
		| _ ->
			None
		end
	);;
	
	(* typing for binary operators *)
	
	let int_prec (prec1: int_prec) (prec2: int_prec): int_prec = (
		begin match prec1, prec2 with
		| `unsigned_long_long, _ | _, `unsigned_long_long -> `unsigned_long_long
		| `signed_long_long, _ | _, `signed_long_long -> `signed_long_long
		| `unsigned_long, _ | _, `unsigned_long -> `unsigned_long
		| `signed_long, _ | _, `signed_long -> `signed_long
		| `unsigned_int, _ | _, `unsigned_int -> `unsigned_int
		| _ -> `signed_int
		end
	);;
	
	let float_prec (prec1: float_prec) (prec2: float_prec): [> float_prec] = (
		begin match prec1, prec2 with
		| `long_double, _ | _, `long_double -> `long_double
		| `double, _ | _, `double -> `double
		| `float, `float -> `float
		end
	);;
	
	let extended_float_prec (prec1: extended_float_prec) (prec2: extended_float_prec): [> extended_float_prec] = (
		begin match prec1, prec2 with
		| `_Float128, _ | _, `_Float128 -> `_Float128
		| `_Float64x, _ | _, `_Float64x -> `_Float64x (* fNx is higher than fN *)
		| `_Float64, _ | _, `_Float64 -> `_Float64
		| `_Float32x, _ | _, `_Float32x -> `_Float32x
		| `_Float32, _ | _, `_Float32 -> `_Float32
		| (#float_prec as prec1), (#float_prec as prec2) -> float_prec prec1 prec2
		end
	);;
	
	let rec result_type_of
		(op: [`add | `sub | `multiplicative | `bit | `conditional])
		(t1: all_type)
		(t2: all_type)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		: all_type option * derived_types =
	(
		begin match resolve_typedef t1, resolve_typedef t2 with
		| `bool, `bool ->
			Some t1, derived_types
		| (#int_prec as int_t), `bool
		| `bool, (#int_prec as int_t) ->
			Some int_t, derived_types
		| (#int_prec as prec1), (#int_prec as prec2) ->
			begin match op with
			| `bit | `conditional when prec1 = prec2 ->
				Some prec1, derived_types
			| _ ->
				Some (find_predefined_type (int_prec prec1 prec2) predefined_types), derived_types
			end
		| (#extended_float_prec | #extended_decimal_prec as real_t), #int_prec
		| #int_prec, (#extended_float_prec | #extended_decimal_prec as real_t) ->
			Some real_t, derived_types
		| (#extended_float_prec as prec1), (#extended_float_prec as prec2) ->
			Some (find_predefined_type (extended_float_prec prec1 prec2) predefined_types),
				derived_types
		| (`imaginary prec1), (`imaginary prec2) when op <> `multiplicative ->
			Some (find_predefined_type (`imaginary (extended_float_prec prec1 prec2)) predefined_types), derived_types
		| (#float_prec as prec1), (`imaginary prec2 | `complex prec2)
		| (`imaginary prec1 | `complex prec1), (#float_prec as prec2)
		| (`imaginary prec1 | `complex prec1), (`imaginary prec2 | `complex prec2) ->
			Some (find_predefined_type (`complex (extended_float_prec prec1 prec2)) predefined_types), derived_types
		| (`pointer _ as ptr_t), #int_prec
		| (`restrict (`pointer _) as ptr_t), #int_prec
			when op = `add || op = `sub ->
			Some ptr_t, derived_types
		| #int_prec, (`pointer _ as ptr_t)
		| #int_prec, (`restrict (`pointer _) as ptr_t)
			when op = `add ->
			Some ptr_t, derived_types
		| `array (_, elm_t), #int_prec when op = `add || op = `sub ->
			let ptr_t, derived_types = Typing.find_pointer_type (elm_t :> all_type) derived_types in
			Some ptr_t, derived_types
		| #int_prec, `array (_, elm_t) when op = `add ->
			let ptr_t, derived_types = Typing.find_pointer_type (elm_t :> all_type) derived_types in
			Some ptr_t, derived_types
		| `pointer _, `pointer _ when op = `sub ->
			Some (find_ptrdiff_t predefined_types), derived_types
		| `pointer ptr_t1, `pointer ptr_t2 when ptr_t1 == ptr_t2 && op = `conditional ->
			Some ptr_t1, derived_types
		| `volatile t1, _ ->
			result_type_of op (t1 :> all_type) t2 predefined_types derived_types
		| _, `volatile t2 ->
			result_type_of op t1 (t2 :> all_type) predefined_types derived_types
		| `const t1, _ ->
			result_type_of op (t1 :> all_type) t2 predefined_types derived_types
		| _, `const t2 ->
			result_type_of op t1 (t2 :> all_type) predefined_types derived_types
		| (`void as void_t), _
		| _, (`void as void_t)
			when op = `conditional ->
			Some void_t, derived_types
		| (`named (_, _, `generic_type, _) as g_t), _
		| _, (`named (_, _, `generic_type, _) as g_t) ->
			Some g_t, derived_types
		| _ ->
			None, derived_types
		end
	);;
	
	(* build new expressions *)
	
	let implicit_conv (t: all_type) (expr: expression): expression = (
		begin match resolve_typedef ~stop_on_language_typedef:true t with
		| #predefined_numeric_type as t1 ->
			begin match resolve_typedef ~stop_on_language_typedef:true (snd expr) with
			| #predefined_numeric_type as t2 when t1 != t2 ->
				`implicit_conv expr, t
			| `named (_, _, `typedef _, _) ->
				`implicit_conv expr, t (* language typedef -> predefined type *)
			| `pointer _ ->
				`implicit_conv expr, t (* pointer as bool *)
			| _ ->
				expr
			end
		| `named (_, _, `typedef _, _) as t1 ->
			if resolve_typedef ~stop_on_language_typedef:true (snd expr) != t1 then (
				`implicit_conv expr, t (* predefined type -> language typedef *)
			) else (
				expr
			)
		| `pointer (`void as t1) | `pointer (`const `void as t1) ->
			begin match expr with
			| _, `pointer t2 when t2 != t1 ->
				`implicit_conv expr, t (* any type * -> void * *)
			| _ ->
				expr
			end
		| `pointer x ->
			begin match expr with
			| `chars_literal _, `array (_, `char)
				when (
					match x with
					| `const `char -> true
					| _ -> false) ->
				`implicit_conv expr, t (* char array literal -> char const * *)
			| `cast (`int_literal (_, n), _ as z), t2
			| `cast (`implicit_conv (`int_literal (_, n), _ as z), #int_prec), t2 (* (any type * )(void * )(implicit ptrdiff_t)0 *)
			| `explicit_conv (`int_literal (_, n), _ as z), t2
			| `implicit_conv (`int_literal (_, n), _ as z), t2
				when (
						match resolve_typedef t2 with
						| `pointer `void -> true
						| _ -> false)
					&& Integer.compare n Integer.zero = 0 ->
				`implicit_conv z, t (* (void * )NULL -> (any type * )NULL *)
			| `int_literal (_, n), _
			| `implicit_conv (`int_literal (_, n), _), #int_prec
				when Integer.compare n Integer.zero = 0 ->
				`implicit_conv expr, t (* 0 -> (any type * )NULL *)
			| _, `pointer `void
			| _, `pointer (`const `void) ->
				`implicit_conv expr, t (* void * -> any type * *)
			| _ ->
				expr
			end
		| _ ->
			expr
		end
	);;
	
	let int_conv
		(predefined_types: predefined_types)
		(t: int_prec)
		(x: Integer.t): expression =
	(
		if Integer.compare x Integer.zero >= 0 && Integer.compare_int x 128 < 0 then (
			`int_literal (t, x), (t :> all_type) (* all int types can have 0..127 *)
		) else (
			let bit_size = Typing.sizeof_predefined_type t predefined_types * 8 in
			let bits = Integer.sub (Integer.shift_left Integer.one bit_size) Integer.one in
			begin match t with
			| `signed_char | `signed_short | `signed_int
			| `signed_long | `signed_long_long | `__int128_t ->
				let x2 =
					if Integer.test_bit x (bit_size - 1) = 0 then (
						Integer.logand x bits
					) else (
						Integer.logor x (Integer.lognot bits)
					)
				in
				`int_literal (t, x2), (t :> all_type)
			| `unsigned_char | `unsigned_short | `unsigned_int
			| `unsigned_long | `unsigned_long_long | `__uint128_t ->
				let x2 = Integer.logand x bits in
				`int_literal (t, x2), (t :> all_type)
			end
		)
	);;
	
	let float_conv (t: extended_float_prec) (x: Real.t): expression = (
		let `mantissa mantissa, `emin emin =
			match t with
			| `float -> float_repr
			| `double -> double_repr
			| `long_double -> long_double_repr
			| `_Float32 -> `mantissa 24, `emin ~-125
			| `_Float64 | `_Float32x -> `mantissa 53, `emin ~-1021
			| `_Float128 | `_Float64x -> `mantissa 113, `emin ~-16381
		in
		let x =
			let emin2 = emin - mantissa in
			let m, e = Real.frexp x in
			if e < emin2 then (
				Real.zero (* underflow *)
			) else if e = emin2 then (
				let threshold = Real.scale ~base:2 ~exponent:~-1 Real.one in
				if Real.compare m threshold <= 0 then (
					Real.zero (* underflow, just 0.5 is to even *)
				) else (
					Real.scale ~base:2 ~exponent:e Real.one (* round up *)
				)
			) else (
				round ~prec:mantissa x
			)
		in
		`float_literal (t, x), (t :> all_type)
	);;
	
	let rec zero
		(t: all_type)
		: expression option =
	(
		begin match t with
		| #int_prec as prec ->
			Some (`int_literal (prec, Integer.zero), t)
		| #extended_float_prec as prec ->
			Some (`float_literal (prec, Real.zero), t)
		| `imaginary prec ->
			Some (`imaginary_literal (prec, Real.zero), t)
		| #extended_decimal_prec ->
			None (* unsupported *)
		| `char ->
			Some (`char_literal '\x00', t)
		| `wchar ->
			Some (`wchar_literal (WideChar.of_int 0), t)
		| `bool | `complex _ | `anonymous (_, `enum _) | `named (_, _, `enum _, _)
		| `pointer (_: all_type) | `__builtin_va_list | `block_pointer _ ->
			Some (`implicit_conv (`int_literal (`signed_int, Integer.zero), `signed_int), t)
		| `array (size, element_t) ->
			begin match size with
			| Some size ->
				if Integer.compare size Integer.zero <= 0 then (
					None
				) else (
					begin match zero (element_t :> all_type) with
					| Some zero ->
						let es = list_make (Integer.to_int size) zero in
						Some (`compound (es, None), t)
					| None ->
						None
					end
				)
			| None ->
				None
			end
		| `restrict (t: pointer_type) ->
			zero (t :> all_type)
		| `anonymous (_, `struct_type (_, items)) | `named (_, _, `struct_type (_, items), _) ->
			let rec loop items es = (
				begin match items with
				| (_, element_t, _, _) :: items_r ->
					begin match zero element_t with
					| Some zero ->
						loop items_r (zero :: es)
					| None ->
						[]
					end
				| [] ->
					List.rev es
				end
			) in
			begin match loop items [] with
			| [] ->
				None
			| es ->
				Some (`compound (es, None), t)
			end
		| `anonymous (_, `union items) | `named (_, _, `union items, _) ->
			begin match items with
			| (_, element_t, _, _) :: _ ->
				begin match zero element_t with
				| Some zero ->
					Some (`compound (zero :: [], None), t)
				| None ->
					None
				end
			| [] ->
				None
			end
		| `named (_, _, `typedef t, _) ->
			zero t
		| `volatile t ->
			zero (t :> all_type)
		| `const t ->
			zero (t :> all_type)
		| `void | `function_type _ | `named (_, _, `generic_type, _) ->
			None
		| `named (_, _, `opaque_enum, _)
		| `named (_, _, `opaque_struct, _)
		| `named (_, _, `opaque_union, _) ->
			None (* ??? *)
		end
	);;
	
end;;
