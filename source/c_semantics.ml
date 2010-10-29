open Value;;

type position = (string * int * int * int);;
type ranged_position = position * position;;

type language = [`c | `cxx | `objc | `objcxx];;

(* types *)

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

let unsigned_of_signed (p: signed_int_prec): [> unsigned_int_prec] = (
	match p with
	| `signed_char -> `unsigned_char
	| `signed_short -> `unsigned_short
	| `signed_int -> `unsigned_int
	| `signed_long -> `unsigned_long
	| `signed_long_long -> `unsigned_long_long
);;

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

type predefined_type = [
	| `void
	| `bool
	| int_prec
	| real_prec
	| `imaginary of float_prec
	| `complex of float_prec
	| `char
	| `wchar (* only C++ or Objective-C++ *)
	| `__builtin_va_list];;

(* operators in iso646.h *)

type operator = [
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

(* for __attribute__((__mode__)) *)

type bit_width_mode = [
	| `__QI__ (* 8 *)
	| `__HI__ (* 16 *)
	| `__SI__ (* 32 *)
	| `__DI__ (* 64 *)
	| `__pointer__
	| `__unwind_word__ (* pointer size ? *)
	| `__word__];;

(* storage class *)

type storage_class = [
	| `typedef
	| `extern
	| `static
	| `auto
	| `register];;

(* type qualifier *)

type type_qualifier = [
	| `const
	| `restrict
	| `volatile];;

(* (6.5.16) assignment-operator *)

type assignment_operator = [
	| `assign
	| `mul_assign
	| `div_assign
	| `rem_assign
	| `add_assign
	| `sub_assign
	| `l_shift_assign
	| `r_shift_assign
	| `and_assign
	| `xor_assign
	| `or_assign];;

(* calling conventions *)

type calling_convention = [`cdecl | `stdcall | `fastcall];;
type varargs_opt = [`varargs | `none];;

(* source info *)

type extra_info = {
	ei_fenv: bool; (* #pragma GCC fenv *)
	ei_system_header: bool};; (* #pragma GCC system_header *)

let no_extra_info = {
	ei_fenv = false;
	ei_system_header = false};;

module StringMap = struct
	include Map.Make (String);;
	
	let find_or ~(default: 'a) (key: string) (m: 'a t): 'a = (
		try find key m with Not_found -> default
	);;
	
	let modify (f: 'a -> 'a) ~(default: 'a) (key: string) (m: 'a t): 'a t = (
		add key (f (find_or ~default key m)) m
	);;
	
end;;

module Semantics (Literals: LiteralsType) = struct
	open Literals;;
	
	(* attributes *)
	
	type alignment = [`default | `explicit_aligned | `aligned of int | `packed];;
	
	type attributes = {
		at_aligned: alignment;
		at_const: bool;
		at_conventions: calling_convention;
		at_deprecated: bool;
		at_dllimport: bool;
		at_dllexport: bool;
		at_format: [`none | `like of string * int * int | `arg of int];
		at_inline: [`none | `noinline | `inline | `always_inline];
		at_malloc: bool;
		at_mode: bit_width_mode option;
		at_noreturn: bool;
		at_nothrow: bool;
		at_pure: bool;
		at_sentinel: bool;
		at_selectany: bool;
		at_unavailable: bool;
		at_used: [`none | `used | `unused];
		at_weak_import: bool};;
	
	let no_attributes = {
		at_aligned = `default;
		at_const = false;
		at_conventions = `cdecl;
		at_deprecated = false;
		at_dllimport = false;
		at_dllexport = false;
		at_format = `none;
		at_inline = `none;
		at_malloc = false;
		at_mode = None;
		at_noreturn = false;
		at_nothrow = false;
		at_pure = false;
		at_sentinel = false;
		at_selectany = false;
		at_unavailable = false;
		at_used = `none;
		at_weak_import = false};;
	
	(* items *)
	
	type literal_value = [
		| `int_literal of int_prec * Integer.t
		| `float_literal of real_prec * Real.t
		| `imaginary_literal of float_prec * Real.t
		| `char_literal of char
		| `chars_literal of string
		| `wchar_literal of WideString.elm
		| `wchars_literal of WideString.t
		| `objc_string_literal of string];;
	
	type enum_element = [`enum_element of Integer.t];;
	
	type 'a with_name = [`named of ranged_position * string * 'a * attributes];;
	type 'a anonymous = [`anonymous of ranged_position * 'a];;
	
	type 'a item = 'a * item_info
	and item_info = {it_depending: named_item list}
	and pointer_type = [`pointer of type_item]
	and enum_item = enum_element with_name item
	and struct_item = string * type_item * int option * attributes
	and anonymous_type_var = [
		| `enum of enum_item list
		| `struct_type of alignment * struct_item list
		| `union of struct_item list]
	and named_type_var = [
		| `opaque_enum
		| `enum of enum_item list
		| `opaque_struct
		| `struct_type of alignment * struct_item list
		| `opaque_union
		| `union of struct_item list
		| `typedef of type_item
		| `generic_type]
	and prototype = calling_convention * variable item list * varargs_opt * type_item
	and function_type = [`function_type of prototype]
	and not_qualified_type = [ (* predefined_type | anonymous_type | named_type | `pointer | `array | `restrict *)
		| `void
		| `bool
		| int_prec
		| real_prec
		| `imaginary of float_prec
		| `complex of float_prec
		| `char
		| `wchar (* only C++ or Objective-C++ *)
		| `__builtin_va_list
		| `pointer of type_item
		| `array of Integer.t option * not_qualified_type item
		| `restrict of pointer_type item
		| `anonymous of ranged_position * anonymous_type_var
		| `function_type of prototype (* also, anonymous *)
		| `named of ranged_position * string * named_type_var * attributes]
	and not_const_type = [ (* not_qualified_type | `volatile *)
		| `void
		| `bool
		| int_prec
		| real_prec
		| `imaginary of float_prec
		| `complex of float_prec
		| `char
		| `wchar (* only C++ or Objective-C++ *)
		| `__builtin_va_list
		| `pointer of type_item
		| `array of Integer.t option * not_qualified_type item
		| `restrict of pointer_type item
		| `volatile of not_qualified_type item (* added to not_qualified_type *)
		| `anonymous of ranged_position * anonymous_type_var
		| `function_type of prototype (* also, anonymous *)
		| `named of ranged_position * string * named_type_var * attributes]
	and all_type = [ (* not_const_type | `const *)
		| `void
		| `bool
		| int_prec
		| real_prec
		| `imaginary of float_prec
		| `complex of float_prec
		| `char
		| `wchar (* only C++ or Objective-C++ *)
		| `__builtin_va_list
		| `pointer of type_item
		| `array of Integer.t option * not_qualified_type item
		| `restrict of pointer_type item
		| `volatile of not_qualified_type item
		| `const of not_const_type item (* added to not_const_type *)
		| `anonymous of ranged_position * anonymous_type_var
		| `function_type of prototype (* also, anonymous *)
		| `named of ranged_position * string * named_type_var * attributes]
	and struct_or_union_type_var = [
		| `struct_type of alignment * struct_item list
		| `union of struct_item list]
	and struct_or_union_type = [
		| `anonymous of ranged_position * struct_or_union_type_var
		| `named of ranged_position * string * struct_or_union_type_var * attributes]
	and type_item = all_type item
	and named_var = [ (* named_type | more... *)
		| `enum_element of Integer.t
		| `opaque_enum
		| `enum of enum_item list
		| `opaque_struct
		| `struct_type of alignment * struct_item list
		| `opaque_union
		| `union of struct_item list
		| `typedef of type_item
		| `extern of type_item * [`alias of string | `none]
		| `variable of type_item * expression option
		| `function_forward of [`static | `builtin] * function_type item
		| `function_definition of [`static | `extern_inline | `none] * function_type item * statement list
		| `defined_operator of operator
		| `defined_specifiers of [storage_class | `none] (* and attributes *)
		| `defined_type_qualifier of type_qualifier
		| `defined_typedef of type_item
		| `defined_element_access of struct_or_union_type item * struct_item list
		| `defined_expression of expression
		| `defined_generic_expression of [`generic_type] with_name item list * [`generic_value of type_item] with_name item list * varargs_opt * expression
		| `defined_generic_statement of [`generic_type] with_name item list * [`generic_value of type_item] with_name item list * varargs_opt * statement
		| `defined_any of string
		| `defined_alias of named_item (* only used for #pragma for LANG FILE1 include FILE2 *)
		| `generic_type (* macro argument as type *)
		| `generic_value of type_item] (* macro argument as value *)
	and named = named_var with_name
	and named_item = named item
	and source_item_var = [ (* anonymous_type | named *)
		| `anonymous of ranged_position * anonymous_type_var
		| `function_type of prototype (* also, anonymous *)
		| `named of ranged_position * string * named_var * attributes]
	and source_item = source_item_var item
	and variable_var = [`variable of type_item * expression option]
	and variable = variable_var with_name
	and object_var = [
		| `extern of type_item * [`alias of string | `none]
		| `variable of type_item * expression option
		| `generic_value of type_item]
	and function_var = [
		| `extern of function_type item * [`alias of string | `none]
		| `function_forward of [`static | `builtin] * function_type item
		| `function_definition of [`static | `extern_inline | `none] * function_type item * statement list]
	and expression_var = [
		| literal_value
		| `enumerator of enum_item
		| `ref_object of object_var with_name item * [`lvalue | `rvalue]
		| `ref_function of function_var with_name item
		| `__FILE__
		| `__LINE__
		| `__func__
		| `statement of statement list
		| `function_call of expression * expression list
		| `va_arg of expression
		| `element_access of expression * struct_item
		| `dereference of expression
		| `post_increment of expression
		| `post_decrement of expression
		| `compound of expression list
		| `increment of expression
		| `decrement of expression
		| `address of expression
		| `neg of expression
		| `bit_not of expression
		| `not of expression
		| `sizeof_formal_type of string
		| `real of expression
		| `imag of expression
		| `cast of expression
		| `mul of expression * expression
		| `div of expression * expression
		| `rem of expression * expression
		| `add of expression * expression
		| `sub of expression * expression
		| `l_shift of expression * expression
		| `r_shift of expression * expression
		| `lt of expression * expression
		| `gt of expression * expression
		| `le of expression * expression
		| `ge of expression * expression
		| `eq of expression * expression
		| `ne of expression * expression
		| `bit_and of expression * expression
		| `bit_xor of expression * expression
		| `bit_or of expression * expression
		| `and_then of expression * expression
		| `or_else of expression * expression
		| `cond of expression * expression * expression
		| `assign of expression * assignment_operator * expression
		| `comma of expression * expression]
	and expression = expression_var * type_item
	and statement = [
		| `asm of [`volatile | `none] * string * (string * expression) list * (string * expression) list * string list
		| `local of source_item list * statement list
		| `compound of statement list
		| `expression of expression
		| `va_start of expression * expression
		| `va_end of expression
		| `va_copy of expression * expression
		| `label of string * statement list
		| `if_statement of expression * statement list * statement list
		| `while_loop of expression * statement list
		| `do_loop of statement list * expression
		| `for_loop of expression option * expression option * expression option * statement list
		| `goto of string
		| `break of [`loop | `switch]
		| `return of expression option * type_item];;
	
	type array_type = [`array of Integer.t option * not_qualified_type item];;
	type restrict_type = [`restrict of pointer_type item];;
	type volatile_type = [`volatile of not_qualified_type item];;
	type const_type = [`const of not_const_type item];;
	
	type struct_or_union_type_item = struct_or_union_type item;;
	type not_qualified_type_item = not_qualified_type item;;
	
	type derived_type = [
		| `pointer of type_item
		| `array of Integer.t option * not_qualified_type item
		| `restrict of pointer_type item
		| `volatile of not_qualified_type item
		| `const of not_const_type item];;
	
	(* full = anonymous | named
	 * opaquable = opaque | named *)
	
	type enum_type_var = [`enum of enum_item list];;
	type named_enum_item = enum_type_var with_name item;;
	type full_enum_type = [enum_type_var anonymous | enum_type_var with_name];;
	type full_enum_item = full_enum_type item;;
	type opaque_enum_type = [`opaque_enum] with_name;;
	type opaque_enum_item = opaque_enum_type item;;
	type opaquable_enum_var = [`opaque_enum | enum_type_var];;
	
	type struct_type_var = [`struct_type of alignment * struct_item list];;
	type named_struct_item = struct_type_var with_name item;;
	type opaque_struct_type = [`opaque_struct] with_name;;
	type opaque_struct_item = opaque_struct_type item;;
	type opaquable_struct_var = [`opaque_struct | struct_type_var];;
	
	type union_type_var = [`union of struct_item list];;
	type named_union_item = union_type_var with_name item;;
	type opaque_union_type = [`opaque_union] with_name;;
	type opaque_union_item = opaque_union_type item;;
	type opaquable_union_var = [`opaque_union | union_type_var];;
	
	type opaque_type_var = [`opaque_enum | `opaque_struct | `opaque_union];;
	
	type anonymous_type = [anonymous_type_var anonymous | function_type];;
	
	type typedef_var = [`typedef of type_item];;
	type typedef_type = typedef_var with_name;;
	type typedef_item = typedef_type item;;
	
	type named_type = named_type_var with_name;;
	type named_type_item = named_type item;;
	
	type function_definition = [`function_definition of [`static | `extern_inline | `none] * function_type item * statement list];;
	type function_definition_item = function_definition with_name item;;
	type function_item = function_var with_name item;;
	
	type predefined_item = predefined_type item;;
	type derived_item = derived_type item;;
	
	type anonymous_item = anonymous_type item;;
	
	type all = [predefined_type | derived_type | anonymous_type | named]
	and all_item = all item;;
	
	type conditional_expression_var = [`cond of expression * expression * expression];;
	type conditional_expression = conditional_expression_var * type_item;;
	
	type assignment_expression_var = [
		| `assign of expression * assignment_operator * expression];;
	type assignment_expression = assignment_expression_var * type_item;;
	type any_assignment_expression_var = [
		| `post_increment of expression
		| `post_decrement of expression
		| `increment of expression
		| `decrement of expression
		| `assign of expression * assignment_operator * expression];;
	type any_assignment_expression = any_assignment_expression_var * type_item;;
	
	type statement_expression_var = [`statement of statement list];;
	type statement_expression = statement_expression_var * type_item;;
	
	(* type check *)
	
	ignore (lazy ((assert false : named_type) :> named));;
	ignore (lazy ((assert false : named_type) :> not_qualified_type));;
	ignore (lazy ((assert false : predefined_type) :> not_qualified_type));;
	ignore (lazy ((assert false : anonymous_type) :> not_qualified_type));;
	ignore (lazy ((assert false : anonymous_type) :> source_item_var));;
	ignore (lazy ((assert false : not_qualified_type) :> not_const_type));;
	ignore (lazy ((assert false : not_const_type) :> all_type));;
	ignore (lazy ((assert false : derived_type) :> all_type));;
	ignore (lazy ((assert false : object_var) :> named_var));;
	ignore (lazy ((assert false : function_var) :> named_var));;
	ignore (lazy ((assert false : conditional_expression) :> expression));;
	ignore (lazy ((assert false : assignment_expression) :> any_assignment_expression));;
	ignore (lazy ((assert false : any_assignment_expression) :> expression));;
	ignore (lazy ((assert false : statement_expression) :> expression));;
	
	(* typedef *)
	
	let rec resolve_typedef (t: type_item): type_item = (
		begin match t with
		| `named (_, _, `typedef t, _), _ ->
			resolve_typedef t
		| _ ->
			t
		end
	);;
	
	(* predefined types *)
	
	type predefined_types = (predefined_item * int) list * typedef_item list;;
	
	let find_predefined_type (e: [< predefined_type]) (predefined_types: predefined_types): [> predefined_type] item = (
		begin match fst (List.find (fun ((x, _), _) -> x = (e :> predefined_type)) (fst predefined_types)) with
		| #predefined_type, _ as result ->
			result
		end
	);;
	
	let sizeof_predefined_type (e: [< predefined_type]) (predefined_types: predefined_types): int = (
		snd (List.find (fun ((x, _), _) -> x = (e :> predefined_type)) (fst predefined_types))
	);;
	
	let find_ptrdiff_t (predefined_types: predefined_types): [> [> typedef_var] with_name] item = (
		begin match List.find (fun (`named (_, name, _, _), _) -> name = "ptrdiff_t") (snd predefined_types) with
		| `named (_, _, `typedef _, _), _ as result ->
			result
		end
	);;
	
	let find_size_t (predefined_types: predefined_types): [> [> typedef_var] with_name] item = (
		begin match List.find (fun (`named (_, name, _, _), _) -> name = "size_t") (snd predefined_types) with
		| `named (_, _, `typedef _, _), _ as result ->
			result
		end
	);;
	
	let find_wchar_t (predefined_types: predefined_types): [> predefined_type | [> typedef_var] with_name] item = (
		begin try
			begin match List.find (fun (`named (_, name, _, _), _) -> name = "wchar_t") (snd predefined_types) with
			| `named (_, _, `typedef _, _), _ as result ->
				result
			end
		with Not_found -> (* C++ or Objective-C++ *)
			find_predefined_type `wchar predefined_types
		end
	);;
	
	(* derived types *)
	
	let rec is_pointer (t: type_item): bool = (
		begin match resolve_typedef t with
		| `pointer _, _
		| `restrict (`pointer _, _), _ ->
			true
		| `volatile t, _ ->
			is_pointer (t :> type_item)
		| `const t, _ ->
			is_pointer (t :> type_item)
		| _ ->
			false
		end
	);;
	
	let rec is_derived_type (d: derived_item) (t: type_item): bool = (
		begin match d with
		| `pointer x, _
		| `restrict (`pointer x, _), _ ->
			if x == t then true else
			begin match x with
			| #derived_type, _ as x -> is_derived_type x t
			| _ -> false
			end
		| `array (_, x), _
		| `volatile x, _ ->
			if (x :> type_item) == t then true else
			begin match (x :> type_item) with
			| #derived_type, _ as x -> is_derived_type x t
			| _ -> false
			end
		| `const x, _ ->
			if (x :> type_item) == t then true else
			begin match (x :> type_item) with
			| #derived_type, _ as x -> is_derived_type x t
			| _ -> false
			end
		end
	);;
	
	(* opaque types *)
	
	type opaque_types = opaque_enum_item StringMap.t *
		opaque_struct_item StringMap.t *
		opaque_union_item StringMap.t;;
	
	let empty_opaque_types = StringMap.empty, StringMap.empty, StringMap.empty;;
	
	let is_opaque (item: opaque_type_var with_name item)
		(opaque_types: opaque_types)
		: bool =
	(
		let `named (_, name, kind, _), _ = item in
		let oe, os, ou = opaque_types in
		begin match kind with
		| `opaque_enum -> StringMap.mem name oe
		| `opaque_struct -> StringMap.mem name os
		| `opaque_union -> StringMap.mem name ou
		end
	);;
	
	(* struct / union *)
	
	let rec find_field (name: string) (xs: struct_item list): struct_item option = (
		begin match xs with
		| (field_name, field_t, _, _ as field) :: xr ->
			if field_name = name then Some field else (
				if field_name = "" then (
					begin match field_t with
					| `anonymous (_, `struct_type (_, items)), _
					| `anonymous (_, `union items), _
					| `named (_, _, `struct_type (_, items), _), _
					| `named (_, _, `union items, _), _ ->
						begin match find_field name items with
						| Some _ as result ->
							result
						| None ->
							find_field name xr
						end
					| _ ->
						find_field name xr
					end
				) else (
					find_field name xr
				)
			)
		| [] ->
			None
		end
	);;
	
	(* generic types *)
	
	let rec is_generic_type (t: type_item): bool = (
		begin match resolve_typedef t with
		| `named (_, _, `generic_type, _), _ ->
			true
		| `pointer t, _
		| `restrict (`pointer t, _), _ ->
			is_generic_type t
		| `array (_, t), _
		| `volatile t, _ ->
			is_generic_type (t :> type_item)
		| `const t, _ ->
			is_generic_type (t :> type_item)
		| _ ->
			false
		end
	);;
	
	(* expression / statement *)
	
	let rec is_static_expression (expr: expression): bool = (
		begin match fst expr with
		| #literal_value | `enumerator _ ->
			true
		| `ref_object _ ->
			false
		| `ref_function _ ->
			true
		| `__FILE__ | `__LINE__ | `__func__
		| `statement _ | `function_call _ | `va_arg _ ->
			false
		| `element_access (a, _) | `dereference a ->
			is_static_expression a
		| `post_increment _ | `post_decrement _ | `compound _
		| `increment _ | `decrement _ ->
			false
		| `address a ->
			begin match a with
			| `ref_object _, _ -> true
			| _ -> is_static_expression a
			end
		| `neg a | `bit_not a | `not a ->
			is_static_expression a
		| `sizeof_formal_type _ ->
			false (* ??? *)
		| `real a | `imag a | `cast a ->
			is_static_expression a
		| `mul (a, b) | `div (a, b) | `rem (a, b)
		| `add (a, b) | `sub (a, b)
		| `l_shift (a, b) | `r_shift (a, b)
		| `lt (a, b) | `gt (a, b) | `le (a, b) | `ge (a, b)
		| `eq (a, b) | `ne (a, b)
		| `bit_and (a, b) | `bit_xor (a, b) | `bit_or (a, b)
		| `and_then (a, b) | `or_else (a, b) ->
			is_static_expression a && is_static_expression b
		| `cond (a, b, c) ->
			is_static_expression a && is_static_expression b && is_static_expression c
		| `assign _ ->
			false
		| `comma (a, b) ->
			is_static_expression a && is_static_expression b
		end
	);;
	
	let integer_of_expression (x: expression): (int_prec * Integer.t) option = (
		begin match fst x with
		| `int_literal pn ->
			Some pn
		| `char_literal c ->
			Some (`signed_int, Integer.of_int (int_of_char c))
		| `enumerator (`named (_, _, `enum_element n, _), _) ->
			Some (`signed_int, n)
		| _ ->
			None
		end
	);;
	
	let rec exists_in_expression
		(stmt_f: statement -> bool)
		(expr_f: expression -> bool)
		(expr: expression)
		: bool =
	(
		if expr_f expr then true else
		begin match fst expr with
		| #literal_value | `enumerator _
		| `ref_object _ | `ref_function _
		| `__FILE__ | `__LINE__ | `__func__ ->
			false
		| `statement stmts ->
			List.exists (exists_in_statement stmt_f expr_f) stmts
		| `function_call (f, args) ->
			exists_in_expression stmt_f expr_f f ||
			List.exists (exists_in_expression stmt_f expr_f) args
		| `va_arg expr
		| `element_access (expr, _) | `dereference expr
		| `post_increment expr | `post_decrement expr
		| `increment expr | `decrement expr
		| `address expr
		| `neg expr | `bit_not expr | `not expr
		| `real expr | `imag expr | `cast expr ->
			exists_in_expression stmt_f expr_f expr
		| `sizeof_formal_type _ ->
			false
		| `compound exprs ->
			List.exists (exists_in_expression stmt_f expr_f) exprs
		| `mul (expr1, expr2) | `div (expr1, expr2) | `rem (expr1, expr2)
		| `add (expr1, expr2) | `sub (expr1, expr2)
		| `l_shift (expr1, expr2) | `r_shift (expr1, expr2)
		| `lt (expr1, expr2) | `gt (expr1, expr2)
		| `le (expr1, expr2) | `ge (expr1, expr2)
		| `eq (expr1, expr2) | `ne (expr1, expr2)
		| `bit_and (expr1, expr2) | `bit_xor (expr1, expr2) | `bit_or (expr1, expr2)
		| `and_then (expr1, expr2) | `or_else (expr1, expr2)
		| `assign (expr1, _, expr2) | `comma (expr1, expr2) ->
			exists_in_expression stmt_f expr_f expr1 ||
			exists_in_expression stmt_f expr_f expr2
		| `cond (expr1, expr2, expr3) ->
			exists_in_expression stmt_f expr_f expr1 ||
			exists_in_expression stmt_f expr_f expr2 ||
			exists_in_expression stmt_f expr_f expr3
		end
	) and exists_in_statement
		(stmt_f: statement -> bool)
		(expr_f: expression -> bool)
		(stmt: statement)
		: bool =
	(
		if stmt_f stmt then true else
		begin match stmt with
		| `asm (_, _, out_args, in_args, _) ->
			List.exists (fun (_, expr) -> exists_in_expression stmt_f expr_f expr) out_args ||
			List.exists (fun (_, expr) -> exists_in_expression stmt_f expr_f expr) in_args
		| `local (_, stmts) | `compound stmts | `label (_, stmts) ->
			List.exists (exists_in_statement stmt_f expr_f) stmts
		| `while_loop (expr, stmts) | `do_loop (stmts, expr) ->
			exists_in_expression stmt_f expr_f expr ||
			List.exists (exists_in_statement stmt_f expr_f) stmts
		| `for_loop (expr1, expr2, expr3, stmts) ->
			(match expr1 with Some expr1 -> exists_in_expression stmt_f expr_f expr1 | None -> false) ||
			(match expr2 with Some expr2 -> exists_in_expression stmt_f expr_f expr2 | None -> false) ||
			(match expr3 with Some expr3 -> exists_in_expression stmt_f expr_f expr3 | None -> false) ||
			List.exists (exists_in_statement stmt_f expr_f) stmts
		| `if_statement (expr, stmts1, stmts2) ->
			exists_in_expression stmt_f expr_f expr ||
			List.exists (exists_in_statement stmt_f expr_f) stmts1 ||
			List.exists (exists_in_statement stmt_f expr_f) stmts2
		| `va_start (expr1, expr2) | `va_copy (expr1, expr2) ->
			exists_in_expression stmt_f expr_f expr1 ||
			exists_in_expression stmt_f expr_f expr2
		| `va_end expr | `expression expr ->
			exists_in_expression stmt_f expr_f expr
		| `goto _ | `break _ ->
			false
		| `return (expr, _) ->
			(match expr with Some expr -> exists_in_expression stmt_f expr_f expr | None -> false)
		end
	);;
	
	let fold_expression
		(stmt_f: 'a -> statement -> 'a)
		(expr_f: 'a -> expression -> 'a)
		(a: 'a)
		(expr: expression)
		: 'a =
	(
		let result = ref a in
		let stmt_f2 stmt = (
			result := stmt_f !result stmt;
			false
		) in
		let expr_f2 expr = (
			result := expr_f !result expr;
			false
		) in
		ignore (exists_in_expression stmt_f2 expr_f2 expr);
		!result
	);;
	
	let fold_statement
		(stmt_f: 'a -> statement -> 'a)
		(expr_f: 'a -> expression -> 'a)
		(a: 'a)
		(stmt: statement)
		: 'a =
	(
		let result = ref a in
		let stmt_f2 stmt = (
			result := stmt_f !result stmt;
			false
		) in
		let expr_f2 expr = (
			result := expr_f !result expr;
			false
		) in
		ignore (exists_in_statement stmt_f2 expr_f2 stmt);
		!result
	);;
	
	let has_asm: statement -> bool =
		let stmt_f stmt = (
			begin match stmt with
			| `asm _ -> true
			| _ -> false
			end
		) in
		exists_in_statement stmt_f (fun _ -> false);;
	
	let lvalue_referenced (variable: variable item): statement -> bool =
		let expr_f expr = (
			begin match expr with
			| `ref_object (v, `lvalue), _ when v == (variable :> object_var with_name item) -> true
			| _ -> false
			end
		) in
		exists_in_statement (fun _ -> false) expr_f;;
	
	(* language mapping *)
	
	type language_mapping = {
		lm_type: (type_item * string) list;
		lm_overload: (function_item * prototype list) list;
		lm_include: (string * string) list};;
	
	let no_language_mapping = {
		lm_type = [];
		lm_overload = [];
		lm_include = []};;
	
end;;

module SemanticsType (Literals: LiteralsType) = struct
	module type S = module type of Semantics (Literals);;
end;;
