open C_filename;;
open C_literals;;
open Position;;

module StringMap = struct
	include Map.Make (String);;
	
	let find_or ~(default: 'a) (key: string) (m: 'a t): 'a = (
		try find key m with Not_found -> default
	);;
	
	let modify (f: 'a -> 'a) ~(default: 'a) (key: string) (m: 'a t): 'a t = (
		add key (f (find_or ~default key m)) m
	);;
	
end;;

module SemanticsThin (Literals: LiteralsType) = struct
	open Literals;;
	
	(* calling conventions *)
	
	type calling_convention = [`cdecl | `stdcall | `fastcall | `thiscall];;
	type varargs_opt = [`varargs | `none];;
	
	(* attributes *)
	
	type alignment = [`default | `explicit_aligned | `aligned of int | `packed];;
	
	type attributes = {
		at_aligned: alignment;
		at_alloc_size: int list;
		at_artificial: bool;
		at_blocks: [`none | `byref];
		at_const: bool;
		at_conventions: calling_convention;
		at_deprecated: bool;
		at_dllimport: bool;
		at_dllexport: bool;
		at_format: [`none | `like of string * int * int | `arg of int];
		at_inline: [`none | `noinline | `inline | `always_inline];
		at_leaf: bool;
		at_malloc: bool;
		at_mode: bit_width_mode option;
		at_nonnull: int list;
		at_noreturn: bool;
		at_nothrow: bool;
		at_objc_gc: [`none | `weak];
		at_optimize: string option;
		at_pure: bool;
		at_regparm: int option;
		at_returns_twice: bool;
		at_sentinel: bool;
		at_selectany: bool;
		at_transparent_union: bool;
		at_unavailable: bool;
		at_used: [`none | `used | `unused];
		at_warn_unused_result : bool;
		at_weak_link: [`none | `weak | `weak_import]};;
	
	(* items *)
	
	type predefined_numeric_type = [
		| `bool
		| int_prec
		| real_prec
		| `imaginary of float_prec
		| `complex of float_prec
		| `char
		| `wchar];; (* only C++ or Objective-C++ *)
	
	type predefined_type = [predefined_numeric_type
		| `void
		| `__builtin_va_list];;
	
	type storage_class = [
		| `typedef
		| `extern
		| `static
		| `auto
		| `register];;
	
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
	
	type pointer_type = [`pointer of all_type]
	and opaque_type_var = [`opaque_enum | `opaque_struct | `opaque_union]
	and enum_item = enum_element with_name
	and struct_item = string * all_type * (int * int * bool) option * attributes (* position, width, explicit *)
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
		| `typedef of all_type
		| `generic_type]
	and prototype = calling_convention * variable list * varargs_opt * all_type
	and function_type = [`function_type of prototype]
	and not_qualified_type = [ (* predefined_type | anonymous_type | named_type | `pointer | `block_pointer | `array | `restrict *)
		| predefined_type
		| `pointer of all_type
		| `block_pointer of function_type
		| `array of Integer.t option * not_qualified_type
		| `restrict of pointer_type
		| `anonymous of ranged_position * anonymous_type_var
		| `function_type of prototype (* also, anonymous *)
		| `named of ranged_position * string * named_type_var * attributes]
	and not_const_type = [ (* not_qualified_type | `volatile *)
		| predefined_type
		| `pointer of all_type
		| `block_pointer of function_type
		| `array of Integer.t option * not_qualified_type
		| `restrict of pointer_type
		| `volatile of not_qualified_type (* added to not_qualified_type *)
		| `anonymous of ranged_position * anonymous_type_var
		| `function_type of prototype (* also, anonymous *)
		| `named of ranged_position * string * named_type_var * attributes]
	and all_type = [ (* not_const_type | `const *)
		| predefined_type
		| `pointer of all_type
		| `block_pointer of function_type
		| `array of Integer.t option * not_qualified_type
		| `restrict of pointer_type
		| `volatile of not_qualified_type
		| `const of not_const_type (* added to not_const_type *)
		| `anonymous of ranged_position * anonymous_type_var
		| `function_type of prototype (* also, anonymous *)
		| `named of ranged_position * string * named_type_var * attributes]
	and struct_or_union_type_var = [
		| `struct_type of alignment * struct_item list
		| `union of struct_item list]
	and struct_or_union_type = [
		| `anonymous of ranged_position * struct_or_union_type_var
		| `named of ranged_position * string * struct_or_union_type_var * attributes]
	and named_var = [ (* named_type | more... *)
		| `enum_element of Integer.t
		| `opaque_enum
		| `enum of enum_item list
		| `opaque_struct
		| `struct_type of alignment * struct_item list
		| `opaque_union
		| `union of struct_item list
		| `typedef of all_type
		| `extern of all_type * [`alias of string | `none]
		| `variable of all_type * expression option
		| `function_forward of [`static | `builtin] * function_type
		| `function_definition of [`static | `extern_inline | `none] * function_type * statement list
		| `defined_operator of iso646_operator
		| `defined_attributes
		| `defined_storage_class of storage_class
		| `defined_type_specifier of [`imaginary | `complex]
		| `defined_type_qualifier of [`const | `restrict | `volatile]
		| `defined_typedef of all_type
		| `defined_opaque_type of opaque_type_var with_name (* tag *)
		| `defined_element_access of struct_or_union_type * struct_item list
		| `defined_expression of expression
		| `defined_generic_expression of [`generic_type] with_name list * [`generic_value of all_type] with_name list * varargs_opt * expression
		| `defined_generic_statement of [`generic_type] with_name list * [`generic_value of all_type] with_name list * varargs_opt * statement
		| `defined_any of string
		| `defined_alias of named_item (* only used for #pragma for LANG FILE1 include FILE2 *)
		| `generic_type (* macro argument as type *)
		| `generic_value of all_type] (* macro argument as value *)
	and named_item = named_var with_name
	and anonymous_alias = [`anonymous_alias of ranged_position * function_type] (* fst points source *)
	and include_point = [`include_point of string]
	and source_item = [ (* anonymous_type | named *)
		| `anonymous of ranged_position * anonymous_type_var
		| `function_type of prototype (* also, anonymous *)
		| `anonymous_alias of ranged_position * function_type
		| `named of ranged_position * string * named_var * attributes
		| `include_point of string]
	and variable_var = [`variable of all_type * expression option]
	and variable = variable_var with_name
	and object_var = [
		| `extern of all_type * [`alias of string | `none]
		| `variable of all_type * expression option
		| `generic_value of all_type]
	and function_var = [
		| `extern of function_type * [`alias of string | `none]
		| `function_forward of [`static | `builtin] * function_type
		| `function_definition of [`static | `extern_inline | `none] * function_type * statement list]
	and expression_var = [
		| literal_value
		| `enumerator of enum_item
		| `ref_object of object_var with_name * [`lvalue | `rvalue]
		| `ref_function of function_var with_name
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
		| `compound of expression list * expression option
		| `increment of expression
		| `decrement of expression
		| `address of expression
		| `neg of expression
		| `bit_not of expression
		| `not of expression
		| `sizeof_formal_type of string
		| `real of expression
		| `imag of expression
		| `cast of expression (* keeping binary-image like (char * )void_ptr *)
		| `explicit_conv of expression (* like (float)int_var *)
		| `implicit_conv of expression (* like int to float *)
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
		| `uo of expression * expression (* unordered *)
		| `bit_and of expression * expression
		| `bit_xor of expression * expression
		| `bit_or of expression * expression
		| `and_then of expression * expression
		| `or_else of expression * expression
		| `cond of expression * expression * expression
		| `assign of expression * assignment_operator * expression
		| `comma of expression * expression]
	and assignment_operator = [
		(* (6.5.16) assignment-operator *)
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
		| `or_assign]
	and expression = expression_var * all_type
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
		| `return of expression option * all_type];;
	
	type array_type = [`array of Integer.t option * not_qualified_type];;
	type restrict_type = [`restrict of pointer_type];;
	type volatile_type = [`volatile of not_qualified_type];;
	type const_type = [`const of not_const_type];;
	
	type derived_type = [
		| `pointer of all_type
		| `block_pointer of function_type
		| `array of Integer.t option * not_qualified_type
		| `restrict of pointer_type
		| `volatile of not_qualified_type
		| `const of not_const_type];;
	
	(* full = anonymous | named
	 * opaquable = opaque | named *)
	
	type enum_type_var = [`enum of enum_item list];;
	type named_enum_type = enum_type_var with_name;;
	type full_enum_type = [enum_type_var anonymous | enum_type_var with_name];;
	type opaque_enum_type = [`opaque_enum] with_name;;
	type opaquable_enum_var = [`opaque_enum | enum_type_var];;
	
	type struct_type_var = [`struct_type of alignment * struct_item list];;
	type named_struct_type = struct_type_var with_name;;
	type opaque_struct_type = [`opaque_struct] with_name;;
	type opaquable_struct_var = [`opaque_struct | struct_type_var];;
	
	type union_type_var = [`union of struct_item list];;
	type named_union_type = union_type_var with_name;;
	type opaque_union_type = [`opaque_union] with_name;;
	type opaquable_union_var = [`opaque_union | union_type_var];;
	
	type opaque_type = opaque_type_var with_name;;
	type non_opaque_type_var = [enum_type_var | struct_type_var | union_type_var];;
	type non_opaque_type = non_opaque_type_var with_name;;
	
	type anonymous_type = [anonymous_type_var anonymous | function_type];;
	
	type typedef_var = [`typedef of all_type];;
	type typedef_type = typedef_var with_name;;
	
	type named_type = named_type_var with_name;;
	
	type function_definition = [`function_definition of [`static | `extern_inline | `none] * function_type * statement list];;
	type function_definition_item = function_definition with_name;;
	type function_item = function_var with_name;;
	
	type all_item = [predefined_type | derived_type | anonymous_type | named_item | anonymous_alias | include_point]
	
	type conditional_expression_var = [`cond of expression * expression * expression];;
	type conditional_expression = conditional_expression_var * all_type;;
	
	type assignment_expression_var = [
		| `assign of expression * assignment_operator * expression];;
	type assignment_expression = assignment_expression_var * all_type;;
	type any_assignment_expression_var = [
		| `post_increment of expression
		| `post_decrement of expression
		| `increment of expression
		| `decrement of expression
		| `assign of expression * assignment_operator * expression];;
	type any_assignment_expression = any_assignment_expression_var * all_type;;
	
	type statement_expression_var = [`statement of statement list];;
	type statement_expression = statement_expression_var * all_type;;
	
	(* type check *)
	
	ignore (lazy ((assert false : named_type) :> named_item));;
	ignore (lazy ((assert false : named_type) :> not_qualified_type));;
	ignore (lazy ((assert false : predefined_type) :> not_qualified_type));;
	ignore (lazy ((assert false : anonymous_type) :> not_qualified_type));;
	ignore (lazy ((assert false : anonymous_type) :> source_item));;
	ignore (lazy ((assert false : not_qualified_type) :> not_const_type));;
	ignore (lazy ((assert false : not_const_type) :> all_type));;
	ignore (lazy ((assert false : derived_type) :> all_type));;
	ignore (lazy ((assert false : object_var) :> named_var));;
	ignore (lazy ((assert false : function_var) :> named_var));;
	ignore (lazy ((assert false : conditional_expression) :> expression));;
	ignore (lazy ((assert false : assignment_expression) :> any_assignment_expression));;
	ignore (lazy ((assert false : any_assignment_expression) :> expression));;
	ignore (lazy ((assert false : statement_expression) :> expression));;
	
	(* list *)
	
	type predefined_types = (predefined_type * int) list * typedef_type list;;
	type derived_types = derived_type list;;
	
	(* namespace *)
	
	type namespace = {
		ns_namespace: named_item StringMap.t;
		ns_enum_of_element: full_enum_type StringMap.t;
		ns_opaque_enum: opaque_enum_type StringMap.t;
		ns_enum: named_enum_type StringMap.t;
		ns_opaque_struct: opaque_struct_type StringMap.t;
		ns_struct: named_struct_type StringMap.t;
		ns_opaque_union: opaque_union_type StringMap.t;
		ns_union: named_union_type StringMap.t};;
	
	(* mapping from opaque type to body *)
	
	type opaque_mapping =
		(opaque_enum_type * named_enum_type) StringMap.t *
		(opaque_struct_type * named_struct_type) StringMap.t *
		(opaque_union_type * named_union_type) StringMap.t;;
	
	(* source info *)
	
	type extra_info = {
		ei_fenv: bool; (* #pragma GCC fenv *)
		ei_system_header: bool};; (* #pragma GCC system_header *)
	
	(* mapping options *)
	
	type language_mapping = {
		lm_type: (all_type * string) list;
		lm_overload: (function_item * prototype list) list;
		lm_include: (string * string) list;
		lm_monolithic_include: (string * string) list};;
	
	type mapping_options = {
		mo_instances: (all_type list) StringMap.t;
		mo_language_mappings: language_mapping StringMap.t};;
	
end;;

module type SemanticsThinType = sig
	module Literals: LiteralsType;;
	include module type of SemanticsThin (Literals);;
end;;

module type SemanticsType = sig
	module Literals: LiteralsType;;
	open Literals;;
	include SemanticsThinType
		with module Literals := Literals;;
	
	(* attributes *)
	
	val no_attributes: attributes;;
	
	(* typedef *)
	
	val resolve_typedef:
		?stop_on_language_typedef: bool ->
		?stop_on_anonymous: bool ->
		all_type ->
		all_type;;
	
	val remove_type_qualifiers: all_type -> not_qualified_type;;
	
	(* predefined types *)
	
	val find_predefined_type_with_size: [< predefined_type] -> predefined_types -> [> predefined_type] * int;;
	val find_predefined_type: [< predefined_type] -> predefined_types -> [> predefined_type];;
	val find_ptrdiff_t: predefined_types -> [> [> typedef_var] with_name];;
	val find_size_t: predefined_types -> [> [> typedef_var] with_name];;
	val find_wchar_t: predefined_types -> [> predefined_type | [> typedef_var] with_name];;
	
	val unsigned_of_signed: signed_int_prec -> [> unsigned_int_prec];;
	
	(* derived types *)
	
	val is_derived_type: (all_type -> bool) -> derived_type -> bool;;
	
	val is_pointer: all_type -> bool;;
	val dereference: all_type -> all_type option;;
	
	val fold_derived_types: ('a -> derived_type -> 'a) ->
		'a -> all_type -> derived_types -> 'a;;
	
	(* namespace *)
	
	val empty_namespace: namespace;;
	
	(* mapping from opaque type to body *)
	
	val empty_opaque_mapping: opaque_mapping;;
	
	val opaque_to_full: opaque_type -> opaque_mapping -> non_opaque_type option;;
	val full_to_opaque: non_opaque_type -> opaque_mapping -> opaque_type;;
	
	val is_opaque: opaque_type -> opaque_mapping -> bool;;
	
	(* struct / union *)
	
	val find_field: string -> struct_item list -> struct_item option;;
	
	val is_bitfield: struct_item list -> bool;;
	
	val tail_type_of_element_access: struct_item list -> all_type;;
	
	(* generic types *)
	
	val is_generic_type: all_type -> bool;;
	
	(* expression / statement *)
	
	val is_static_expression: expression -> bool;;
	
	val integer_of_expression: expression -> (int_prec * Integer.t) option;;
	
	val exists_in_expression: (statement -> bool) -> (expression -> bool) ->
		expression -> bool;;
	val exists_in_statement: (statement -> bool) -> (expression -> bool) ->
		statement -> bool;;
	
	val fold_expression: ('a -> statement -> 'a) -> ('a -> expression -> 'a) ->
		'a -> expression -> 'a;;
	val fold_statement: ('a -> statement -> 'a) -> ('a -> expression -> 'a) ->
		'a -> statement -> 'a;;
	
	(* source info *)
	
	val no_extra_info: extra_info;;
	val empty_source: source_item list * extra_info;;
	
	(* mapping options *)
	
	val no_language_mapping: language_mapping;;
	val no_mapping_options: mapping_options;;
	
	val find_langauge_mapping: string -> mapping_options -> language_mapping;;
	
	val find_mapped_type: all_type -> language_mapping -> string;;
	val find_mapped_type_of_unconstrained_array: not_qualified_type -> language_mapping -> string;;
	
	val mem_mapped_type: all_type -> language_mapping -> bool;;
	
end;;

module Semantics
	(Literals: LiteralsType)
	: SemanticsType
		with module Literals := Literals =
struct
	open Literals;;
	include SemanticsThin (Literals);;
	
	(* attributes *)
	
	let no_attributes = {
		at_aligned = `default;
		at_alloc_size = [];
		at_artificial = false;
		at_blocks = `none;
		at_const = false;
		at_conventions = `cdecl;
		at_deprecated = false;
		at_dllimport = false;
		at_dllexport = false;
		at_format = `none;
		at_inline = `none;
		at_leaf = false;
		at_malloc = false;
		at_mode = None;
		at_nonnull = [];
		at_noreturn = false;
		at_nothrow = false;
		at_objc_gc = `none;
		at_optimize = None;
		at_pure = false;
		at_regparm = None;
		at_returns_twice = false;
		at_sentinel = false;
		at_selectany = false;
		at_transparent_union = false;
		at_unavailable = false;
		at_used = `none;
		at_warn_unused_result = false;
		at_weak_link = `none};;
	
	(* typedef *)
	
	let rec resolve_typedef
		?(stop_on_language_typedef: bool = false)
		?(stop_on_anonymous: bool = false)
		(t: all_type)
		: all_type =
	(
		begin match t with
		| `named (ps, _, `typedef t, _)
			when (
				not stop_on_language_typedef
				|| let (filename, _, _, _), _ = ps in not (is_special_filename filename))
				&& (
				not stop_on_anonymous
				||
					match t with
					| `anonymous _ | `function_type _
					| `pointer (`anonymous _ | `function_type _) -> false
					| _ -> true)
		->
			resolve_typedef ~stop_on_language_typedef t
		| _ ->
			t
		end
	);;
	
	let rec remove_type_qualifiers (t: all_type): not_qualified_type = (
		begin match t with
		| `const t ->
			remove_type_qualifiers (t :> all_type)
		| `volatile t ->
			remove_type_qualifiers (t :> all_type)
		| #not_qualified_type as t ->
			t
		end
	);;
	
	(* predefined types *)
	
	let find_predefined_type_with_size (e: [< predefined_type]) (predefined_types: predefined_types): [> predefined_type] * int = (
		begin match List.find (fun (x, _) -> x = (e :> predefined_type)) (fst predefined_types) with
		| (#predefined_type, _) as result ->
			result
		end
	);;
	
	let find_predefined_type (e: [< predefined_type]) (predefined_types: predefined_types): [> predefined_type] = (
		begin match List.find (fun (x, _) -> x = (e :> predefined_type)) (fst predefined_types) with
		| (#predefined_type as result), _ ->
			result
		end
	);;
	
	let find_ptrdiff_t (predefined_types: predefined_types): [> [> typedef_var] with_name] = (
		begin match List.find (fun (`named (_, name, _, _)) -> name = "ptrdiff_t") (snd predefined_types) with
		| `named (_, _, `typedef _, _) as result ->
			result
		end
	);;
	
	let find_size_t (predefined_types: predefined_types): [> [> typedef_var] with_name] = (
		begin match List.find (fun (`named (_, name, _, _)) -> name = "size_t") (snd predefined_types) with
		| `named (_, _, `typedef _, _) as result ->
			result
		end
	);;
	
	let find_wchar_t (predefined_types: predefined_types): [> predefined_type | [> typedef_var] with_name] = (
		begin try
			begin match List.find (fun (`named (_, name, _, _)) -> name = "wchar_t") (snd predefined_types) with
			| `named (_, _, `typedef _, _) as result ->
				result
			end
		with Not_found -> (* C++ or Objective-C++ *)
			find_predefined_type `wchar predefined_types
		end
	);;
	
	let unsigned_of_signed (p: signed_int_prec): [> unsigned_int_prec] = (
		match p with
		| `signed_char -> `unsigned_char
		| `signed_short -> `unsigned_short
		| `signed_int -> `unsigned_int
		| `signed_long -> `unsigned_long
		| `signed_long_long -> `unsigned_long_long
	);;
	
	(* derived types *)
	
	let rec is_derived_type (f: all_type -> bool) (d: derived_type): bool = (
		let rec handle (f: all_type -> bool) (x: all_type): bool = (
			if f x then true else
			begin match x with
			| #derived_type as x -> is_derived_type f x
			| `named (_, _, `typedef x, _) -> handle f x
			| _ -> false
			end
		) in
		begin match d with
		| `pointer x
		| `restrict (`pointer x) ->
			handle f x
		| `block_pointer x ->
			f (x :> all_type)
		| `array (_, x)
		| `volatile x ->
			handle f (x :> all_type)
		| `const x ->
			handle f (x :> all_type)
		end
	);;
	
	let rec is_pointer (t: all_type): bool = (
		begin match resolve_typedef t with
		| `pointer _
		| `restrict (`pointer _) ->
			true
		| `volatile t ->
			is_pointer (t :> all_type)
		| `const t ->
			is_pointer (t :> all_type)
		| _ ->
			false
		end
	);;
	
	let rec dereference (t: all_type): all_type option = (
		begin match resolve_typedef t with
		| `pointer t ->
			Some t
		| `block_pointer t ->
			Some (t :> all_type)
		| `restrict (`pointer t) ->
			Some t
		| `volatile t ->
			dereference (t :> all_type)
		| `const t ->
			dereference (t :> all_type)
		| `named (_, _, `generic_type, _) ->
			Some t
		| _ ->
			None
		end
	);;
	
	let fold_derived_types
		(f: 'a -> derived_type -> 'a)
		(a: 'a)
		(base_type: all_type)
		(derived_types: derived_types)
		: 'a =
	(
		List.fold_left (fun a dt ->
			begin match dt with
			| `pointer t ->
				if t == base_type then f a dt else a
			| `block_pointer t ->
				if (t :> all_type) == base_type then f a dt else a
			| `array (_, t) ->
				if (t :> all_type) == base_type then f a dt else a
			| `restrict t ->
				if (t :> all_type) == base_type then f a dt else a
			| `volatile t ->
				if (t :> all_type) == base_type then f a dt else a
			| `const t ->
				if (t :> all_type) == base_type then f a dt else a
			end
		) a derived_types
	);;
	
	(* namespace *)
	
	let empty_namespace = {
		ns_namespace = StringMap.empty;
		ns_enum_of_element = StringMap.empty;
		ns_opaque_enum = StringMap.empty;
		ns_enum = StringMap.empty;
		ns_opaque_struct = StringMap.empty;
		ns_struct = StringMap.empty;
		ns_opaque_union = StringMap.empty;
		ns_union = StringMap.empty};;
	
	(* mapping from opaque type to body *)
	
	let empty_opaque_mapping =
		StringMap.empty,
		StringMap.empty,
		StringMap.empty;;
	
	let opaque_to_full
		(item: opaque_type)
		(opaque_mapping: opaque_mapping)
		: non_opaque_type option =
	(
		try
			let `named (_, name, kind, _) = item in
			let oe, os, ou = opaque_mapping in
			Some (
				match kind with
				| `opaque_enum -> (snd (StringMap.find name oe) :> non_opaque_type)
				| `opaque_struct -> (snd (StringMap.find name os) :> non_opaque_type)
				| `opaque_union -> (snd (StringMap.find name ou) :> non_opaque_type))
		with Not_found -> None
	);;
	
	let full_to_opaque
		(item: non_opaque_type)
		(opaque_mapping: opaque_mapping)
		: opaque_type =
	(
		let `named (_, name, kind, _) = item in
		let oe, os, ou = opaque_mapping in
		begin match kind with
		| `enum _ -> (fst (StringMap.find name oe) :> opaque_type)
		| `struct_type _ -> (fst (StringMap.find name os) :> opaque_type)
		| `union _ -> (fst (StringMap.find name ou) :> opaque_type)
		end
	);;
	
	let is_opaque
		(item: opaque_type)
		(opaque_mapping: opaque_mapping)
		: bool =
	(
		opaque_to_full item opaque_mapping = None
	);;
	
	(* struct / union *)
	
	let rec find_field (name: string) (xs: struct_item list): struct_item option = (
		begin match xs with
		| (field_name, field_t, _, _ as field) :: xr ->
			if field_name = name then Some field else (
				if field_name = "" then (
					begin match field_t with
					| `anonymous (_, `struct_type (_, items))
					| `anonymous (_, `union items)
					| `named (_, _, `struct_type (_, items), _)
					| `named (_, _, `union items, _) ->
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
	
	let is_bitfield (xs: struct_item list): bool = (
		begin match xs with
		| (_, _, Some _, _) :: _ -> true
		| _ -> false
		end
	);;
	
	let tail_type_of_element_access (route: struct_item list): all_type = (
		let last_route = List.fold_left (fun _ r -> r) (List.hd route) route in
		let _, result_type, _, _ = last_route in
		result_type
	);;
	
	(* generic types *)
	
	let rec is_generic_type (t: all_type): bool = (
		begin match resolve_typedef t with
		| `named (_, _, `generic_type, _) ->
			true
		| `pointer t
		| `restrict (`pointer t) ->
			is_generic_type t
		| `array (_, t)
		| `volatile t ->
			is_generic_type (t :> all_type)
		| `const t ->
			is_generic_type (t :> all_type)
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
		| `post_increment _ | `post_decrement _ | `increment _ | `decrement _ ->
			false
		| `compound (xs, zero) ->
			List.for_all is_static_expression xs && (
				match zero with
				| Some zero -> is_static_expression zero
				| None -> true)
		| `address a ->
			begin match a with
			| `ref_object _, _ -> true
			| _ -> is_static_expression a
			end
		| `neg a | `bit_not a | `not a ->
			is_static_expression a
		| `sizeof_formal_type _ ->
			false (* ??? *)
		| `real a | `imag a | `cast a | `explicit_conv a | `implicit_conv a ->
			is_static_expression a
		| `mul (a, b) | `div (a, b) | `rem (a, b)
		| `add (a, b) | `sub (a, b)
		| `l_shift (a, b) | `r_shift (a, b)
		| `lt (a, b) | `gt (a, b) | `le (a, b) | `ge (a, b)
		| `eq (a, b) | `ne (a, b) | `uo (a, b)
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
	
	let rec integer_of_expression (x: expression): (int_prec * Integer.t) option = (
		begin match fst x with
		| `int_literal pn ->
			Some pn
		| `char_literal c ->
			Some (`signed_int, Integer.of_int (int_of_char c))
		| `enumerator (`named (_, _, `enum_element n, _)) ->
			Some (`signed_int, n)
		| `implicit_conv expr ->
			begin match resolve_typedef (snd expr) with
			| #int_prec as t ->
				begin match integer_of_expression expr with
				| Some (_, value) ->
					Some (t, value)
				| None ->
					None
				end
			| _ ->
				None
			end
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
		| `real expr | `imag expr
		| `cast expr | `explicit_conv expr | `implicit_conv expr ->
			exists_in_expression stmt_f expr_f expr
		| `sizeof_formal_type _ ->
			false
		| `compound (exprs, zero) ->
			List.exists (exists_in_expression stmt_f expr_f) exprs ||
			begin match zero with
			| Some zero -> exists_in_expression stmt_f expr_f zero
			| None -> false
			end
		| `mul (expr1, expr2) | `div (expr1, expr2) | `rem (expr1, expr2)
		| `add (expr1, expr2) | `sub (expr1, expr2)
		| `l_shift (expr1, expr2) | `r_shift (expr1, expr2)
		| `lt (expr1, expr2) | `gt (expr1, expr2)
		| `le (expr1, expr2) | `ge (expr1, expr2)
		| `eq (expr1, expr2) | `ne (expr1, expr2) | `uo (expr1, expr2)
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
	
	(* source info *)
	
	let no_extra_info = {
		ei_fenv = false;
		ei_system_header = false};;
	
	let empty_source = [], no_extra_info;;
	
	(* mapping options *)
	
	let no_language_mapping = {
		lm_type = [];
		lm_overload = [];
		lm_include = [];
		lm_monolithic_include = []};;
	
	let no_mapping_options = {
		mo_instances = StringMap.empty;
		mo_language_mappings = StringMap.empty};;
	
	let find_langauge_mapping (lang: string) (x: mapping_options): language_mapping = (
		try
			StringMap.find lang x.mo_language_mappings
		with Not_found -> no_language_mapping
	);;
	
	let find_mapped_type
		(t: all_type)
		(language_mapping: language_mapping)
		: string =
	(
		List.assq t language_mapping.lm_type
	);;
	
	let find_mapped_type_of_unconstrained_array
		(base_type: not_qualified_type)
		(language_mapping: language_mapping)
		: string =
	(
		snd (
			List.find (fun (t, _) ->
				begin match t with
				| `array (None, b) when b == base_type ->
					true
				| _ ->
					false
				end
			) language_mapping.lm_type
		)
	);;
	
	let mem_mapped_type
		(t: all_type)
		(language_mapping: language_mapping)
		: bool =
	(
		List.mem_assq t language_mapping.lm_type
	);;
	
end;;
