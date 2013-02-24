open C_filename;;
open C_literals;;
open C_semantics;;
open Position;;

let builtin_position: ranged_position =
	let p = builtin_name, 0, 0, 0 in
	p, p;;

type sizeof =
	[`sizeof_bool of int] *
	[`sizeof_short of int] *
	[`sizeof_int of int] *
	[`sizeof_long of int] *
	[`sizeof_long_long of int] *
	[`sizeof_float of int] *
	[`sizeof_double of int] *
	[`sizeof_long_double of int] *
	[`sizeof_intptr of int];;

type language_typedef =
	[`typedef_ptrdiff_t of signed_int_prec] *
	[`typedef_size_t of unsigned_int_prec] *
	[`typedef_wchar_t of int_prec];;

module type TypingType = sig
	module Literals: LiteralsType;;
	module Semantics: SemanticsType
		with module Literals := Literals;;
	
	(* type compatibility *)
	
	type compatibility = [`just | `compatible | `error];;
	
	val type_ABI_compatibility:
		dest:Semantics.all_type ->
		source:Semantics.all_type ->
		compatibility;;
	val prototype_ABI_compatibility:
		dest:Semantics.prototype ->
		source:Semantics.prototype ->
		compatibility;;
	
	(* predefined types *)
	
	val ready_predefined_types:
		language ->
		sizeof ->
		language_typedef ->
		Semantics.predefined_types;;
	
	val select_bit_width_int:
		Semantics.predefined_types ->
		int ->
		int_prec ->
		int_prec ->
		[> int_prec] * [> `none | `not_had of int];;
	
	val apply_bit_width_mode:
		Semantics.predefined_types ->
		bit_width_mode ->
		Semantics.all_type ->
		Semantics.all_type * [> `none | `not_had of int | `not_int];;
	
	(* derived types *)
	
	val find_const_type:
		Semantics.all_type ->
		Semantics.derived_types ->
		[> Semantics.const_type] * Semantics.derived_types;;
	val find_volatile_type:
		Semantics.all_type ->
		Semantics.derived_types ->
		[> Semantics.volatile_type | `const of [> Semantics.volatile_type]] * Semantics.derived_types;;
	val find_pointer_type:
		Semantics.all_type ->
		Semantics.derived_types ->
		[> Semantics.pointer_type] * Semantics.derived_types;;
	val find_block_pointer_type:
		Semantics.function_type ->
		Semantics.derived_types ->
		[> `block_pointer of Semantics.function_type] * Semantics.derived_types;;
	val find_array_type:
		Literals.Integer.t option ->
		Semantics.all_type ->
		Semantics.derived_types ->
		[> Semantics.array_type | `volatile of [> Semantics.array_type]
			| `const of [> Semantics.array_type | `volatile of [> Semantics.array_type]]] * Semantics.derived_types;;
	val find_restrict_type:
		Semantics.pointer_type ->
		Semantics.derived_types ->
		[> Semantics.restrict_type] * Semantics.derived_types;;
	
	(* anonymous types *)
	
	val find_function_type:
		Semantics.prototype ->
		Semantics.source_item list ->
		[> Semantics.function_type] * Semantics.source_item list;;
	
	(* named types *)
	
	val find_enum:
		(ranged_position * [`ident of string]) ->
		Semantics.namespace ->
		Semantics.source_item list ->
		[> [> Semantics.opaquable_enum_var] Semantics.with_name] * Semantics.namespace * Semantics.source_item list;;
	
	val find_struct:
		(ranged_position * [`ident of string]) ->
		Semantics.namespace ->
		Semantics.source_item list ->
		[> [> Semantics.opaquable_struct_var] Semantics.with_name] * Semantics.namespace * Semantics.source_item list;;
	
	val find_union:
		(ranged_position * [`ident of string]) ->
		Semantics.namespace ->
		Semantics.source_item list ->
		[> [> Semantics.opaquable_union_var] Semantics.with_name] * Semantics.namespace * Semantics.source_item list;;
	
	val find_enum_by_element:
		Semantics.enum_item ->
		Semantics.predefined_types ->
		Semantics.namespace ->
		Semantics.all_type;;
	
	val resolve_opaque:
		Semantics.namespace ->
		([> [> Semantics.opaque_type_var | Semantics.non_opaque_type_var] Semantics.with_name] as 'a) ->
		'a;;
	
	(* sizeof / alignof *)
	
	val sizeof_predefined_type:
		[< Semantics.predefined_type] ->
		Semantics.predefined_types ->
		int;;
	val sizeof:
		Semantics.all_type ->
		Semantics.predefined_types ->
		int option;;
	
	val alignof_predefined_type:
		[< Semantics.predefined_type] ->
		Semantics.predefined_types ->
		int;;
	val alignof:
		Semantics.all_type ->
		Semantics.predefined_types ->
		int option;;
	val alignof_struct:
		Semantics.struct_item list ->
		Semantics.predefined_types ->
		int option;;
	
	(* bit-field *)
	
	type bitfield_or_not = [`empty | `is_bitfield | `is_not_bitfield | `mixed];;
	
	val is_bitfield:
		Semantics.struct_item list ->
		bitfield_or_not;;
	val fill_bitfield:
		Semantics.predefined_types ->
		Semantics.struct_item list ->
		Semantics.struct_item list * [`error of Semantics.struct_item | `none];;
	
end;;

module Typing
	(Literals: LiteralsType)
	(Semantics: SemanticsType
		with module Literals := Literals)
	: TypingType
		with module Literals := Literals
		with module Semantics := Semantics =
struct
	open Literals;;
	open Semantics;;
	
	(* type compatibility *)
	
	type compatibility = [`just | `compatible | `error];;
	
	let min_compatibility (x: compatibility) (y: compatibility): compatibility = (
		begin match x, y with
		| `error, _ | _, `error -> `error
		| `compatible, _ | _, `compatible -> `compatible
		| `just, `just -> `just
		end
	);;
	
	let rec type_ABI_compatibility ~(dest: all_type) ~(source: all_type): compatibility = (
		if dest == source then `just else
		begin match dest, source with
		| _, `void ->
			`compatible
		| (`char | `signed_char | `unsigned_char), (`char | `signed_char | `unsigned_char) ->
			`compatible
		| `pointer dest, `pointer source ->
			type_ABI_compatibility ~dest ~source
		| `const dest, `const source ->
			type_ABI_compatibility ~dest:(dest :> all_type) ~source:(source :> all_type)
		| `const dest, _ ->
			let r2 = type_ABI_compatibility ~dest:(dest :> all_type) ~source in
			min_compatibility `compatible r2
		| `named (_, _, `typedef dest, _), _ ->
			type_ABI_compatibility ~dest ~source
		| _, `named (_, _, `typedef source, _) ->
			type_ABI_compatibility ~dest ~source
		| _ ->
			`error
		end
	) and prototype_ABI_compatibility ~(dest: prototype) ~(source: prototype): compatibility = (
		let d_conv, d_args, d_varargs, d_result = dest in
		let s_conv, s_args, s_varargs, s_result = source in
		(* calling convention *)
		if d_conv <> s_conv then `error else
		(* arguments *)
		let result =
			let rec loop (ds: variable list) (ss: variable list) result = (
				begin match ds, ss with
				| d :: dr, s :: sr ->
					let `named (_, _, `variable (d_t, _), _) = d in
					let `named (_, _, `variable (s_t, _), _) = s in
					let r2 = type_ABI_compatibility ~dest:d_t ~source:s_t in
					if r2 = `error then `error else
					loop dr sr (min_compatibility r2 result)
				| [], [] ->
					result
				| _ :: _, [] ->
					if s_varargs = `varargs then `compatible else `error
				| [], _ :: _ ->
					`error
				end
			) in
			loop d_args s_args `just
		in
		if result = `error then `error else
		(* varargs *)
		let result =
			if d_varargs = s_varargs then result else
			if d_varargs = `none && s_varargs = `varargs then `compatible else
			`error
		in
		if result = `error then `error else
		(* result *)
		let r2 = type_ABI_compatibility ~dest:d_result ~source:s_result in
		if r2 = `error then (
			begin match d_result with
			| `void -> `compatible
			| _ -> `error
			end
		) else (
			min_compatibility r2 result
		)
	);;
	
	(* predefined types *)
	
	let ready_predefined_types (lang: language) (sizeof: sizeof) (typedef: language_typedef): predefined_types = (
		let `sizeof_bool sizeof_bool,
			`sizeof_short sizeof_short,
			`sizeof_int sizeof_int,
			`sizeof_long sizeof_long,
			`sizeof_long_long sizeof_long_long,
			`sizeof_float sizeof_float,
			`sizeof_double sizeof_double,
			`sizeof_long_double sizeof_long_double,
			`sizeof_intptr sizeof_intptr = sizeof
		in
		let `typedef_ptrdiff_t typedef_ptrdiff_t,
			`typedef_size_t typedef_size_t,
			`typedef_wchar_t typedef_wchar_t = typedef
		in
		let predefined_types: (predefined_type * int) list =
			(`void, 0) ::
			(`bool, sizeof_bool) ::
			(`signed_char, 1) ::
			(`unsigned_char, 1) ::
			(`signed_short, sizeof_short) ::
			(`unsigned_short, sizeof_short) ::
			(`signed_int, sizeof_int) ::
			(`unsigned_int, sizeof_int) ::
			(`signed_long, sizeof_long) ::
			(`unsigned_long, sizeof_long) ::
			(`signed_long_long, sizeof_long_long) ::
			(`unsigned_long_long, sizeof_long_long) ::
			(`float, sizeof_float) ::
			(`double, sizeof_double) ::
			(`long_double, sizeof_long_double) ::
			(`decimal32, 4) ::
			(`decimal64, 8) ::
			(`decimal128, 16) ::
			((`imaginary `float), sizeof_float) ::
			((`imaginary `double), sizeof_double) ::
			((`imaginary `long_double), sizeof_long_double) ::
			((`complex `float), sizeof_float * 2) ::
			((`complex `double), sizeof_double * 2) ::
			((`complex `long_double), sizeof_long_double * 2) ::
			(`char, 1) ::
			(`__builtin_va_list, sizeof_intptr) :: (
				begin match lang with
				| `c | `objc ->
					[]
				| `cxx | `objcxx ->
					let sizeof_wchar =
						begin match typedef_wchar_t with
						| `signed_char | `unsigned_char -> 1
						| `signed_short | `unsigned_short -> sizeof_short
						| `signed_int | `unsigned_int -> sizeof_int
						| `signed_long | `unsigned_long -> sizeof_long
						| `signed_long_long | `unsigned_long_long -> sizeof_long_long
						end
					in
					(`wchar, sizeof_wchar) :: []
				end
			)
		in
		let language_typedefs: typedef_type list =
			let find_f t (x, _) = x = (t :> predefined_type) in
			let find t = (fst (List.find (find_f t) predefined_types) :> all_type) in
			(`named (builtin_position, "ptrdiff_t", `typedef (find typedef_ptrdiff_t), no_attributes)) ::
			(`named (builtin_position, "size_t", `typedef (find typedef_size_t), no_attributes)) :: (
				begin match lang with
				| `c | `objc ->
					(`named (builtin_position, "wchar_t", `typedef (find typedef_wchar_t), no_attributes)) :: []
				| `cxx | `objcxx ->
					[]
				end
			)
		in
		predefined_types, language_typedefs
	);;
	
	let select_bit_width_int predefined_types size (t1: int_prec) (t2: int_prec) = (
		let t1_pdt, t1_size = find_predefined_type_with_size t1 predefined_types in
		assert (t1_pdt == (t1 :> predefined_type));
		if t1_size = size then (t1 : int_prec :> [> int_prec]), `none else
		let t2_pdt, t2_size = find_predefined_type_with_size t2 predefined_types in
		assert (t2_pdt == (t2 :> predefined_type));
		if t2_size = size then (t2 : int_prec :> [> int_prec]), `none else
		(t2 : int_prec :> [> int_prec]), `not_had size
	);;
	
	let apply_bit_width_mode
		(predefined_types: predefined_types)
		(bit_width_mode: bit_width_mode)
		(t : all_type)
		: all_type * [> `none | `not_had of int | `not_int] =
	(
		begin match t with
		| #signed_int_prec ->
			begin match bit_width_mode with
			| `__QI__ ->
				assert (`signed_char == find_predefined_type `signed_char predefined_types);
				`signed_char, `none
			| `__HI__ ->
				(select_bit_width_int predefined_types 2 `signed_int `signed_short :> all_type * 'err)
			| `__SI__ ->
				(select_bit_width_int predefined_types 4 `signed_int `signed_long :> all_type * 'err)
			| `__DI__ ->
				(select_bit_width_int predefined_types 8 `signed_long `signed_long_long :> all_type * 'err)
			| `__pointer__ | `__unwind_word__ | `__word__ ->
				let ptrdiff_t = find_ptrdiff_t predefined_types in
				let `named (_, _, `typedef result, _) = ptrdiff_t in
				result, `none
			end
		| #unsigned_int_prec ->
			begin match bit_width_mode with
			| `__QI__ ->
				assert (`unsigned_char == find_predefined_type `unsigned_char predefined_types);
				`unsigned_char, `none
			| `__HI__ ->
				(select_bit_width_int predefined_types 2 `unsigned_int `unsigned_short :> all_type * 'err)
			| `__SI__ ->
				(select_bit_width_int predefined_types 4 `unsigned_int `unsigned_long :> all_type * 'err)
			| `__DI__ ->
				(select_bit_width_int predefined_types 8 `unsigned_long `unsigned_long_long :> all_type * 'err)
			| `__pointer__ | `__unwind_word__ | `__word__ ->
				let size_t = find_size_t predefined_types in
				let `named (_, _, `typedef result, _) = size_t in
				result, `none
			end
		| _ ->
			t, `not_int
		end
	);;
	
	(* derived types *)
	
	let find_const_type (t: all_type) (xs: derived_types): [> const_type] * derived_types = (
		begin match t with
		| `const _ as t ->
			t, xs
		| #not_const_type as t ->
			begin try
				let rec loop xs = (
					begin match xs with
					| [] ->
						raise Not_found
					| x :: xr ->
						begin match x with
						| `const (u: not_const_type) as x when u == t ->
							(x :> [> const_type])
						| _ ->
							loop xr
						end
					end
				) in
				loop xs, xs
			with Not_found ->
				let new_item: [> const_type] = `const t in
				new_item, ((new_item :> derived_type) :: xs)
			end
		end
	);;
	
	let find_volatile_type (t: all_type) (xs: derived_types)
		: [> volatile_type | `const of [> volatile_type]] * derived_types =
	(
		(* order of qualifier: const of volatile of t *)
		let make_volatile (t: not_qualified_type) (xs: derived_types)
			: [> volatile_type] * derived_types =
		(
			begin try
				let rec loop xs = (
					begin match xs with
					| [] ->
						raise Not_found
					| x :: xr ->
						begin match x with
						| `volatile (u: not_qualified_type) as x when u == t ->
							(x :> [> volatile_type])
						| _ ->
							loop xr
						end
					end
				) in
				loop xs, xs
			with Not_found ->
				let new_item: [> volatile_type] = `volatile t in
				new_item, ((new_item :> derived_type) :: xs)
			end
		) in
		begin match t with
		| `volatile _ | `const (`volatile _) as t ->
			t, xs
		| `const (#not_qualified_type as t) ->
			let t, xs = make_volatile t xs in
			begin match find_const_type (t :> all_type) xs with
			| (`const (`volatile _)), _ as result ->
				result
			| _ ->
				assert false (* does not come here *)
			end
		| #not_qualified_type as t ->
			make_volatile t xs
		end
	);;
	
	let find_pointer_type (t: all_type) (xs: derived_types)
		: [> pointer_type] * derived_types =
	(
		begin try
			let rec loop xs = (
				begin match xs with
				| [] ->
					raise Not_found
				| x :: xr ->
					begin match x with
					| `pointer u as x when u == t ->
						(x :> [> pointer_type])
					| `pointer (`named (_, tag, `opaque_enum, _)) as x
						when (match t with `named (_, o_tag, `enum _, _) -> tag = o_tag | _ -> false)
					->
						(x :> [> pointer_type])
					| `pointer (`named (_, tag, `opaque_struct, _)) as x
						when (match t with `named (_, o_tag, `struct_type _, _) -> tag = o_tag | _ -> false)
					->
						(x :> [> pointer_type])
					| `pointer (`named (_, tag, `opaque_union, _)) as x
						when (match t with `named (_, o_tag, `union _, _) -> tag = o_tag | _ -> false)
					->
						(x :> [> pointer_type])
					| _ ->
						loop xr
					end
				end
			) in
			loop xs, xs
		with Not_found ->
			let new_item: [> pointer_type] = `pointer t in
			new_item, ((new_item :> derived_type) :: xs)
		end
	);;
	
	let find_block_pointer_type (t: function_type) (xs: derived_types)
		: [> `block_pointer of function_type] * derived_types =
	(
		begin try
			let rec loop xs = (
				begin match xs with
				| [] ->
					raise Not_found
				| x :: xr ->
					begin match x with
					| `block_pointer u as x when u == t ->
						(x :> [> `block_pointer of function_type])
					| _ ->
						loop xr
					end
				end
			) in
			loop xs, xs
		with Not_found ->
			let new_item: [> `block_pointer of function_type] = `block_pointer t in
			new_item, ((new_item :> derived_type) :: xs)
		end
	);;
	
	let find_array_type (n: Integer.t option) (t: all_type) (xs: derived_types)
		: [> array_type | `volatile of [> array_type]
			| `const of [> array_type | `volatile of [> array_type]]] * derived_types =
	(
		let make_array (n: Integer.t option) (t: not_qualified_type) (xs: derived_types)
			: [> array_type] * derived_types =
		(
			let make_n_array (n: Integer.t option) (t: not_qualified_type) (xs: derived_types)
				: [> array_type] * derived_types =
			(
				begin try
					let rec loop xs = (
						begin match xs with
						| [] ->
							raise Not_found
						| x :: xr ->
							begin match x with
							| `array (m, u) as x when n = m && u == t ->
								(x :> [> array_type])
							| _ ->
								loop xr
							end
						end
					) in
					loop xs, xs
				with Not_found ->
					let new_item: [> array_type] = `array (n, t) in
					new_item, ((new_item :> derived_type) :: xs)
				end
			) in
			begin match n with
			| Some _ ->
				let _, xs = make_n_array None t xs in
				make_n_array n t xs
			| None ->
				make_n_array n t xs
			end
		) in
		begin match t with
		| #not_qualified_type as t ->
			make_array n t xs
		| `volatile (#not_qualified_type as t) ->
			let t, xs = make_array n t xs in
			begin match find_volatile_type (t :> all_type) xs with
			| (`volatile (#array_type)), _ as result ->
				result
			| _ ->
				assert false (* does not come here *)
			end
		| `const (`volatile (#not_qualified_type as t)) ->
			let t, xs = make_array n t xs in
			let t, xs = find_volatile_type t xs in
			begin match find_const_type t xs with
			| (`const (`volatile (#array_type))), _ as result ->
				result
			| _ ->
				assert false (* does not come here *)
			end
		| `const (#not_qualified_type as t) ->
			let t, xs = make_array n t xs in
			begin match find_const_type t xs with
			| (`const (#array_type)), _ as result ->
				result
			| _ ->
				assert false (* does not come here *)
			end
		end
	);;
	
	let find_restrict_type (t: pointer_type) (xs: derived_types): [> restrict_type] * derived_types = (
		begin try
			let rec loop xs = (
				begin match xs with
				| [] ->
					raise Not_found
				| x :: xr ->
					begin match x with
					| `restrict u as x when u == t ->
						(x :> [> restrict_type])
					| _ ->
						loop xr
					end
				end
			) in
			loop xs, xs
		with Not_found ->
			let new_item: [> restrict_type] = `restrict t in
			new_item, ((new_item :> derived_type) :: xs)
		end
	);;
	
	(* anonymous types *)
	
	let prototype_eq (left: prototype) (right: prototype): bool = (
		let d_conv, d_args, d_varargs, d_result = left in
		let s_conv, s_args, s_varargs, s_result = right in
		(* calling convention *)
		if d_conv <> s_conv then false else
		(* arguments *)
		let result =
			let rec loop (ds: variable list) (ss: variable list): bool = (
				begin match ds, ss with
				| d :: dr, s :: sr ->
					let `named (_, d_n, `variable (d_t, _), d_attr) = d in
					let `named (_, s_n, `variable (s_t, _), s_attr) = s in
					if d_n <> "" || s_n <> "" then false else (* named parameter is not eq, always *)
					if d_t != s_t then false else
					if d_attr <> s_attr then false else
					loop dr sr
				| [], [] ->
					true
				| _ :: _, [] | [], _ :: _ ->
					false
				end
			) in
			loop d_args s_args
		in
		if not result then false else
		(* varargs *)
		if d_varargs <> s_varargs then false else
		(* result *)
		d_result == s_result
	);;
	
	let find_function_type (prototype: prototype) (source: source_item list)
		: [> function_type] * source_item list =
	(
		let rec loop xs = (
			begin match xs with
			| x :: xr ->
				begin match x with
				| `function_type u as x when prototype_eq u prototype ->
					x, source
				| _ ->
					loop xr
				end
			| [] ->
				let func_type: [> function_type] = `function_type prototype in
				let source = (func_type :> source_item) :: source in
				func_type, source
			end
		) in
		loop source
	);;
	
	(* named types *)
	
	let find_enum
		(id: ranged_position * [`ident of string])
		(namespace: namespace)
		(source: source_item list)
		: [> [> opaquable_enum_var] with_name] * namespace * source_item list =
	(
		let id_p, `ident id_e = id in
		try
			begin match StringMap.find id_e namespace.ns_enum with
			| `named (_, _, `enum _, _) as item ->
				item, namespace, source
			end
		with Not_found ->
		try
			begin match StringMap.find id_e namespace.ns_opaque_enum with
			| `named (_, _, `opaque_enum, _) as item ->
				item, namespace, source
			end
		with Not_found ->
			let item: [> [> `opaque_enum] with_name] =
				`named (id_p, id_e, `opaque_enum, no_attributes)
			in
			let namespace = {namespace with ns_opaque_enum = StringMap.add id_e (item :> opaque_enum_type) namespace.ns_opaque_enum} in
			let source = (item :> source_item) :: source in
			item, namespace, source
	);;
	
	let find_struct
		(id: ranged_position * [`ident of string])
		(namespace: namespace)
		(source: source_item list)
		: [> [> opaquable_struct_var] with_name] * namespace * source_item list =
	(
		let id_p, `ident id_e = id in
		try
			begin match StringMap.find id_e namespace.ns_struct with
			| `named (_, _, `struct_type _, _) as item ->
				item, namespace, source
			end
		with Not_found ->
		try
			begin match StringMap.find id_e namespace.ns_opaque_struct with
			| `named (_, _, `opaque_struct, _) as item ->
				item, namespace, source
			end
		with Not_found ->
			let item: [> [> `opaque_struct] with_name] =
				`named (id_p, id_e, `opaque_struct, no_attributes)
			in
			let namespace = {namespace with ns_opaque_struct = StringMap.add id_e (item :> opaque_struct_type) namespace.ns_opaque_struct} in
			let source = (item :> source_item) :: source in
			item, namespace, source
	);;
	
	let find_union
		(id: ranged_position * [`ident of string])
		(namespace: namespace)
		(source: source_item list)
		: [> [> opaquable_union_var] with_name] * namespace * source_item list =
	(
		let id_p, `ident id_e = id in
		try
			begin match StringMap.find id_e namespace.ns_union with
			| `named (_, _, `union _, _) as item ->
				item, namespace, source
			end
		with Not_found ->
		try
			begin match StringMap.find id_e namespace.ns_opaque_union with
			| `named (_, _, `opaque_union, _) as item ->
				item, namespace, source
			end
		with Not_found ->
			let item: [> [> `opaque_union] with_name] =
				`named (id_p, id_e, `opaque_union, no_attributes)
			in
			let namespace = {namespace with ns_opaque_union = StringMap.add id_e (item :> opaque_union_type) namespace.ns_opaque_union} in
			let source = (item :> source_item) :: source in
			item, namespace, source
	);;
	
	let find_enum_by_element
		(element: enum_item)
		(predefined_types: predefined_types)
		(namespace: namespace)
		: all_type =
	(
		begin try
			let `named (_, element_name, `enum_element _, _) = element in
			let result = StringMap.find element_name namespace.ns_enum_of_element in
			(result :> all_type)
		with Not_found ->
			(* when building emum type, ns_enum_of_element has not been set yet *)
			find_predefined_type `signed_int predefined_types
		end
	);;
	
	let resolve_opaque (namespace: namespace)
		(t: [> [> opaque_type_var | non_opaque_type_var] with_name] as 'a)
		: 'a =
	(
		begin match t with
		| `named (_, name, `opaque_enum, _) ->
			begin try
				begin match StringMap.find name namespace.ns_enum with
				| `named (_, _, `enum _, _) as result -> result
				end
			with Not_found ->
				t
			end
		| `named (_, name, `opaque_struct, _) ->
			begin try
				begin match StringMap.find name namespace.ns_struct with
				| `named (_, _, `struct_type _, _) as result -> result
				end
			with Not_found ->
				t
			end
		| `named (_, name, `opaque_union, _) ->
			begin try
				begin match StringMap.find name namespace.ns_union with
				| `named (_, _, `union _, _) as result -> result
				end
			with Not_found ->
				t
			end
		| _ ->
			t
		end
	);;
	
	(* sizeof / alignof *)
	
	let rec sizeof_predefined_type
		(e: [< predefined_type])
		(predefined_types: predefined_types): int =
	(
		snd (find_predefined_type_with_size e predefined_types)
	) and alignof_predefined_type
		(e: [< predefined_type])
		(predefined_types: predefined_types): int =
	(
		let sizeof = sizeof_predefined_type e predefined_types in
		let rec loop sizeof alignof = (
			if alignof >= sizeof then alignof else
			loop sizeof (alignof * 2)
		) in
		loop sizeof 1
	);;
	
	let rec sizeof
		(t: all_type)
		(predefined_types: predefined_types)
		: int option =
	(
		begin match t with
		| #predefined_type as p ->
			Some (sizeof_predefined_type p predefined_types)
		| `pointer _ | `block_pointer _ ->
			Some (sizeof_predefined_type `__builtin_va_list predefined_types)
		| `restrict base ->
			sizeof (base :> all_type) predefined_types
		| `volatile base ->
			sizeof (base :> all_type) predefined_types
		| `const base ->
			sizeof (base :> all_type) predefined_types
		| `anonymous (_, `enum _)
		| `named (_, _, `enum _, _) ->
			Some (sizeof_predefined_type `signed_int predefined_types)
		| `anonymous (_, (`struct_type (_, items)))
		| `named (_, _, (`struct_type (_, items)), _) ->
			if is_bitfield items then (
				let rec loop_bf total bits items = (
					begin match items with
					| (_, item_type, Some (item_pos, item_bits, item_explicit_bf), _) :: items ->
						begin match alignof item_type predefined_types with
						| Some item_align ->
							if item_explicit_bf then (
								if bits + item_bits > item_align * 8 then (
									let total = total + item_align in
									let bits = item_bits in
									assert (total * 8 = item_pos);
									loop_bf total bits items
								) else (
									assert (total * 8 + bits = item_pos);
									let bits = bits + item_bits in
									loop_bf total bits items
								)
							) else (
								let total = total + (bits + 7) / 8 in
								let total = (total + item_align - 1) / item_align * item_align in
								assert (total * 8 = item_pos);
								assert (item_bits mod 8 = 0);
								let total = total + item_bits / 8 in
								loop_bf total 0 items
							)
						| None ->
							None
						end
					| (_, _, None, _) :: _ ->
						assert false; (* non bit field member is mixed *)
					| [] ->
						let total = total + (bits + 7) / 8 in
						begin match alignof t predefined_types with
						| Some total_align ->
							Some ((total + total_align - 1) / total_align * total_align)
						| None ->
							None
						end
					end
				) in
				loop_bf 0 0 items
			) else (
				let rec loop_normal total items = (
					begin match items with
					| (_, item_type, _, _) :: items ->
						begin match sizeof item_type predefined_types with
						| Some item_size ->
							begin match alignof item_type predefined_types with
							| Some item_align ->
								let total = (total + item_align - 1) / item_align * item_align + item_size in
								loop_normal total items
							| None ->
								None
							end
						| None ->
							None
						end
					| [] ->
						begin match alignof t predefined_types with
						| Some total_align ->
							Some ((total + total_align - 1) / total_align * total_align)
						| None ->
							None
						end
					end
				) in
				loop_normal 0 items
			)
		| `anonymous (_, `union items)
		| `named (_, _, `union items, _) ->
			let rec loop max_size items = (
				begin match items with
				| (_, item_type, _, _) :: items ->
					begin match sizeof item_type predefined_types with
					| Some item_size ->
						loop (max max_size item_size) items
					| None ->
						None
					end
				| [] ->
					Some max_size
				end
			) in
			loop 0 items
		| `named (_, _, `typedef base, _) ->
			sizeof base predefined_types
		| `array (Some n, base) ->
			begin match sizeof (base :> all_type) predefined_types with
			| Some item_size ->
				Some (item_size * Integer.to_int n)
			| None ->
				None
			end
		| `array (None, _)
		| `named (_, _, (`generic_type | `opaque_enum | `opaque_struct | `opaque_union), _)
		| `function_type _ ->
			None
		end
	) and alignof
		(t: all_type)
		(predefined_types: predefined_types)
		: int option =
	(
		begin match t with
		| #predefined_type as p ->
			Some (alignof_predefined_type p predefined_types)
		| `pointer _ | `block_pointer _ ->
			Some (alignof_predefined_type `__builtin_va_list predefined_types)
		| `restrict base ->
			alignof (base :> all_type) predefined_types
		| `volatile base ->
			alignof (base :> all_type) predefined_types
		| `const base ->
			alignof (base :> all_type) predefined_types
		| `anonymous (_, `enum _)
		| `named (_, _, `enum _, _) ->
			Some (alignof_predefined_type `signed_int predefined_types)
		| `anonymous (_, `struct_type (alignment, items))
		| `named (_, _, `struct_type (alignment, items), _) ->
			begin match alignment with
			| `default | `explicit_aligned ->
				alignof_struct items predefined_types
			| `aligned n ->
				Some n
			| `packed ->
				Some 1
			end
		| `anonymous (_, `union items)
		| `named (_, _, `union items, _) ->
			let rec loop max_align items = (
				begin match items with
				| (_, item_type, _, _) :: items ->
					begin match alignof item_type predefined_types with
					| Some item_align ->
						loop (max max_align item_align) items
					| None ->
						None
					end
				| [] ->
					Some max_align
				end
			) in
			loop 0 items
		| `named (_, _, `typedef base, _) ->
			alignof base predefined_types
		| `array (Some _, base) ->
			alignof (base :> all_type) predefined_types
		| `array (None, _)
		| `named (_, _, (`generic_type | `opaque_enum | `opaque_struct | `opaque_union), _)
		| `function_type _ ->
			None
		end
	) and alignof_struct
		(items: struct_item list)
		(predefined_types: predefined_types)
		: int option =
	(
		let rec loop max_align items = (
			begin match items with
			| (_, item_type, (None | Some (_, _, false)), _) :: items ->
				begin match alignof item_type predefined_types with
				| Some item_align ->
					loop (max max_align item_align) items
				| None ->
					None
				end
			| (_, _, Some (_, _, true), _) :: items ->
				loop max_align items
			| [] ->
				Some max_align
			end
		) in
		loop 1 items
	);;
	
	(* bit-field *)
	
	type bitfield_or_not = [`empty | `is_bitfield | `is_not_bitfield | `mixed];;
	
	let is_bitfield (xs: struct_item list): bitfield_or_not = (
		let rec check_loop xs result = (
			begin match xs with
			| x :: xr ->
				begin match x with
				| _, _, Some _, _ ->
					begin match result with
					| `is_not_bitfield -> `mixed
					| `is_bitfield | `empty -> check_loop xr `is_bitfield
					| `mixed -> assert false
					end
				| _, _, None, _ ->
					begin match result with
					| `is_bitfield -> `mixed
					| `is_not_bitfield | `empty -> check_loop xr `is_not_bitfield
					| `mixed -> assert false
					end
				end
			| [] ->
				result
			end
		) in
		check_loop xs `empty
	);;
	
	let fill_bitfield
		(predefined_types: predefined_types)
		(xs: struct_item list)
		: struct_item list * [`error of struct_item | `none] =
	(
		let rec loop xs total_bytes bits = (
			begin match xs with
			| x :: xr ->
				begin match x with
				| item_name, item_type, Some (_, item_bits, item_explicit_bf), item_attrs ->
					assert item_explicit_bf;
					begin match alignof item_type predefined_types with
					| Some item_align ->
						if bits + item_bits > item_align * 8 then (
							let total_bytes = total_bytes + item_align in
							let x = item_name, item_type, Some (total_bytes * 8, item_bits, true), item_attrs in
							let bits = item_bits in
							let rr, error_report = loop xr total_bytes bits in
							x :: rr, error_report
						) else (
							let x = item_name, item_type, Some (total_bytes * 8 + bits, item_bits, true), item_attrs in
							let bits = bits + item_bits in
							let rr, error_report = loop xr total_bytes bits in
							x :: rr, error_report
						)
					| None ->
						[], `error x
					end
				| item_name, item_type, None, item_attrs ->
					begin match sizeof item_type predefined_types with
					| Some item_size ->
						begin match alignof item_type predefined_types with
						| Some item_align ->
							let total_bytes = total_bytes + (bits + 7) / 8 in
							let total_bytes = (total_bytes + item_align - 1) / item_align * item_align in
							let x = item_name, item_type, Some (total_bytes * 8, item_size * 8, false), item_attrs in
							let total_bytes = total_bytes + item_size in
							let rr, error_report = loop xr total_bytes 0 in
							x :: rr, error_report
						| None ->
							[], `error x
						end
					| None ->
						[], `error x
					end
				end
			| [] ->
				[], `none
			end
		) in
		loop xs 0 0
	);;
	
end;;
