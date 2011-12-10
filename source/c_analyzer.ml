open C_analyzer_errors;;
open C_semantics;;
open C_semantics_machine;;
open C_syntax;;
open C_syntax_traversing;;
open Position;;
open Value;;

let bind_option (f: 'a -> 'a) (x : 'a option): 'a option = (
	begin match x with
	| Some x -> Some (f x)
	| None -> None
	end
);;

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

let builtin_name = "<builtin>";;

let builtin_position: ranged_position =
	let p = builtin_name, 0, 0, 0 in
	p, p;;

module Analyzer
	(Literals: LiteralsType)
	(Syntax: SyntaxType (Literals).S)
	(Semantics: SemanticsType (Literals).S) =
struct
	module Traversing = Traversing (Literals) (Syntax);;
	module Machine = Machine (Literals) (Semantics);;
	open Literals;;
	open Semantics;;
	
	(* in *)
	
	type 'a p = 'a Syntax.p;;
	type 'a pe = 'a Syntax.pe;;
	type 'a opt = 'a Syntax.opt;;
	
	type define = [
		| `operator of operator
		| `declaration_specifiers of Syntax.declaration_specifiers
		| `initializer_t of Syntax.initializer_t
		| `function_expr of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.expression
		| `function_stmt of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.statement
		| `any of string];;
	
	(* source info *)
	
	let empty_source = [], no_extra_info;;
	
	(* attributes *)
	
	let attributes_of_alignment (alignment: alignment): attributes = (
		if alignment = `default then no_attributes else
		{no_attributes with at_aligned = alignment}
	);;
	
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
	
	let empty_namespace = {
		ns_namespace = StringMap.empty;
		ns_enum_of_element = StringMap.empty;
		ns_opaque_enum = StringMap.empty;
		ns_enum = StringMap.empty;
		ns_opaque_struct = StringMap.empty;
		ns_struct = StringMap.empty;
		ns_opaque_union = StringMap.empty;
		ns_union = StringMap.empty};;
	
	let to_opaque_mapping
		(opaques: 'a StringMap.t)
		(fulls: 'b StringMap.t)
		: ('a * 'b) StringMap.t =
	(
		let add_to_opaque_mapping
			(opaques: 'a StringMap.t)
			(tag: string)
			(full: 'b)
			(r: ('a * 'b) StringMap.t)
			: ('a * 'b) StringMap.t =
		(
			StringMap.add tag (StringMap.find tag opaques, full) r
		) in
		StringMap.fold (add_to_opaque_mapping opaques) fulls StringMap.empty
	);;
	
	let opaque_mapping (namespace: namespace): opaque_mapping = (
		to_opaque_mapping namespace.ns_opaque_enum namespace.ns_enum,
		to_opaque_mapping namespace.ns_opaque_struct namespace.ns_struct,
		to_opaque_mapping namespace.ns_opaque_union namespace.ns_union
	);;
	
	let rec resolve_opaque (namespace: namespace) (t: all_type): all_type = (
		begin match t with
		| `named (_, name, `opaque_enum, _) ->
			(try (StringMap.find name namespace.ns_enum :> all_type) with Not_found -> t)
		| `named (_, name, `opaque_struct, _) ->
			(try (StringMap.find name namespace.ns_struct :> all_type) with Not_found -> t)
		| `named (_, name, `opaque_union, _) ->
			(try (StringMap.find name namespace.ns_union :> all_type) with Not_found -> t)
		| _ ->
			t
		end
	);;
	
	(* predefined types *)
	
	type predefined_types = Semantics.predefined_types;;
	
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
	
	(* derived types *)
	
	type derived_types = derived_type list;;
	
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
	
	let find_array_type (n: Integer.t option) (t: all_type) (xs: derived_types)
		: [> array_type | `volatile of [> array_type]
			| `const of [> array_type | `volatile of [> array_type]]] * derived_types =
	(
		let make_array (t: not_qualified_type) (xs: derived_types)
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
		begin match t with
		| #not_qualified_type as t ->
			make_array t xs
		| `volatile (#not_qualified_type as t) ->
			let t, xs = make_array t xs in
			begin match find_volatile_type (t :> all_type) xs with
			| (`volatile (#array_type)), _ as result ->
				result
			| _ ->
				assert false (* does not come here *)
			end
		| `const (`volatile (#not_qualified_type as t)) ->
			let t, xs = make_array t xs in
			begin match find_const_type t xs with
			| (`const (#array_type)), _ as result ->
				result
			| _ ->
				assert false (* does not come here *)
			end
		| `const (#not_qualified_type as t) ->
			let t, xs = make_array t xs in
			let t, xs = find_volatile_type t xs in
			begin match find_const_type t xs with
			| (`const (`volatile (#array_type))), _ as result ->
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
	
	let find_function_type (prototype: prototype) (source: source_item list)
		: function_type * source_item list =
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
				let func_type: function_type = `function_type prototype in
				let source = (func_type :> source_item) :: source in
				func_type, source
			end
		) in
		loop source
	);;
	
	(* named types *)
	
	let find_enum
		(id: Syntax.identifier p)
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
		(id: Syntax.identifier p)
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
		(id: Syntax.identifier p)
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
	
	(* implicit conversion *)
	
	let conv_expr (t: all_type) (expr: expression): expression = (
		begin match resolve_typedef t with
		| #predefined_numeric_type as t1 ->
			begin match resolve_typedef (snd expr) with
			| #predefined_numeric_type as t2 when t1 != t2 ->
				`implicit_conv expr, t
			| _ ->
				expr
			end
		| `pointer (`const `char) ->
			begin match expr with
			| `chars_literal _, `array (_, `char) ->
				`implicit_conv expr, t (* char array literal -> char *)
			| _ ->
				expr
			end
		| `pointer x when x != `void ->
			begin match expr with
			| `cast (`int_literal (_, n), _ as z), t2
			| `explicit_conv (`int_literal (_, n), _ as z), t2
			| `implicit_conv (`int_literal (_, n), _ as z), t2
				when (match resolve_typedef t2 with `pointer `void -> true | _ -> false)
					&& Integer.compare n Integer.zero = 0
			->
				`implicit_conv z, t (* NULL... is it removing the cast "(void * )"0, ok? *)
			| _ ->
				expr
			end
		| _ ->
			expr
		end
	);;
	
	(* zero value *)
	
	let zero_expression
		(error: ranged_position -> string -> unit)
		(ps: ranged_position)
		(t: not_qualified_type)
		: expression =
	(
		begin match t with
		| `char ->
			`char_literal '\x00', (t :> all_type)
		| _ ->
			error ps "unimplemented!";
			assert false
		end
	);;
	
	(* analyzing *)
	
	let get_bit_width_int error predefined_types ps size t1 t2 = (
		if Machine.sizeof_predefined_type t1 predefined_types = size then t1 else
		if Machine.sizeof_predefined_type t2 predefined_types = size then t2 else (
			error ps ("this environment does not have " ^ string_of_int (size * 8) ^ " bit-integer.");
			t2
		)
	);;
	
	let apply_bit_width_mode
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(ps: ranged_position)
		(bit_width_mode: bit_width_mode)
		(t : all_type)
		: all_type =
	(
		begin match t with
		| #signed_int_prec ->
			begin match bit_width_mode with
			| `__QI__ ->
				let t = `signed_char in
				find_predefined_type t predefined_types
			| `__HI__ ->
				let t = get_bit_width_int error predefined_types ps 2 `signed_int `signed_short in
				find_predefined_type t predefined_types
			| `__SI__ ->
				let t = get_bit_width_int error predefined_types ps 4 `signed_int `signed_long in
				find_predefined_type t predefined_types
			| `__DI__ ->
				let t = get_bit_width_int error predefined_types ps 8 `signed_long `signed_long_long in
				find_predefined_type t predefined_types
			| `__pointer__ | `__unwind_word__ | `__word__ ->
				let ptrdiff_t = find_ptrdiff_t predefined_types in
				let `named (_, _, `typedef result, _) = ptrdiff_t in
				result
			end
		| #unsigned_int_prec ->
			begin match bit_width_mode with
			| `__QI__ ->
				let t = `unsigned_char in
				find_predefined_type t predefined_types
			| `__HI__ ->
				let t = get_bit_width_int error predefined_types ps 2 `unsigned_int `unsigned_short in
				find_predefined_type t predefined_types
			| `__SI__ ->
				let t = get_bit_width_int error predefined_types ps 4 `unsigned_int `unsigned_long in
				find_predefined_type t predefined_types
			| `__DI__ ->
				let t = get_bit_width_int error predefined_types ps 8 `unsigned_long `unsigned_long_long in
				find_predefined_type t predefined_types
			| `__pointer__ | `__unwind_word__ | `__word__ ->
				let size_t = find_size_t predefined_types in
				let `named (_, _, `typedef result, _) = size_t in
				result
			end
		| _ ->
			error ps "attribute \"__mode__\" was used for not int type.";
			t
		end
	);;
	
	type type_specifier_set = {
		ts_void: int;
		ts_char: int;
		ts_short: int;
		ts_int: int;
		ts_long: int;
		ts_float: int;
		ts_double: int;
		ts_signed: int;
		ts_unsigned: int;
		ts_bool: int;
		ts_imaginary: int;
		ts_complex: int;
		ts_int64: int;
		ts_builtin_va_list: int};;
	
	let no_type_specifier_set = {
		ts_void = 0;
		ts_char = 0;
		ts_short = 0;
		ts_int = 0;
		ts_long = 0;
		ts_float = 0;
		ts_double = 0;
		ts_signed = 0;
		ts_unsigned = 0;
		ts_bool = 0;
		ts_imaginary = 0;
		ts_complex = 0;
		ts_int64 = 0;
		ts_builtin_va_list = 0};;
	
	let get_type_by_specifier_set
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(ps: ranged_position)
		(bit_width_mode: bit_width_mode option)
		(set, named: type_specifier_set * all_type option)
		: all_type =
	(
		let invalid = "invalid combination of type-specifier(s)." in
		begin match named with
		| Some named ->
			if set <> no_type_specifier_set then (
				error ps invalid
			);
			if bit_width_mode <> None then (
				error ps "attribute \"__mode__\" was used for not int type."
			);
			named
		| None ->
			let t =
				if set = {no_type_specifier_set with ts_void = 1} then (
					`void
				) else if set = {no_type_specifier_set with ts_bool = 1} then (
					`bool
				) else if set = {no_type_specifier_set with ts_char = 1} then (
					`char
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_char = 1} then (
					`signed_char
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_char = 1} then (
					`unsigned_char
				) else if set = {no_type_specifier_set with ts_short = 1} then (
					`signed_short
				) else if set = {no_type_specifier_set with ts_short = 1; ts_int = 1} then (
					`signed_short
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_short = 1} then (
					`signed_short
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_short = 1; ts_int = 1} then (
					`signed_short
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_short = 1} then (
					`unsigned_short
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_short = 1; ts_int = 1} then (
					`unsigned_short
				) else if set = {no_type_specifier_set with ts_int = 1} then (
					`signed_int
				) else if set = {no_type_specifier_set with ts_signed = 1} then (
					`signed_int
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_int = 1} then (
					`signed_int
				) else if set = {no_type_specifier_set with ts_unsigned = 1} then (
					`unsigned_int
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_int = 1} then (
					`unsigned_int
				) else if set = {no_type_specifier_set with ts_long = 1} then (
					`signed_long
				) else if set = {no_type_specifier_set with ts_long = 1; ts_int = 1} then (
					`signed_long
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_long = 1} then (
					`signed_long
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_long = 1; ts_int = 1} then (
					`signed_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_long = 1} then (
					`unsigned_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_long = 1; ts_int = 1} then (
					`unsigned_long
				) else if set = {no_type_specifier_set with ts_long = 2} then (
					`signed_long_long
				) else if set = {no_type_specifier_set with ts_long = 2; ts_int = 1} then (
					`signed_long_long
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_long = 2} then (
					`signed_long_long
				) else if set = {no_type_specifier_set with ts_signed = 1; ts_long = 2; ts_int = 1} then (
					`signed_long_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_long = 2} then (
					`unsigned_long_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_long = 2; ts_int = 1} then (
					`unsigned_long_long
				) else if set = {no_type_specifier_set with ts_float = 1} then (
					`float
				) else if set = {no_type_specifier_set with ts_double = 1} then (
					`double
				) else if set = {no_type_specifier_set with ts_long = 1; ts_double = 1} then (
					`long_double
				) else if set = {no_type_specifier_set with ts_float = 1; ts_imaginary = 1} then (
					`imaginary `float
				) else if set = {no_type_specifier_set with ts_double = 1; ts_imaginary = 1} then (
					`imaginary `double
				) else if set = {no_type_specifier_set with ts_long = 1; ts_double = 1; ts_imaginary = 1} then (
					`imaginary `long_double
				) else if set = {no_type_specifier_set with ts_float = 1; ts_complex = 1} then (
					`complex `float
				) else if set = {no_type_specifier_set with ts_double = 1; ts_complex = 1} then (
					`complex `double
				) else if set = {no_type_specifier_set with ts_long = 1; ts_double = 1; ts_complex = 1} then (
					`complex `long_double
				) else if set = {no_type_specifier_set with ts_int64 = 1} then (
					get_bit_width_int error predefined_types ps 8 `signed_long `signed_long_long
				) else if set = {no_type_specifier_set with ts_unsigned = 1; ts_int64 = 1} then (
					get_bit_width_int error predefined_types ps 8 `unsigned_long `unsigned_long_long
				) else if set = {no_type_specifier_set with ts_builtin_va_list = 1} then (
					`__builtin_va_list
				) else (
					error ps invalid;
					`signed_int
				)
			in
			begin match bit_width_mode with
			| Some bit_width_mode ->
				apply_bit_width_mode error predefined_types ps bit_width_mode (find_predefined_type t predefined_types)
			| None ->
				find_predefined_type t predefined_types
			end
		end
	);;
	
	type type_qualifier_set = {
		tq_const: bool;
		tq_restrict: bool;
		tq_volatile: bool};;
	
	let no_type_qualifier_set = {
		tq_const = false;
		tq_restrict = false;
		tq_volatile = false};;
	
	let get_type_by_qualifier_set
		(error: ranged_position -> string -> unit)
		(derived_types: derived_types)
		(ps: ranged_position)
		(t: all_type)
		(qualifiers: type_qualifier_set)
		: derived_types * all_type =
	(
		let t, derived_types =
			if qualifiers.tq_const then (
				find_const_type t derived_types
			) else (
				t, derived_types
			)
		in
		let t, derived_types =
			if qualifiers.tq_restrict then (
				begin match t with
				| `pointer _ as t ->
					find_restrict_type t derived_types
				| _ ->
					error ps "\"restrict\" could not be applied to not pointer type!";
					t, derived_types
				end
			) else (
				t, derived_types
			)
		in
		let t, derived_types =
			if qualifiers.tq_volatile then (
				find_volatile_type t derived_types
			) else (
				t, derived_types
			)
		in
		derived_types, t
	);;
	
	let integer_cast
		(predefined_types: predefined_types)
		(t: int_prec)
		(x: Integer.t): expression =
	(
		let bit_size = Machine.sizeof_predefined_type t predefined_types * 8in
		let bits = Integer.sub (Integer.shift_left Integer.one bit_size) Integer.one in
		let x2 = Integer.logand x bits in
		begin match t with
		| `signed_char | `signed_short | `signed_int | `signed_long | `signed_long_long ->
			let x3 =
				if Integer.compare x Integer.zero >= 0 then x2 else
				Integer.logor x2 (Integer.shift_left (Integer.of_int ~-1) bit_size)
			in
			`int_literal (t, x3), (t :> all_type)
		| `unsigned_char | `unsigned_short | `unsigned_int | `unsigned_long | `unsigned_long_long ->
			`int_literal (t, x2), (t :> all_type)
		end
	);;
	
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
	
	let real_prec (prec1: real_prec) (prec2: real_prec): real_prec = (
		begin match prec1, prec2 with
		| `decimal128, _ | _, `decimal128 -> `decimal128
		| `decimal64, _ | _, `decimal64 -> `decimal64
		| `decimal32, _ | _, `decimal32 -> `decimal32
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
		| (#real_prec as real_t), #int_prec
		| #int_prec, (#real_prec as real_t) ->
			Some real_t, derived_types
		| (#real_prec as prec1), (#real_prec as prec2) ->
			Some (find_predefined_type (real_prec prec1 prec2) predefined_types), derived_types
		| (`imaginary prec1), (`imaginary prec2) when op <> `multiplicative ->
			Some (find_predefined_type (`imaginary (float_prec prec1 prec2)) predefined_types), derived_types
		| (#float_prec as prec1), (`imaginary prec2 | `complex prec2)
		| (`imaginary prec1 | `complex prec1), (#float_prec as prec2)
		| (`imaginary prec1 | `complex prec1), (`imaginary prec2 | `complex prec2) ->
			Some (find_predefined_type (`complex (float_prec prec1 prec2)) predefined_types), derived_types
		| (`pointer _ as ptr_t), #int_prec
		| (`restrict (`pointer _) as ptr_t), #int_prec when op = `add || op = `sub ->
			Some ptr_t, derived_types
		| #int_prec, (`pointer _ as ptr_t)
		| #int_prec, (`restrict (`pointer _) as ptr_t) when op = `add ->
			Some ptr_t, derived_types
		| `array (_, elm_t), #int_prec when op = `add || op = `sub ->
			let ptr_t, derived_types = find_pointer_type (elm_t :> all_type) derived_types in
			Some ptr_t, derived_types
		| #int_prec, `array (_, elm_t) when op = `add ->
			let ptr_t, derived_types = find_pointer_type (elm_t :> all_type) derived_types in
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
		| _, (`void as void_t) when op = `conditional ->
			Some void_t, derived_types
		| (`named (_, _, `generic_type, _) as g_t), _
		| _, (`named (_, _, `generic_type, _) as g_t) ->
			Some g_t, derived_types
		| _ ->
			None, derived_types
		end
	);;
	
	let rec real_type_of (t: all_type) (predefined_types: predefined_types): all_type option = (
		begin match t with
		| #int_prec | #real_prec | `char | `bool as t ->
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
	
	let rec dereference (t: all_type): all_type option = (
		begin match resolve_typedef t with
		| `pointer t ->
			Some t
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
	
	let find_enum_by_element
		(element: enum_item)
		(namespace: namespace)
		(predefined_types: predefined_types)
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
	
	let inference_by_field
		(accessing: string list)
		(source: source_item list)
		: [`defined_element_access of struct_or_union_type * struct_item list
			| `error of string] =
	(
		(* search struct having the field of same name *)
		let rec source_loop ss = (
			begin match ss with
			| s :: sr ->
				begin match s with
				| `anonymous (_, `struct_type (_, items))
				| `anonymous (_, `union items)
				| `named (_, _, `struct_type (_, items), _)
				| `named (_, _, `union items, _) as t ->
					let rec item_loop root_t items accessing route = (
						begin match find_field (List.hd accessing) items with
						| Some ((field_name, field_type, _, _) as field) ->
							let route = field :: route in
							begin match List.tl accessing with
							| _ :: _ as ac_r ->
								begin match field_type with
								| `anonymous (_, `struct_type (_, items))
								| `anonymous (_, `union items)
								| `named (_, _, `struct_type (_, items), _)
								| `named (_, _, `union items, _) ->
									item_loop root_t items ac_r route
								| _ ->
									`error (field_name ^ " is not struct or union.")
								end
							| [] ->
								`defined_element_access (root_t, List.rev route)
							end
						| None ->
							source_loop sr
						end
					) in
					item_loop t items accessing []
				| _ ->
					source_loop sr
				end
			| [] ->
				`error ("any struct or union having " ^ List.hd accessing ^ " was not found.");
			end
		) in
		source_loop source
	);;
	
	let rec handle_pragma
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(info: extra_info)
		(alignment_stack: alignment list)
		(mapping_options: mapping_options)
		(x: Syntax.pragma p)
		: derived_types * extra_info * alignment list * mapping_options =
	(
		let ps, (_, directive) = x in
		begin match directive with
		| `some directive ->
			begin match snd directive with
			| `gcc (_, gcc_directive) ->
				begin match gcc_directive with
				| `some gcc_directive ->
					begin match snd gcc_directive with
					| `fenv ->
						let info = {info with ei_fenv = true} in
						derived_types, info, alignment_stack, mapping_options
					| `poison _ ->
						error (fst gcc_directive) "unimplemented!";
						assert false
					| `visibility _ ->
						(* ignore #pragma visibility *)
						derived_types, info, alignment_stack, mapping_options
					| `system_header ->
						let info = {info with ei_system_header = true} in
						derived_types, info, alignment_stack, mapping_options
					end
				| `error ->
					derived_types, info, alignment_stack, mapping_options
				end
			| `instance (_, sql, decl) ->
				begin match sql, decl with
				| `some sql, `some decl ->
					let derived_types, n2, s2, base_type = handle_specifier_qualifier_list error predefined_types derived_types namespace [] (List.hd alignment_stack) sql in
					let derived_types, n2, s2, decl = handle_declarator error predefined_types derived_types n2 s2 base_type no_attributes decl in
					begin match decl with
					| Some (_, name, t, attrs) ->
						if n2 != namespace || (
							match s2 with
							| [] -> false
							| st :: [] when (st :> all_item) == (t :> all_item) -> false
							| _ -> true)
						then (
							error (fst directive) "new type-specifier was found in pragma."
						);
						if attrs <> no_attributes then (
							error (fst directive) "attributes are ignored in pragma instance."
						);
						let mapping_options = {mapping_options with mo_instances =
							StringMap.modify (fun rs ->
								let error =
									List.exists (fun i ->
										begin match t, i with
										| `function_type p1, `function_type p2 ->
											prototype_ABI_compatibility ~dest:p1 ~source:p2 = `just (* same prototype *)
										| _ ->
											error (fst directive) "this type could not be overloaded.";
											true
										end
									) rs
								in
								if error then rs else t :: rs
							) ~default:[] name mapping_options.mo_instances}
						in
						derived_types, info, alignment_stack, mapping_options
					| None ->
						derived_types, info, alignment_stack, mapping_options
					end
				| _ ->
					derived_types, info, alignment_stack, mapping_options
				end
			| `pack (_, _, arg, _) ->
				let push (n: Integer.t) alignment_stack = (
					begin match Integer.to_int n with
					| 1 ->
						`packed :: alignment_stack
					| n ->
						`aligned n :: alignment_stack
					end
				) in
				let pop alignment_stack = (
					begin match alignment_stack with
					| _ :: [] | [] ->
						error ps ("too many #pragma pack(pop).");
						alignment_stack
					| _ :: xr ->
						xr
					end
				) in
				begin match arg with
				| `some arg ->
					begin match snd arg with
					| `push (_, _, n) ->
						begin match n with
						| `some n ->
							let alignment_stack = push (snd n) alignment_stack in
							derived_types, info, alignment_stack, mapping_options
						| `error ->
							derived_types, info, alignment_stack, mapping_options
						end
					| `pop ->
						let alignment_stack = pop alignment_stack in
						derived_types, info, alignment_stack, mapping_options
					| `set n ->
						let alignment_stack = push n (List.tl alignment_stack) in
						derived_types, info, alignment_stack, mapping_options
					end
				| `none ->
					(* set default *)
					let alignment_stack = `default :: (List.tl alignment_stack) in
					derived_types, info, alignment_stack, mapping_options
				end
			| `language_mapping (_, lang, mapping) ->
				begin match lang, mapping with
				| `some lang, `some mapping ->
					let lang = String.uppercase (snd lang) in
					begin match snd mapping with
					| `type_mapping (_, typename, _, repr) ->
						begin match typename, repr with
						| `some typename, `some (_, `chars_literal repr) ->
							let derived_types, s2, t = handle_type_name error predefined_types derived_types namespace [] typename in
							if s2 <> [] then (
								error (fst typename) "new type-specifier was found in pragma."
							);
							let mapping_options = {mapping_options with mo_language_mappings =
								StringMap.modify (fun x ->
									{x with lm_type = (t, repr) :: x.lm_type}
								) ~default:no_language_mapping lang mapping_options.mo_language_mappings}
							in
							derived_types, info, alignment_stack, mapping_options
						| _ ->
							derived_types, info, alignment_stack, mapping_options
						end
					| `overload (_, sql, decl) ->
						begin match sql, decl with
						| `some sql, `some decl ->
							let derived_types, n2, s2, base_type = handle_specifier_qualifier_list error predefined_types derived_types namespace [] (List.hd alignment_stack) sql in
							let derived_types, n2, s2, decl = handle_declarator error predefined_types derived_types n2 s2 base_type no_attributes decl in
							begin match decl with
							| Some (_, name, t, attrs) ->
								if n2 != namespace || (
									match s2 with
									| s :: [] when (s :> all_item) == (t :> all_item) -> false
									| _ -> true)
								then (
									error (fst mapping) "new type-specifier was found in pragma."
								);
								if attrs <> no_attributes then (
									error (fst mapping) "attributes are ignored in pragma overload."
								);
								begin match t with
								| `function_type prototype ->
									begin try
										begin match StringMap.find name namespace.ns_namespace with
										| `named (_, _, `extern ((#function_type as original_t), _), _)
										| `named (_, _, `function_forward (_, original_t), _)
										| `named (_, _, `function_definition (_, original_t, _), _) as func ->
											let `function_type original_prototype = original_t in
											begin match prototype_ABI_compatibility ~dest:prototype ~source:original_prototype with
											| `compatible ->
												let mapping_options = {mapping_options with mo_language_mappings =
													StringMap.modify (fun x ->
														let overload =
															begin try
																let e = List.assq func x.lm_overload in
																let e = prototype :: e in
																(func, e) :: List.remove_assq func x.lm_overload
															with Not_found ->
																(func, [prototype]) :: x.lm_overload
															end
														in
														{x with lm_overload = overload}
													) ~default:no_language_mapping lang mapping_options.mo_language_mappings}
												in
												derived_types, info, alignment_stack, mapping_options
											| `error ->
												error (fst mapping) "this overload has no ABI compatibility with original function.";
												derived_types, info, alignment_stack, mapping_options
											| `just ->
												error (fst mapping) "this overload has same prototype of original function.";
												derived_types, info, alignment_stack, mapping_options
											end
										| _ ->
											error (fst mapping) ("\"" ^ name ^ "\" is not function.");
											derived_types, info, alignment_stack, mapping_options
										end
									with Not_found ->
										error (fst mapping) ("\"" ^ name ^ "\" was not found.");
										derived_types, info, alignment_stack, mapping_options
									end
								| _ ->
									error (fst mapping) ("overloading \"" ^ name ^ "\" is not function.");
									derived_types, info, alignment_stack, mapping_options
								end
							| None ->
								derived_types, info, alignment_stack, mapping_options
							end
						| _ ->
							derived_types, info, alignment_stack, mapping_options
						end
					| `includes ((_, `chars_literal file1), _, file2) ->
						begin match file2 with
						| `some (_, `chars_literal file2) ->
							let mapping_options = {mapping_options with mo_language_mappings =
								StringMap.modify (fun x ->
									{x with lm_include = (file1, file2) :: x.lm_include}
								) ~default:no_language_mapping lang mapping_options.mo_language_mappings}
							in
							derived_types, info, alignment_stack, mapping_options
						| `error ->
							derived_types, info, alignment_stack, mapping_options
						end
					end
				| _ ->
					derived_types, info, alignment_stack, mapping_options
				end
			end
		| `error ->
			derived_types, info, alignment_stack, mapping_options
		end
	) and handle_attribute
		(error: ranged_position -> string -> unit)
		(attributes: attributes)
		(x: Syntax.attribute p)
		: attributes =
	(
		let _, (_, _, _, list, _, _) = x in
		begin match list with
		| `some list ->
			Traversing.fold_ail (handle_attribute_item error) attributes list
		| `error ->
			attributes
		end
	) and handle_attribute_item
		(error: ranged_position -> string -> unit)
		(attributes: attributes)
		(x: Syntax.attribute_item p)
		: attributes =
	(
		begin match snd x with
		| `aligned (_, param) ->
			begin match param with
			| `none ->
				{attributes with at_aligned = `explicit_aligned}
			| `some (_, (_, (_, n), _)) ->
				{attributes with at_aligned = `aligned (Integer.to_int n)}
			end
		| `alloc_size (_, _, list, _) ->
			begin match list with
			| `some list ->
				let rec loop (list: Syntax.argument_expression_list p) rs = (
					let int_of_expr ps (expr: Syntax.assignment_expression) rs = (
						begin match expr with
						| `int_literal (_, value) ->
							Integer.to_int value :: rs
						| _ ->
							error ps "complex expression in __attributes__((alloc_size(...))) is not supported.";
							rs
						end
					) in
					begin match snd list with
					| `nil expr ->
						{attributes with at_alloc_size = int_of_expr (fst list) expr rs}
					| `cons (list, _, expr) ->
						loop list (match expr with
							| `some expr -> int_of_expr (fst expr) (snd expr) rs
							| `error -> rs)
					end
				) in
				loop list []
			| `error ->
				attributes
			end
		| `always_inline _ ->
			{attributes with at_inline = `always_inline}
		| `cdecl _ ->
			{attributes with at_conventions = `cdecl}
		| `const _ ->
			{attributes with at_const = true}
		| `deprecated _ ->
			{attributes with at_deprecated = true}
		| `dllimport _ ->
			{attributes with at_dllimport = true}
		| `dllexport _ ->
			{attributes with at_dllexport = true}
		| `fastcall ->
			{attributes with at_conventions = `fastcall}
		| `format (_, _, (_, `ident id), _, (_, m), _, (_, n), _) ->
			{attributes with at_format = `like (id, Integer.to_int m, Integer.to_int n)}
		| `format_arg (_, _, (_, n), _) ->
			{attributes with at_format = `arg (Integer.to_int n)}
		| `inline _ ->
			if attributes.at_inline = `always_inline then attributes else
			{attributes with at_inline = `inline}
		| `malloc ->
			{attributes with at_malloc = true}
		| `mode (_, _, (_, m), _) ->
			{attributes with at_mode = Some m}
		| `noinline ->
			{attributes with at_inline = `noinline}
		| `nonnull (_, _, list, _) ->
			begin match list with
			| `some list ->
				let rec loop (list: Syntax.argument_expression_list p) rs = (
					let int_of_expr ps (expr: Syntax.assignment_expression) rs = (
						begin match expr with
						| `int_literal (_, value) ->
							Integer.to_int value :: rs
						| _ ->
							error ps "complex expression in __attributes__((nonnull(...))) is not supported.";
							rs
						end
					) in
					begin match snd list with
					| `nil expr ->
						{attributes with at_nonnull = int_of_expr (fst list) expr rs}
					| `cons (list, _, expr) ->
						loop list (match expr with
							| `some expr -> int_of_expr (fst expr) (snd expr) rs
							| `error -> rs)
					end
				) in
				loop list []
			| `error ->
				attributes
			end
		| `noreturn _ ->
			{attributes with at_noreturn = true}
		| `nothrow ->
			{attributes with at_nothrow = true}
		| `optimize (_, _, arg, _) ->
			begin match arg with
			| `some (_, `chars_literal arg) ->
				{attributes with at_optimize = Some arg}
			| `error ->
				attributes
			end
		| `packed _ ->
			{attributes with at_aligned = `packed}
		| `pure ->
			{attributes with at_pure = true}
		| `returns_twice ->
			{attributes with at_returns_twice = true}
		| `sentinel ->
			{attributes with at_sentinel = true}
		| `selectany ->
			{attributes with at_selectany = true}
		| `stdcall ->
			{attributes with at_conventions = `stdcall}
		| `thiscall ->
			{attributes with at_conventions = `thiscall}
		| `unavailable ->
			{attributes with at_unavailable = true}
		| `unused _ ->
			{attributes with at_used = `unused}
		| `used ->
			{attributes with at_used = `used}
		| `warn_unused_result ->
			{attributes with at_warn_unused_result = true}
		| `weak_import ->
			{attributes with at_weak_import = true}
		end
	) and handle_expression
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(request: [`lvalue | `rvalue])
		(x: Syntax.expression p)
		: derived_types * source_item list * expression option =
	(
		let handle_field
			(dereferencing: bool)
			(var: Syntax.expression p)
			(var_request: [`lvalue | `rvalue])
			(field: Syntax.identifier pe)
			: derived_types * source_item list * expression option =
		(
			let derived_types, source, var = handle_expression error predefined_types derived_types namespace source var_request var in
			begin match var, field with
			| Some (`ref_object ((`named (_, _, `generic_value _, _)), _), _), _ ->
				error (fst x) "currently, macro argument could not be inferenced as struct or union.";
				derived_types, source, None
			| Some var, `some (_, `ident field_name) ->
				let t = snd var in
				(* dereferencing *)
				let resolved_t, var = (
					if dereferencing then (
						begin match dereference t with
						| Some target_t ->
							let target_t = resolve_typedef target_t in
							target_t, (`dereference var, target_t)
						| None ->
							error (fst x) "this expression is not a pointer type.";
							t, var
						end
					) else (
						(resolve_typedef t), var
					)
				) in
				(* remove const *)
				let resolved_t =
					begin match resolved_t with
					| `const t -> resolve_typedef (t :> all_type)
					| _ -> resolved_t
					end
				in
				(* element access *)
				begin match resolved_t with
				| `anonymous (_, `struct_type (_, items))
				| `anonymous (_, `union items)
				| `named (_, _, `struct_type (_, items), _)
				| `named (_, _, `union items, _) ->
					begin match find_field field_name items with
					| Some (_, field_t, _, _ as field) ->
						derived_types, source, Some (`element_access (var, field), field_t)
					| None ->
						error (fst x) ("field \"" ^ field_name ^ "\" does not exist.");
						derived_types, source, None
					end
				| _ ->
					error (fst x) "this expression is not a struct or union type.";
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		) in
		let handle_sizeof derived_types source t: derived_types * source_item list * expression option = (
			let size_t = find_size_t predefined_types in
			begin match t with
			| `named (_, formal_type, `generic_type, _) ->
				derived_types, source, Some (`sizeof_formal_type formal_type, size_t)
			| _ ->
				begin match Machine.sizeof t predefined_types with
				| Some sizeof_value ->
					derived_types, source, Some (`int_literal (`unsigned_int, Integer.of_int sizeof_value), size_t)
				| None ->
					error (fst x) "sizeof(opaque) is invalid.";
					derived_types, source, None
				end
			end
		) in
		let handle_unary (f: derived_types -> expression -> derived_types * expression option) request (right: Syntax.expression pe): derived_types * source_item list * expression option = (
			begin match right with
			| `some right ->
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source request right in
				begin match right with
				| Some right ->
					let derived_types, result = f derived_types right in
					derived_types, source, result
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		) in
		let handle_unary_folding (f: derived_types -> expression -> derived_types * expression option) (int_f: Integer.t -> Integer.t) (right: Syntax.expression pe): derived_types * source_item list * expression option = (
			handle_unary (fun derived_types right ->
				begin match integer_of_expression right with
				| Some (prec, right) ->
					let result = int_f right in
					derived_types, Some (`int_literal (prec, result), find_predefined_type prec predefined_types)
				| None ->
					f derived_types right
				end
			) `rvalue right
		) in
		let handle_unary_bool (f: expression -> expression_var option) (int_f: Integer.t -> bool) (right: Syntax.expression pe): derived_types * source_item list * expression option = (
			handle_unary (fun derived_types right ->
				let bool_type = find_predefined_type `bool predefined_types in
				begin match integer_of_expression right with
				| Some (_, right) ->
					let result = if int_f right then Integer.one else Integer.zero in
					derived_types, Some (`int_literal (`unsigned_char, result), bool_type)
				| None ->
					begin match f right with
					| Some x -> derived_types, Some (x, bool_type)
					| None -> derived_types, None
					end
				end
			) `rvalue right
		) in
		let handle_binary (f: derived_types -> expression -> expression -> derived_types * expression option) (left: Syntax.expression p) (right: Syntax.expression pe): derived_types * source_item list * expression option = (
			begin match right with
			| `some right ->
				let derived_types, source, left = handle_expression error predefined_types derived_types namespace source `rvalue left in
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source `rvalue right in
				begin match left, right with
				| Some left, Some right ->
					let derived_types, result = f derived_types left right in
					derived_types, source, result
				| _ ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		) in
		let handle_binary_folding op (f: expression -> expression -> expression_var option) (int_f: Integer.t -> Integer.t -> Integer.t) (left: Syntax.expression p) (right: Syntax.expression pe): derived_types * source_item list * expression option = (
			handle_binary (fun derived_types left right ->
				begin match (integer_of_expression left), (integer_of_expression right) with
				| Some (prec1, left), Some (prec2, right) ->
					let prec =
						begin match op with
						| `bit | `conditional when prec1 = prec2 ->
							prec1
						| _ ->
							int_prec prec1 prec2
						end
					in
					let result = int_f left right in
					derived_types, Some (`int_literal (prec, result), find_predefined_type prec predefined_types)
				| _ ->
					begin match result_type_of op (snd left) (snd right) predefined_types derived_types with
					| Some result_type, derived_types ->
						begin match f (conv_expr result_type left) (conv_expr result_type right) with
						| Some x ->
							derived_types, Some (x, result_type)
						| None ->
							derived_types, None
						end
					| None, derived_types ->
						error (fst x) "type mismatch for binary-operator.";
						derived_types, None
					end
				end
			) left right
		) in
		let handle_compare (f: expression -> expression -> expression_var option) (int_f: Integer.t -> Integer.t -> bool) (left: Syntax.expression p) (right: Syntax.expression pe): derived_types * source_item list * expression option = (
			handle_binary (fun derived_types left right ->
				let bool_type = find_predefined_type `bool predefined_types in
				begin match (integer_of_expression left), (integer_of_expression right) with
				| Some (_, left), Some (_, right) ->
					let result = if int_f left right then Integer.one else Integer.zero in
					derived_types, Some (`int_literal (`unsigned_char, result), bool_type)
				| _ ->
					begin match f left right with
					| Some x -> derived_types, Some (x, bool_type)
					| None -> derived_types, None
					end
				end
			) left right
		) in
		let handle_shift (f: expression -> expression -> expression_var option) (int_f: Integer.t -> int -> Integer.t) (left: Syntax.expression p) (right: Syntax.expression pe): derived_types * source_item list * expression option = (
			handle_binary (fun derived_types left right ->
				begin match (integer_of_expression left), (integer_of_expression right) with
				| Some (prec, left), Some (_, right) ->
					let result = int_f left (Integer.to_int right) in
					derived_types, Some (`int_literal (prec, result), find_predefined_type prec predefined_types)
				| _ ->
					begin match f left right with
					| Some x -> derived_types, Some (x, snd left)
					| None -> derived_types, None
					end
				end
			) left right
		) in
		begin match snd x with
		| `int_literal (prec, _) as v ->
			derived_types, source, Some (v, find_predefined_type prec predefined_types)
		| `float_literal (prec, _) as v ->
			derived_types, source, Some (v, find_predefined_type prec predefined_types)
		| `imaginary_literal (prec, _) as v ->
			derived_types, source, Some (v, find_predefined_type (`imaginary prec) predefined_types)
		| `char_literal _ as v ->
			derived_types, source, Some (v, find_predefined_type `char predefined_types)
		| `chars_literal s as v ->
			let base_type = find_predefined_type `char predefined_types in
			let length = Some (Integer.of_int (String.length s + 1)) in
			let t, derived_types = find_array_type length base_type derived_types in
			derived_types, source, Some (v, t)
		| `wchar_literal _ as v ->
			derived_types, source, Some (v, find_wchar_t predefined_types)
		| `wchars_literal s as v ->
			let base_type = find_wchar_t predefined_types in
			let length = Some (Integer.of_int (WideString.length s + 1)) in
			let t, derived_types = find_array_type length base_type derived_types in
			derived_types, source, Some (v, t)
		| `objc_string_literal _ ->
			error (fst x) "unimplemented!";
			assert false
		| `ident name ->
			begin try
				begin match StringMap.find name namespace.ns_namespace with
				| `named (_, _, `enum_element _, _) as item ->
					let t = find_enum_by_element item namespace predefined_types in
					derived_types, source, Some (`enumerator item, t)
				| `named (_, _, `function_forward (_, t), _) as item ->
					derived_types, source, Some (`ref_function item, (t :> all_type))
				| `named (_, _, `function_definition (_, t, _), _) as item ->
					derived_types, source, Some (`ref_function item, (t :> all_type))
				| `named (_, _, `extern ((#function_type as t), _), _) as item ->
					derived_types, source, Some (`ref_function item, t)
				| `named (_, _, `extern (t, _), _) as item ->
					derived_types, source, Some (`ref_object (item, request), t)
				| `named (_, _, `variable (t, _), _) as item ->
					derived_types, source, Some (`ref_object (item, request), t)
				| `named (_, _, `generic_value t, _) as item ->
					derived_types, source, Some (`ref_object (item, request), t)
				| _ ->
					error (fst x) "unimplemented!";
					assert false
				end
			with Not_found ->
				if name = "__func__" then (
					let base_type = find_predefined_type `char predefined_types in
					let t, derived_types = find_array_type None base_type derived_types in
					derived_types, source, Some (`__func__, t)
				) else (
					error (fst x) (name ^ " was undefined.");
					derived_types, source, None
				)
			end
		| `__FILE__ _ ->
			let base_type = find_predefined_type `char predefined_types in
			let t, derived_types = find_array_type None base_type derived_types in
			derived_types, source, Some (`__FILE__, t)
		| `__LINE__ _ ->
			derived_types, source, Some (`__LINE__, find_predefined_type `signed_int predefined_types)
		| `paren (_, e, _) ->
			begin match e with
			| `some e ->
				handle_expression error predefined_types derived_types namespace source request e
			| `error ->
				derived_types, source, None
			end
		| `statement (_, compound, _) ->
			let derived_types, source, compound = handle_compound_statement error predefined_types derived_types namespace source `default compound in
			let rec loop xs = (
				begin match xs with
				| stmt :: [] ->
					begin match stmt with
					| `expression (_, t) ->
						t
					| `local (_, stmts) ->
						loop stmts
					| _ ->
						error (fst x) "last of statement expression is not an expression.";
						find_predefined_type `void predefined_types (* should it be error? *)
					end
				| _ :: xr ->
					loop xr
				| [] ->
					error (fst x) "statement expression is empty.";
					find_predefined_type `void predefined_types
				end
			) in
			let t = loop compound in
			derived_types, source, Some (`statement compound, t)
		| `array_access (left, _, right, _) ->
			begin match right with
			| `some right ->
				let derived_types, source, left = handle_expression error predefined_types derived_types namespace source `rvalue left in
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source `rvalue right in
				begin match left, right with
				| Some left, Some right ->
					(* treat a[i] as *(a + i) *)
					begin match result_type_of `add (snd left) (snd right) predefined_types derived_types with
					| Some added_type, derived_types ->
						begin match dereference added_type with
						| Some t ->
							derived_types, source, Some (`dereference (`add (left, right), added_type), t)
						| None ->
							error (fst x) "type mismatch to dereference of array-access.";
							derived_types, source, None
						end
					| None, derived_types ->
						error (fst x) "type mismatch to add of array-access.";
						derived_types, source, None
					end
				| _ ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `function_call (f, _, args, _) ->
			let derived_types, source, f = handle_expression error predefined_types derived_types namespace source `rvalue f in
			begin match f with
			| Some f ->
				let derived_types, source, args =
					Traversing.opt Traversing.fold_ael (fun (derived_types, source, rs) arg ->
						begin match rs with
						| Some rs ->
							let derived_types, source, arg = handle_expression error predefined_types derived_types namespace source `rvalue arg in
							begin match arg with
							| Some arg ->
								derived_types, source, Some (arg :: rs)
							| None ->
								derived_types, source, None
							end
						| None ->
							derived_types, source, None
						end
					) (derived_types, source, Some []) args
				in
				begin match args with
				| Some args ->
					let args = List.rev args in
					(* type check *)
					let function_type = resolve_typedef (snd f) in
					begin match function_type with
					| `function_type (_, args_of_prototype, _, result_type) ->
						if List.length args <> List.length args_of_prototype then (
							error (fst x) "a number of arguments was mismatch.";
							derived_types, source, None
						) else (
							let args =
								List.fold_left2 (fun rs farg rarg ->
									let `named (_, _, `variable (farg_t, _), _) = farg in
									(conv_expr farg_t rarg) :: rs
								) [] args_of_prototype args
							in
							let args = List.rev args in
							derived_types, source, Some (`function_call (f, args), result_type)
						)
					| _ ->
						error (fst x) "this is not function.";
						derived_types, source, None
					end
				| None ->
					derived_types, source, None
				end
			| None ->
				derived_types, source, None
			end
		| `__builtin_constant_p _ ->
			(* currently, always false...todo *)
			derived_types, source, Some (`int_literal (`unsigned_char, Integer.zero), find_predefined_type `bool predefined_types)
		| `__builtin_va_arg (_, _, expr, _, typename, _) ->
			begin match expr, typename with
			| `some expr, `some typename ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `lvalue expr in
				let derived_types, source, t = handle_type_name error predefined_types derived_types namespace source typename in
				begin match expr with
				| Some expr ->
					derived_types, source, Some (`va_arg expr, t)
				| None ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `__builtin_compare ((_, func), _, left, _, right, _) ->
			begin match left with
			| `some left ->
				begin match func with
				| `__builtin_isgreater ->
					let int_gt left right = (Integer.compare left right > 0) in
					handle_compare (fun left right -> Some (`gt (left, right))) int_gt left right
				| `__builtin_isgreaterequal ->
					let int_ge left right = (Integer.compare left right >= 0) in
					handle_compare (fun left right -> Some (`ge (left, right))) int_ge left right
				| `__builtin_isless ->
					let int_lt left right = (Integer.compare left right < 0) in
					handle_compare (fun left right -> Some (`lt (left, right))) int_lt left right
				| `__builtin_islessequal ->
					let int_le left right = (Integer.compare left right <= 0) in
					handle_compare (fun left right -> Some (`le (left, right))) int_le left right
				| `__builtin_islessgreater ->
					let int_ne left right = (Integer.compare left right <> 0) in
					handle_compare (fun left right -> Some (`ne (left, right))) int_ne left right
				| `__builtin_isunordered ->
					let int_uo _ _ = false in (* integer is always orderd *)
					handle_compare (fun left right -> Some (`uo (left, right))) int_uo left right
				end
			| `error ->
				derived_types, source, None
			end
		| `element_access (var, _, field) ->
			handle_field false var request field
		| `dereferencing_element_access (var, _, field) ->
			handle_field true var `rvalue field
		| `post_increment (left, _) ->
			handle_unary (fun derived_types left ->
				derived_types, Some (`post_increment left, snd left)
			) `lvalue (`some left)
		| `post_decrement (left, _) ->
			handle_unary (fun derived_types left ->
				derived_types, Some (`post_decrement left, snd left)
			) `lvalue (`some left)
		| `compound (_, typename, _, _, list, _) ->
			let derived_types, source, t = handle_type_name error predefined_types derived_types namespace source typename in
			begin match list with
			| `some list ->
				handle_initializer_list error predefined_types derived_types namespace source t list
			| `error ->
				derived_types, source, None
			end
		| `increment (_, right) ->
			handle_unary (fun derived_types right ->
				derived_types, Some (`increment right, snd right)
			) `lvalue right
		| `decrement (_, right) ->
			handle_unary (fun derived_types right ->
				derived_types, Some (`decrement right, snd right)
			) `lvalue right
		| `unary ((_, `ampersand), right) ->
			handle_unary (fun derived_types right ->
				let t, derived_types = find_pointer_type (snd right) derived_types in
				derived_types, Some (`address right, t)
			) `lvalue right
		| `unary ((_, `asterisk), right) ->
			handle_unary (fun derived_types right ->
				begin match dereference (snd right) with
				| Some t ->
					derived_types, Some (`dereference right, t)
				| None ->
					error (fst x) "this expression is not a pointer.";
					derived_types, None
				end
			) `rvalue right
		| `unary ((_, `plus), right) ->
			let int_pos x = x in
			handle_unary_folding (fun derived_types right ->
				derived_types, Some right
			) int_pos right
		| `unary ((_, `minus), right) ->
			handle_unary_folding (fun derived_types right ->
				derived_types, Some (`neg right, snd right)
			) Integer.neg right
		| `unary ((_, `tilde), right) ->
			handle_unary_folding (fun derived_types right ->
				derived_types, Some (`bit_not right, snd right)
			) Integer.lognot right
		| `unary ((_, `exclamation), right) ->
			begin match right with
			| `some (_, `unary ((_, `exclamation), (`some right))) -> (* folding !!expr to (bool)expr *)
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source `rvalue right in
				begin match right with
				| Some right ->
					derived_types, source, Some (conv_expr `bool right)
				| None ->
					derived_types, source, None
				end
			| _ ->
				let int_not x = (Integer.compare x Integer.zero = 0) in
				handle_unary_bool (fun right -> Some (`not (conv_expr `bool right))) int_not right
			end
		| `sizeof_expr (_, expr) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					handle_sizeof derived_types source (snd expr)
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `sizeof_type (_, _, typename, _) ->
			let derived_types, source, t = handle_type_name error predefined_types derived_types namespace source typename in
			handle_sizeof derived_types source t
		| `extension (_, right) ->
			handle_unary (fun derived_types right ->
				derived_types, Some right
			) request right
		| `real (_, right) ->
			handle_unary (fun derived_types right ->
				begin match real_type_of (snd right) predefined_types with
				| Some t ->
					derived_types, Some (`real right, t)
				| None ->
					error (fst x) "type mismatch for complex.";
					derived_types, None
				end
			) `rvalue right
		| `imag (_, right) ->
			handle_unary (fun derived_types right ->
				(* __imag__ right return imaginary part of right with base floating type (not _Imaginary type) *)
				begin match real_type_of (snd right) predefined_types with
				| Some t ->
					derived_types, Some (`imag right, t)
				| None ->
					error (fst x) "type mismatch for complex.";
					derived_types, None
				end
			) `rvalue right
		| `cast (_, typename, _, expr) ->
			let derived_types, source, t = handle_type_name error predefined_types derived_types namespace source typename in
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					let resolved_t = resolve_typedef t in
					begin match resolved_t, integer_of_expression expr with
					| (#int_prec as t), Some (_, expr) ->
						derived_types, source, Some (integer_cast predefined_types t expr)
					| _ ->
						begin match resolved_t, resolve_typedef (snd expr) with
						| #predefined_numeric_type, #predefined_numeric_type ->
							derived_types, source, Some (`explicit_conv expr, t)
						| _ ->
							derived_types, source, Some (`cast expr, t)
						end
					end
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `mul (left, _, right) ->
			handle_binary_folding `multiplicative (fun left right -> Some (`mul (left, right))) Integer.mul left right
		| `div (left, _, right) ->
			handle_binary_folding `multiplicative (fun left right -> Some (`div (left, right))) Integer.div left right
		| `rem (left, _, right) ->
			handle_binary_folding `multiplicative (fun left right -> Some (`rem (left, right))) Integer.rem left right
		| `add (left, _, right) ->
			handle_binary_folding `add (fun left right -> Some (`add (left, right))) Integer.add left right
		| `sub (left, _, right) ->
			handle_binary_folding `sub (fun left right -> Some (`sub (left, right))) Integer.sub left right
		| `l_shift (left, _, right) ->
			handle_shift (fun left right -> Some (`l_shift (left, right))) Integer.shift_left left right
		| `r_shift (left, _, right) ->
			handle_shift (fun left right -> Some (`r_shift (left, right))) Integer.shift_right left right
		| `lt (left, _, right) ->
			let int_lt left right = (Integer.compare left right < 0) in
			handle_compare (fun left right -> Some (`lt (left, right))) int_lt left right
		| `gt (left, _, right) ->
			let int_gt left right = (Integer.compare left right > 0) in
			handle_compare (fun left right -> Some (`gt (left, right))) int_gt left right
		| `le (left, _, right) ->
			let int_le left right = (Integer.compare left right <= 0) in
			handle_compare (fun left right -> Some (`le (left, right))) int_le left right
		| `ge (left, _, right) ->
			let int_ge left right = (Integer.compare left right >= 0) in
			handle_compare (fun left right -> Some (`ge (left, right))) int_ge left right
		| `eq (left, _, right) ->
			let int_eq left right = (Integer.compare left right = 0) in
			handle_compare (fun left right -> Some (`eq (left, right))) int_eq left right
		| `ne (left, _, right) ->
			let int_ne left right = (Integer.compare left right <> 0) in
			handle_compare (fun left right -> Some (`ne (left, right))) int_ne left right
		| `bit_and (left, _, right) ->
			handle_binary_folding `bit (fun left right -> Some (`bit_and (left, right))) Integer.logand left right
		| `bit_xor (left, _, right) ->
			handle_binary_folding `bit (fun left right -> Some (`bit_xor (left, right))) Integer.logxor left right
		| `bit_or (left, _, right) ->
			handle_binary_folding `bit (fun left right -> Some (`bit_or (left, right))) Integer.logor left right
		| `and_then (left, _, right) ->
			let int_and_then left right = (Integer.compare left Integer.zero <> 0 && Integer.compare right Integer.zero <> 0) in
			handle_compare (fun left right -> Some (`and_then (left, right))) int_and_then left right
		| `or_else (left, _, right) ->
			let int_or_else left right = (Integer.compare left Integer.zero <> 0 || Integer.compare right Integer.zero <> 0) in
			handle_compare (fun left right -> Some (`or_else (left, right))) int_or_else left right
		| `cond (cond, _, true_case, _, false_case) ->
			begin match true_case, false_case with
			| `some true_case, `some false_case ->
				let derived_types, source, cond = handle_expression error predefined_types derived_types namespace source `rvalue cond in
				let derived_types, source, true_case = handle_expression error predefined_types derived_types namespace source `rvalue true_case in
				let derived_types, source, false_case = handle_expression error predefined_types derived_types namespace source `rvalue false_case in
				begin match cond, true_case, false_case with
				| Some cond, Some true_case, Some false_case ->
					begin match (integer_of_expression cond), (integer_of_expression true_case), (integer_of_expression false_case) with
					| Some (_, cond), Some (prec1, true_case), Some (prec2, false_case) ->
						let prec = int_prec prec1 prec2 in
						let result = if Integer.compare cond Integer.zero <> 0 then true_case else false_case in
						derived_types, source, Some (`int_literal (prec, result), find_predefined_type prec predefined_types)
					| _ ->
						begin match result_type_of `conditional (snd true_case) (snd false_case) predefined_types derived_types with
						| Some t, derived_types ->
							let cond = conv_expr `bool cond in
							let true_case = conv_expr t true_case in
							let false_case = conv_expr t false_case in
							derived_types, source, Some (`cond (cond, true_case, false_case), t)
						| None, derived_types ->
							error (fst x) "type mismatch for cases of conditional-operator";
							derived_types, source, None
						end
					end
				| _ ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `assign (left, op, right) ->
			begin match right with
			| `some right ->
				let derived_types, source, left = handle_expression error predefined_types derived_types namespace source `lvalue left in
				let derived_types, source, right = handle_expression error predefined_types derived_types namespace source `rvalue right in
				begin match left, right with
				| Some left, Some right ->
					let left_t = snd left in
					let right = conv_expr left_t right in
					derived_types, source, Some (`assign (left, snd op, right), left_t)
				| _ ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `comma (left, _, right) ->
			handle_binary (fun derived_types left right ->
				begin match (integer_of_expression left), (integer_of_expression right) with
				| Some _, Some (prec, right) ->
					derived_types, Some (`int_literal (prec, right), find_predefined_type prec predefined_types)
				| _ ->
					derived_types, Some (`comma (left, right), snd right)
				end
			) left right
		end
	) and handle_declaration
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.declaration p)
		: derived_types * namespace * source_item list * named_item list =
	(
		let _, (spec, idecls, _) = x in
		let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
		let derived_types, namespace, source, result =
			Traversing.opt Traversing.fold_idl (fun (derived_types, namespace, source, rs as result) (idecl: Syntax.init_declarator p) ->
				let derived_types, namespace, source, item = handle_init_declarator error predefined_types derived_types namespace source storage_class base_type attributes `none idecl in
				begin match item with
				| Some item -> derived_types, namespace, source, (item :: rs)
				| None -> result
				end
			) (derived_types, namespace, source, []) idecls
		in
		derived_types, namespace, source, List.rev result
	) and handle_declaration_specifiers
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.declaration_specifiers p)
		: derived_types * namespace * source_item list * ([storage_class | `none] * all_type * attributes) =
	(
		let rec extract (derived_types, namespace, source, attributes, storage_class, type_specs, qualifiers) spec = (
			let result, next =
				begin match snd spec with
				| `storage_class_specifier (sc, next) ->
					(derived_types, namespace, source, attributes, (handle_storage_class error storage_class sc), type_specs, qualifiers), next
				| `type_specifier (ts, next) ->
					let derived_types, namespace, source, type_specs = handle_type_specifier error predefined_types derived_types namespace source attributes.at_aligned ts type_specs in
					(derived_types, namespace, source, attributes, storage_class, type_specs, qualifiers), next
				| `type_qualifier (q, next) ->
					let qualifiers = handle_type_qualifier error qualifiers q in
					(derived_types, namespace, source, attributes, storage_class, type_specs, qualifiers), next
				| `function_specifier (fs, next) ->
					(derived_types, namespace, source, (handle_function_specifier error attributes fs), storage_class, type_specs, qualifiers), next
				| `attributes (attr, next) ->
					(derived_types, namespace, source, (handle_attribute error attributes attr), storage_class, type_specs, qualifiers), next
				| `extension (_, next) ->
					(derived_types, namespace, source, attributes, storage_class, type_specs, qualifiers), next
				end
			in
			begin match next with
			| `some s ->
				extract result s
			| `none ->
				result
			end
		) in
		let attributes = attributes_of_alignment alignment in
		let derived_types, namespace, source, attributes, storage_class, specs, qualifiers = extract (derived_types, namespace, source, attributes, `none, (no_type_specifier_set, None), no_type_qualifier_set) x in
		let type_by_spec = get_type_by_specifier_set error predefined_types (fst x) attributes.at_mode specs in
		let derived_types, type_by_qualifiers = get_type_by_qualifier_set error derived_types (fst x) type_by_spec qualifiers in
		derived_types, namespace, source, (storage_class, type_by_qualifiers, attributes)
	) and handle_init_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(storage_class: [storage_class | `none])
		(base_type: all_type)
		(attributes: attributes)
		(alias: [`alias of string | `none])
		(x: Syntax.init_declarator p)
		: derived_types * namespace * source_item list * named_item option =
	(
		let derived_types, namespace, source, analyzed_decl, init =
			begin match snd x with
			| `no_init decl ->
				let derived_types, namespace, source, analyzed_decl = handle_declarator error predefined_types derived_types namespace source base_type attributes (fst x, decl) in
				derived_types, namespace, source, analyzed_decl, None
			| `with_init (decl, _, init) ->
				let derived_types, namespace, source, analyzed_decl = handle_declarator error predefined_types derived_types namespace source base_type attributes decl in
				let required_type =
					begin match analyzed_decl with
					| Some (_, _, t, _) -> t
					| None -> base_type
					end
				in
				begin match init with
				| `some init ->
					let derived_types, source, expr = handle_initializer error predefined_types derived_types namespace source required_type init in
					derived_types, namespace, source, analyzed_decl, expr
				| `error ->
					derived_types, namespace, source, analyzed_decl, None
				end
			end
		in
		begin match analyzed_decl with
		| Some (ps, id, t, attr) ->
			(* fill length *)
			let derived_types, t =
				begin match t, init with
				| `array (None, element_t1), Some (_, `array ((Some _ as length), element_t2)) when element_t1 == element_t2 ->
					let array_type, derived_types = find_array_type length `char derived_types in
					derived_types, array_type
				| _ ->
					derived_types, t
				end
			in
			(* by storage class *)
			begin match storage_class with
			| `typedef ->
				if init <> None then (
					error ps "initializer was found with typedef."
				);
				let result = `named (ps, id, `typedef t, attr) in
				let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
				let source = (result :> source_item) :: source in
				derived_types, namespace, source, Some result
			| `extern ->
				if init <> None then (
					error ps "initializer was found with extern."
				);
				let result = `named (ps, id, `extern (t, alias), attr) in
				let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
				let source = (result :> source_item) :: source in
				derived_types, namespace, source, Some result
			| `static ->
				begin match t with
				| `function_type _ as t ->
					if init <> None then (
						error ps "initializer was found with function."
					);
					let result = `named (ps, id, `function_forward (`static, t), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some result
				| _ ->
					error (fst x) "unimplemented!";
					assert false
				end
			| `auto ->
				error (fst x) "unimplemented!";
				assert false
			| `register ->
				begin match t with
				| `function_type _ ->
					error (fst x) "\"register\" could not be applied to function type.";
					derived_types, namespace, source, None
				| _ ->
					let result = `named (ps, id, `variable (t, init), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some result
				end
			| `none ->
				begin match t with
				| `function_type prototype ->
					if init <> None then (
						error ps "initializer was found with function."
					);
					let conflicted =
						begin try
							let previous = StringMap.find id namespace.ns_namespace in
							begin match previous with
							| `named (_, _, `extern ((`function_type previous_prototype), _), _) ->
								(* no error when two external declaration has same prototype *)
								if prototype_ABI_compatibility ~dest:prototype ~source:previous_prototype <> `just then (
									error ps ("\"" ^ id ^ "\" was conflicted.");
								);
								true
							| _ ->
								error ps ("\"" ^ id ^ "\" was conflicted.");
								true
							end
						with Not_found ->
							false
						end
					in
					if not conflicted then (
						let result = `named (ps, id, `extern (t, alias), attr) in
						let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
						let source = (result :> source_item) :: source in
						derived_types, namespace, source, Some result
					) else (
						derived_types, namespace, source, None
					)
				| _ ->
					let result = `named (ps, id, `variable (t, init), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id result namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some result
				end
			end
		| None ->
			derived_types, namespace, source, None
		end
	) and handle_storage_class
		(error: ranged_position -> string -> unit)
		(storage_class: [storage_class | `none])
		(x: Syntax.storage_class_specifier p)
		: [storage_class | `none] =
	(
		if storage_class = `none then (
			begin match snd x with
			| `TYPEDEF -> `typedef
			| `EXTERN -> `extern
			| `STATIC -> `static
			| `AUTO -> `auto
			| `REGISTER -> `register
			end
		) else (
			error (fst x) "storage-class-specifier was duplicated.";
			storage_class
		)
	) and handle_type_specifier
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.type_specifier p)
		(set, named: type_specifier_set * all_type option)
		: derived_types * namespace * source_item list * (type_specifier_set * all_type option) =
	(
		begin match snd x with
		| `VOID ->
			derived_types, namespace, source, ({set with ts_void = set.ts_void + 1}, named)
		| `CHAR ->
			derived_types, namespace, source, ({set with ts_char = set.ts_char + 1}, named)
		| `SHORT ->
			derived_types, namespace, source, ({set with ts_short = set.ts_short + 1}, named)
		| `INT ->
			derived_types, namespace, source, ({set with ts_int = set.ts_int + 1}, named)
		| `LONG ->
			derived_types, namespace, source, ({set with ts_long = set.ts_long + 1}, named)
		| `FLOAT ->
			derived_types, namespace, source, ({set with ts_float = set.ts_float + 1}, named)
		| `DOUBLE ->
			derived_types, namespace, source, ({set with ts_double = set.ts_double + 1}, named)
		| `SIGNED ->
			derived_types, namespace, source, ({set with ts_signed = set.ts_signed + 1}, named)
		| `UNSIGNED ->
			derived_types, namespace, source, ({set with ts_unsigned = set.ts_unsigned + 1}, named)
		| `_BOOL ->
			derived_types, namespace, source, ({set with ts_bool = set.ts_bool + 1}, named)
		| `_IMAGINARY ->
			derived_types, namespace, source, ({set with ts_imaginary = set.ts_imaginary + 1}, named)
		| `_COMPLEX ->
			derived_types, namespace, source, ({set with ts_complex = set.ts_complex + 1}, named)
		| `struct_or_union_specifier spec ->
			let derived_types, namespace, source, named =
				begin match named with
				| Some _ ->
					error (fst x) ("type-specifier was duplicated.");
					derived_types, namespace, source, named
				| None ->
					handle_struct_or_union_specifier error predefined_types derived_types namespace source alignment (fst x, spec)
				end
			in
			derived_types, namespace, source, (set, named)
		| `enum_specifier spec ->
			let derived_types, namespace, source, named =
				begin match named with
				| Some _ ->
					error (fst x) ("type-specifier was duplicated.");
					derived_types, namespace, source, named
				| None ->
					handle_enum_specifier error predefined_types derived_types namespace source (fst x, spec)
				end
			in
			derived_types, namespace, source, (set, named)
		| `typedef_name (`ident name) ->
			let named =
				begin match named with
				| Some _ ->
					error (fst x) ("type-specifier was duplicated.");
					named
				| None ->
					begin try
						begin match StringMap.find name namespace.ns_namespace with
						| (`named (_, _, #named_type_var, _)) as t ->
							Some t
						| _ ->
							error (fst x) (name ^ " was not type.");
							named
						end
					with Not_found ->
						error (fst x) (name ^ " was not declared.");
						named
					end
				end
			in
			derived_types, namespace, source, (set, named)
		| `__int64 ->
			derived_types, namespace, source, ({set with ts_int64 = set.ts_int64 + 1}, named)
		| `__builtin_va_list ->
			derived_types, namespace, source, ({set with ts_builtin_va_list = set.ts_builtin_va_list + 1}, named)
		end
	) and handle_struct_or_union_specifier
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.struct_or_union_specifier p)
		: derived_types * namespace * source_item list * all_type option =
	(
		begin match snd x with
		| `with_body (sou, id, _, decls, _, attrs) ->
			begin match decls with
			| `some decls ->
				(* ready opaque type for recursive *)
				let namespace, source =
					begin match id with
					| `some id ->
						begin match snd sou with
						| `STRUCT ->
							let _, namespace, source = find_struct id namespace source in
							namespace, source
						| `UNION ->
							let _, namespace, source = find_union id namespace source in
							namespace, source
						end
					| `none ->
						namespace, source
					end
				in
				(* attributes *)
				let default_attributes = attributes_of_alignment alignment in
				let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) default_attributes attrs in
				let alignment = attributes.at_aligned in
				(* items *)
				let derived_types, namespace, source, items =
					Traversing.fold_sdnl (fun (derived_types, namespace, sources, rs) decl ->
						let derived_types, namespace, sources, items = handle_struct_declaration error predefined_types derived_types namespace sources alignment decl in
						derived_types, namespace, sources, (List.append rs items)
					) (derived_types, namespace, source, []) decls
				in
				(* check bit-field or not *)
				let items =
					begin match Machine.is_bitfield items with
					| `is_bitfield | `mixed ->
						Machine.fill_bitfield error predefined_types (fst decls) items (* normal members and bit-fields are mixed *)
					| `is_not_bitfield | `empty ->
						items
					end
				in
				(* adjust alignment *)
				let alignment =
					begin match alignment with
					| `aligned n ->
						begin match Machine.alignof_struct items predefined_types with
						| Some good_n when good_n > n ->
							`aligned good_n
						| _ ->
							alignment
						end
					| _ ->
						alignment
					end
				in
				(* type *)
				begin match id with
				| `some (id_p, `ident id_e) ->
					let item, namespace =
						begin match snd sou with
						| `STRUCT ->
							let var = `struct_type (alignment, items) in
							let item = `named (id_p, id_e, var, attributes) in
							let namespace = {namespace with ns_struct = StringMap.add id_e item namespace.ns_struct} in
							item, namespace
						| `UNION ->
							let var = `union items in
							let item = `named (id_p, id_e, var, attributes) in
							let namespace = {namespace with ns_union = StringMap.add id_e item namespace.ns_union} in
							item, namespace
						end
					in
					derived_types, namespace, ((item :> source_item) :: source), Some (item :> all_type)
				| `none ->
					if attributes <> default_attributes then (
						error (fst x) "attribute(s) could not be accepted by the anonymous struct/union.";
					);
					let t: struct_or_union_type_var =
						begin match snd sou with
						| `STRUCT -> `struct_type (alignment, items)
						| `UNION -> `union items
						end
					in
					let item = `anonymous (fst x, t) in
					derived_types, namespace, ((item :> source_item) :: source), Some (item :> all_type)
				end
			| `error ->
				derived_types, namespace, source, None
			end;
		| `no_body (sou, id) ->
			begin match id with
			| `some id ->
				let item, namespace, source =
					begin match snd sou with
					| `STRUCT ->
						find_struct id namespace source
					| `UNION ->
						find_union id namespace source
					end
				in
				derived_types, namespace, source, Some item
			| `error ->
				derived_types, namespace, source, None
			end
		end
	) and handle_struct_declaration
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.struct_declaration p)
		: derived_types * namespace * source_item list * struct_item list =
	(
		let attributes = attributes_of_alignment alignment in
		let handle_anonymous (x: Syntax.struct_or_union_specifier p) = (
			let derived_types, namespace, source, anonymous_sou = handle_struct_or_union_specifier error predefined_types derived_types namespace source alignment x in
			begin match anonymous_sou with
			| Some t ->
				derived_types, namespace, source, ["", (t :> all_type), None, attributes]
			| None ->
				derived_types, namespace, source, []
			end
		) in
		begin match snd x with
		| `named (_, `some sql, `some sdrl, _) ->
			let derived_types, namespace, source, base_type = handle_specifier_qualifier_list error predefined_types derived_types namespace source alignment sql in
			let derived_types, namespace, source, result =
				Traversing.fold_sdrl (fun (derived_types, namespace, source, rs as result) (sdecl: Syntax.struct_declarator p) ->
					let derived_types, namespace, source, item = handle_struct_declarator error predefined_types derived_types namespace source base_type sdecl in
					begin match item with
					| Some item -> derived_types, namespace, source, (item :: rs)
					| None -> result
					end
				) (derived_types, namespace, source, []) sdrl
			in
			derived_types, namespace, source, List.rev result
		| `named (_, `some sql, `error, _) ->
			begin match snd sql with
			| `type_specifier ((sou_spec_p, `struct_or_union_specifier sou_spec_e), `none) ->
				(* anonymous struct or union without __extension__ *)
				handle_anonymous (sou_spec_p, sou_spec_e)
			| _ ->
				(* error *)
				derived_types, namespace, source, []
			end
		| `named (_, `error, _, _) ->
			(* error *)
			derived_types, namespace, source, []
		| `anonymous_struct_or_union (_, sou_spec, _) ->
			handle_anonymous sou_spec
		end
	) and handle_specifier_qualifier_list
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.specifier_qualifier_list p)
		: derived_types * namespace * source_item list * all_type =
	(
		let rec extract (derived_types, namespace, source, type_specs, qualifiers) spec = (
			let result, next =
				begin match snd spec with
				| `type_specifier (ts, next) ->
					let derived_types, namespace, source, type_specs = handle_type_specifier error predefined_types derived_types namespace source alignment ts type_specs in
					(derived_types, namespace, source, type_specs, qualifiers), next
				| `type_qualifier (q, next) ->
					let qualifiers = handle_type_qualifier error qualifiers q in
					(derived_types, namespace, source, type_specs, qualifiers), next
				end
			in
			begin match next with
			| `some s ->
				extract result s
			| `none ->
				result
			end
		) in
		let derived_types, namespace, source, specs, qualifiers = extract (derived_types, namespace, source, (no_type_specifier_set, None), no_type_qualifier_set) x in
		let type_by_spec = get_type_by_specifier_set error predefined_types (fst x) None specs in
		let derived_types, type_by_qualifiers = get_type_by_qualifier_set error derived_types (fst x) type_by_spec qualifiers in
		derived_types, namespace, source, type_by_qualifiers
	) and handle_struct_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(x: Syntax.struct_declarator p)
		: derived_types * namespace * source_item list * struct_item option =
	(
		begin match snd x with
		| `item decl ->
			let derived_types, namespace, source, item = handle_declarator error predefined_types derived_types namespace source base_type no_attributes (fst x, decl) in
			begin match item with
			| Some (_, name, t, attrs) ->
				derived_types, namespace, source, Some (name, t, None, attrs)
			| None ->
				derived_types, namespace, source, None
			end
		| `bit_width (decl, _, expr) ->
			let derived_types, namespace, source, item =
				begin match decl with
				| `some decl ->
					handle_declarator error predefined_types derived_types namespace source base_type no_attributes decl
				| `none ->
					derived_types, namespace, source, Some (fst x, "", base_type, no_attributes)
				end
			in
			let derived_types, source, bw =
				begin match expr with
				| `some expr ->
					let derived_types, source, n = handle_expression error predefined_types derived_types namespace source `rvalue expr in
					begin match n with
					| Some n ->
						begin match integer_of_expression n with
						| Some (_, n) ->
							derived_types, source, Some (-1, Integer.to_int n, true)
						| None ->
							error (fst expr) "int const value was required for bit-field.";
							derived_types, source, None
						end
					| None ->
						derived_types, source, None (* error was already reported *)
					end
				| `error ->
					derived_types, source, None
				end
			in
			begin match item with
			| Some (_, name, t, attrs) ->
				derived_types, namespace, source, Some (name, t, bw, attrs)
			| None ->
				derived_types, namespace, source, None
			end
		end
	) and handle_enum_specifier
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(x: Syntax.enum_specifier p)
		: derived_types * namespace * source_item list * all_type option =
	(
		begin match snd x with
		| `with_body (_, id, _, items, _, _) ->
			(* ready opaque type for recursive ??? *)
			let namespace, source =
				begin match id with
				| `some id ->
					let _, namespace, source = find_enum id namespace source in
					namespace, source
				| `none ->
					namespace, source
				end
			in
			(* items *)
			let derived_types, namespace, source, _, items =
				begin match items with
				| `some items ->
					Traversing.fold_el (fun (derived_types, namespace, source, next, rs) item ->
						let derived_types, namespace, source, (item, next) = handle_enumerator error predefined_types derived_types namespace source next item in
						derived_types, namespace, source, next, (item :: rs)
					) (derived_types, namespace, source, Integer.zero, []) items
				| `error ->
					derived_types, namespace, source, Integer.zero, []
				end
			in
			let items = List.rev items in
			(* type *)
			let namespace, item =
				begin match id with
				| `some (id_p, `ident id_e) ->
					let item = `named (id_p, id_e, `enum items, no_attributes) in
					let namespace = {namespace with ns_enum = StringMap.add id_e item namespace.ns_enum} in
					namespace, (item :> full_enum_type)
				| `none ->
					let t = `enum items in
					let item = `anonymous (fst x, t) in
					namespace, item
				end
			in
			(* add elements to inverted dictionary *)
			let namespace =
				List.fold_left (fun namespace e ->
					let `named (_, e_name, `enum_element _, _) = e in
					{namespace with ns_enum_of_element = StringMap.add e_name item namespace.ns_enum_of_element}
				) namespace items
			in
			(* add to source *)
			let source = (item :> source_item) :: source in
			derived_types, namespace, source, Some (item :> all_type)
		| `no_body (_, id) ->
			begin match id with
			| `some id ->
				let item, namespace, source = find_enum id namespace source in
				derived_types, namespace, source, Some item
			| `error ->
				derived_types, namespace, source, None
			end
		end
	) and handle_enumerator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(next: Integer.t)
		(x: Syntax.enumerator p)
		: derived_types * namespace * source_item list * (enum_item * Integer.t) =
	(
		let derived_types, source, name, value =
			begin match snd x with
			| `no_repr name
			| `with_repr ((_, name), _, `error) ->
				derived_types, source, name, next
			| `with_repr ((_, name), _, `some expr) ->
				let derived_types, source, n = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				let value =
					begin match n with
					| Some n ->
						begin match integer_of_expression n with
						| Some (_, n) ->
							n
						| None ->
							error (fst expr) "int const value was required for enumerator.";
							next
						end
					| None ->
						next (* error was already reported *)
					end
				in
				derived_types, source, name, value
			end
		in
		let result = `named (fst x, name, `enum_element value, no_attributes) in
		let namespace = {namespace with ns_namespace = StringMap.add name (result :> named_item) namespace.ns_namespace} in
		let source = (result :> source_item) :: source in
		derived_types, namespace, source, (result, Integer.add value Integer.one)
	) and handle_type_qualifier
		(_: ranged_position -> string -> unit)
		(set: type_qualifier_set)
		(x: Syntax.type_qualifier p)
		: type_qualifier_set =
	(
		begin match snd x with
		| `CONST ->
			{set with tq_const = true}
		| `RESTRICT | `__restrict__ ->
			{set with tq_restrict = true}
		| `VOLATILE ->
			{set with tq_volatile = true}
		end
	) and handle_function_specifier
		(_: ranged_position -> string -> unit)
		(attributes: attributes)
		(x: Syntax.function_specifier p)
		: attributes =
	(
		begin match snd x with
		| `INLINE | `__inline | `__inline__ ->
			if attributes.at_inline = `always_inline then attributes else
			{attributes with at_inline = `inline}
		end
	) and handle_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(attributes: attributes)
		(x: Syntax.declarator p)
		: derived_types * namespace * source_item list * (ranged_position * string * all_type * attributes) option =
	(
		let pointer, dd, attrs = snd x in
		let derived_types, (t, attributes) =
			begin match pointer with
			| `some pointer ->
				handle_pointer error derived_types base_type attributes pointer
			| `none ->
				derived_types, (base_type, attributes)
			end
		in
		let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
		begin match dd with
		| `some dd ->
			handle_direct_declarator error predefined_types derived_types namespace source t attributes dd
		| `error ->
			derived_types, namespace, source, None
		end
	) and handle_direct_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(attributes: attributes)
		(x: Syntax.direct_declarator p)
		: derived_types * namespace * source_item list * (ranged_position * string * all_type * attributes) option =
	(
		begin match snd x with
		| `ident name ->
			let t =
				begin match attributes.at_mode with
				| Some mode ->
					apply_bit_width_mode error predefined_types (fst x) mode base_type
				| None ->
					base_type
				end
			in
			derived_types, namespace, source, Some (fst x, name, t, attributes)
		| `paren (_, attrs, d, _) ->
			let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
			begin match d with
			| `some d ->
				handle_declarator error predefined_types derived_types namespace source base_type attributes d
			| `error ->
				derived_types, namespace, source, None
			end
		| `array (dd, _, tql, num, _) ->
			if tql <> `none then (
				error (fst x) "unimplemented!";
				assert false
			);
			let derived_types, source, array_type =
				let derived_types, source, n =
					begin match num with
					| `some expr ->
						let derived_types, source, n = handle_expression error predefined_types derived_types namespace source `rvalue expr in
						begin match n with
						| Some n ->
							begin match integer_of_expression n with
							| Some (_, n) ->
								derived_types, source, Some n
							| None ->
								error (fst expr) "int const value was required for array.";
								derived_types, source, None
							end
						| None ->
							derived_types, source, None (* error was already reported *)
						end
					| `none ->
						derived_types, source, None
					end
				in
				let array_type, derived_types = find_array_type n base_type derived_types in
				derived_types, source, array_type
			in
			handle_direct_declarator error predefined_types derived_types namespace source array_type attributes dd
		| `static_array1 _ ->
			error (fst x) "unimplemented!";
			assert false
		| `static_array2 _ ->
			error (fst x) "unimplemented!";
			assert false
		| `dynamic_array _ ->
			error (fst x) "unimplemented!";
			assert false
		| `function_type (dd, _, ptl, _) ->
			let conventions = attributes.at_conventions in
			let derived_types, namespace, source, (params, varargs) =
				handle_parameter_type_list error predefined_types derived_types namespace source attributes.at_aligned ptl
			in
			let prototype: prototype = conventions, params, varargs, base_type in
			let func_type, source = find_function_type prototype source in
			handle_direct_declarator error predefined_types derived_types namespace source (func_type :> all_type) attributes dd
		| `old_function_type (dd, _, idl, _) ->
			begin match idl with
			| `some _ ->
				error (fst x) "unimplemented!";
				assert false
			| `none ->
				let conventions = attributes.at_conventions in
				let prototype: prototype = conventions, [], `varargs, base_type in
				let func_type, source = find_function_type prototype source in
				handle_direct_declarator error predefined_types derived_types namespace source (func_type :> all_type) attributes dd
			end
		end
	) and handle_pointer
		(error: ranged_position -> string -> unit)
		(derived_types: derived_types)
		(t: all_type)
		(attributes: attributes)
		(x: Syntax.pointer p)
		: derived_types * (all_type * attributes) =
	(
		let t, derived_types = find_pointer_type t derived_types in
		begin match snd x with
		| `nil (_, qs, attrs) ->
			let qualifiers = Traversing.opt Traversing.fold_tql (handle_type_qualifier error) no_type_qualifier_set qs in
			let derived_types, t = get_type_by_qualifier_set error derived_types (fst x) t qualifiers in
			let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
			derived_types, (t, attributes)
		| `cons (_, qs, next) ->
			let qualifiers = Traversing.opt Traversing.fold_tql (handle_type_qualifier error) no_type_qualifier_set qs in
			let derived_types, t = get_type_by_qualifier_set error derived_types (fst x) t qualifiers in
			handle_pointer error derived_types t attributes next
		end
	) and handle_parameter_type_list
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.parameter_type_list p)
		: derived_types * namespace * source_item list * (variable list * varargs_opt) =
	(
		let params, varargs =
			begin match snd x with
			| `args params ->
				(fst x, params), `none
			| `varargs (params, _, _) ->
				params, `varargs
			end
		in
		let derived_types, namespace, source, params =
			Traversing.fold_pl (fun (derived_types, namespace, source, rs) param ->
				let derived_types, namespace, source, param = handle_parameter_declaration error predefined_types derived_types namespace source alignment param in
				derived_types, namespace, source, (param :: rs)
			) (derived_types, namespace, source, []) params
		in
		let derived_types, params =
			begin match params with
			| `named (_, _, `variable (`void, _), _) :: [] ->
				derived_types, []
			| _ ->
				(* rev and replace type to pointer instead of array/function *)
				List.fold_left (fun (derived_types, params) param ->
					let `named (ps, name, `variable (the_type, init), attrs) = param in
					begin match resolve_typedef (remove_type_qualifiers the_type :> all_type) with
					| `array (_, base_type) ->
						let t, derived_types = find_pointer_type (base_type :> all_type) derived_types in
						let new_param = `named (ps, name, `variable (t, init), attrs) in
						derived_types, new_param :: params
					| `function_type _ as ft -> (* illegal, but some headers have this form *)
						let t, derived_types = find_pointer_type (ft :> all_type) derived_types in
						let new_param = `named (ps, name, `variable (t, init), attrs) in
						derived_types, new_param :: params
					| _ ->
						derived_types, param :: params
					end
				) (derived_types, []) params
			end
		in
		derived_types, namespace, source, (params, varargs)
	) and handle_parameter_declaration
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.parameter_declaration p)
		: derived_types * namespace * source_item list * variable =
	(
		begin match snd x with
		| `with_name (spec, decl) ->
			let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
			if storage_class <> `none then (
				error (fst x) "storage-class was found in parameter-declaration."
			);
			let derived_types, namespace, source, param = handle_declarator error predefined_types derived_types namespace source base_type attributes decl in
			begin match param with
			| Some (ps, name, all_type, attributes) ->
				let result = `named (ps, name, `variable (all_type, None), attributes) in
				derived_types, namespace, source, result
			| None ->
				let result = `named (fst x, "", `variable (base_type, None), attributes) in
				derived_types, namespace, source, result
			end
		| `no_name (spec, decl) ->
			let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
			if storage_class <> `none then (
				error (fst x) "storage-class was found in parameter-declaration."
			);
			begin match decl with
			| `some decl ->
				let derived_types, namespace, source, (t, attributes) = handle_abstract_declarator error predefined_types derived_types namespace source base_type attributes decl in
				let result = `named (fst x, "", `variable (t, None), attributes) in
				derived_types, namespace, source, result
			| `none ->
				let result = `named (fst x, "", `variable (base_type, None), attributes) in
				derived_types, namespace, source, result
			end
		end
	) and handle_type_name
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(x: Syntax.type_name p)
		: derived_types * source_item list * all_type =
	(
		let old_namespace = namespace in
		let sql, decl = snd x in
		let derived_types, namespace, source, base_type = handle_specifier_qualifier_list error predefined_types derived_types namespace source `default sql in
		begin match decl with
		| `some decl ->
			let derived_types, namespace, source, (t, attributes) = handle_abstract_declarator error predefined_types derived_types namespace source base_type no_attributes decl in
			if attributes <> no_attributes then (
				error (fst x) "attribute(s) cound not be accepted in expression.";
			);
			if namespace != old_namespace then (
				error (fst x) "new type-specifier was found in expression."
			);
			derived_types, source, t
		| `none ->
			if namespace != old_namespace then (
				error (fst x) "new type-specifier was found in expression."
			);
			derived_types, source, base_type
		end
	) and handle_abstract_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(attributes: attributes)
		(x: Syntax.abstract_declarator p)
		: derived_types * namespace * source_item list * (all_type * attributes) =
	(
		begin match snd x with
		| `pointer p ->
			let derived_types, t_with_attr = handle_pointer error derived_types base_type attributes (fst x, p) in
			derived_types, namespace, source, t_with_attr
		| `declarator (p, dd) ->
			let derived_types, (t, attributes) =
				begin match p with
				| `some p ->
					handle_pointer error derived_types base_type attributes p
				| `none ->
					derived_types, (base_type, attributes)
				end
			in
			handle_direct_abstract_declarator error predefined_types derived_types namespace source t attributes dd
		end
	) and handle_direct_abstract_declarator
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(base_type: all_type)
		(attributes: attributes)
		(x: Syntax.direct_abstract_declarator p)
		: derived_types * namespace * source_item list * (all_type * attributes) =
	(
		begin match snd x with
		| `paren (_, attrs, decl, _) ->
			begin match decl with
			| `some decl ->
				let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
				handle_abstract_declarator error predefined_types derived_types namespace source base_type attributes decl
			| `error ->
				derived_types, namespace, source, (base_type, attributes)
			end
		| `array (dd, _, tql, num, _) ->
			if tql <> `none then (
				error (fst x) "unimplemented!";
				assert false
			);
			let derived_types, source, array_type =
				let derived_types, source, n =
					begin match num with
					| `some expr ->
						let derived_types, source, n = handle_expression error predefined_types derived_types namespace source `rvalue expr in
						begin match n with
						| Some n ->
							begin match integer_of_expression n with
							| Some (_, n) ->
								derived_types, source, Some n
							| None ->
								error (fst expr) "int const value was required for array.";
								derived_types, source, None
							end
						| None ->
							derived_types, source, None (* error was already reported *)
						end
					| `none ->
						derived_types, source, None
					end
				in
				let array_type, derived_types = find_array_type n base_type derived_types in
				derived_types, source, array_type
			in
			begin match dd with
			| `some dd ->
				handle_direct_abstract_declarator error predefined_types derived_types namespace source array_type attributes dd
			| `none ->
				derived_types, namespace, source, ((array_type :> all_type), attributes)
			end
		| `static_array1 _ ->
			error (fst x) "unimplemented!";
			assert false
		| `static_array2 _ ->
			error (fst x) "unimplemented!";
			assert false
		| `dynamic_array _ ->
			error (fst x) "unimplemented!";
			assert false
		| `function_type (dd, _, ptl, _) ->
			let conventions = attributes.at_conventions in
			let derived_types, namespace, source, (params, varargs) =
				begin match ptl with
				| `some ptl ->
					handle_parameter_type_list error predefined_types derived_types namespace source attributes.at_aligned ptl
				| `none ->
					derived_types, namespace, source, ([], `none)
				end
			in
			let prototype: prototype = conventions, params, varargs, base_type in
			let func_type, source = find_function_type prototype source in
			begin match dd with
			| `some dd ->
				handle_direct_abstract_declarator error predefined_types derived_types namespace source (func_type :> all_type) attributes dd
			| `none ->
				derived_types, namespace, source, ((func_type :> all_type), attributes)
			end
		end
	) and handle_initializer
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(required_type: all_type)
		(x: Syntax.initializer_t p)
		: derived_types * source_item list * expression option =
	(
		begin match snd x with
		| `expression expr ->
			let result = handle_expression error predefined_types derived_types namespace source `rvalue (fst x, expr) in
			begin match result with
			| derived_types, sources, Some expr ->
				derived_types, sources, Some (conv_expr required_type expr)
			| _ ->
				result
			end
		| `list (_, list, _) ->
			begin match list with
			| `some list ->
				handle_initializer_list error predefined_types derived_types namespace source required_type list
			| `error ->
				derived_types, source, None
			end
		end
	) and handle_initializer_list
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(required_type: all_type)
		(x: Syntax.initializer_list p)
		: derived_types * source_item list * expression option =
	(
		let resolved_type1 = resolve_typedef required_type in
		let resolved_type2 = resolve_opaque namespace resolved_type1 in
		let required_type =
			if resolved_type1 != resolved_type2 then (
				resolved_type2 (* opaque to full declaration *)
			) else (
				required_type (* keep typedef *)
			)
		in
		let kind =
			begin match resolved_type2 with
			| `anonymous (_, `struct_type (_, items))
			| `anonymous (_, `union items)
			| `named (_, _, `struct_type (_, items), _)
			| `named (_, _, `union items, _) ->
				`aggregate items
			| `array (size, t) ->
				let size =
					begin match size with
					| Some size -> Integer.to_int size
					| None -> -1
					end
				in
				`array (size, (t :> not_qualified_type))
			| `named(ps, _, `generic_type, _) ->
				`array (-1, `named (ps, "", `generic_type, no_attributes))
			| t ->
				error (fst x) "bad type for initializer-list.";
				`array (-1, remove_type_qualifiers t)
			end
		in
		let derived_types, source, kind, list =
			Traversing.fold_il (fun (derived_types, source, kind, rs) (d, i: Syntax.designation opt * Syntax.initializer_t pe) ->
				let element_type, kind =
					begin match kind with
					| `aggregate items ->
						begin match items with
						| (_, t, _, _) :: items_r ->
							t, `aggregate items_r
						| [] ->
							error (fst x) "too many elements for struct or union.";
							let dummy_type = `named (fst x, "", `generic_type, no_attributes) in
							dummy_type, `aggregate []
						end
					| `array (size, t) ->
						(t :> all_type), `array (size - 1, t)
					end
				in
				begin match d with
				| `some (d_p, _) ->
					error d_p "unimplemented.";
					assert false
				| `none ->
					()
				end;
				begin match i with
				| `some init ->
					let derived_types, source, elm = handle_initializer error predefined_types derived_types namespace source element_type init in
					begin match elm with
					| Some elm ->
						derived_types, source, kind, (elm :: rs)
					| None ->
						error (fst x) "unimplemented.";
						assert false
					end
				| `error ->
					error (fst x) "unimplemented.";
					assert false
				end
			) (derived_types, source, kind, []) x
		in
		let zero =
			begin match kind with
			| `aggregate items ->
				if items <> [] then (
					error (fst x) "too few elements for struct or union."
				);
				None
			| `array (size, t) ->
				if size > 0 then Some (zero_expression error (fst x) t) else None
			end
		in
		derived_types, source, Some (`compound (List.rev list, zero), required_type)
	) and handle_statement
		?(control: [`loop | `switch | `none] = `none)
		?(return_type: all_type option)
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.statement p)
		: derived_types * source_item list * statement option =
	(
		let handle_statement_or_error ?control derived_types source (statement: Syntax.statement pe) = (
			begin match statement with
			| `some (cs_p, `compound cs_e) ->
				handle_compound_statement ?control ?return_type error predefined_types derived_types namespace source alignment (cs_p, cs_e)
			| `some statement ->
				let derived_types, source, statement = handle_statement ?control ?return_type error predefined_types derived_types namespace source alignment statement in
				begin match statement with
				| Some statement ->
					derived_types, source, statement :: []
				| None ->
					derived_types, source, []
				end
			| `error ->
				derived_types, source, []
			end
		) in
		begin match snd x with
		| `asm (_, volatile, _, code, in_args, _, _) ->
			let handle_arg request (derived_types, source, rs) arg = (
				let _, ((_, `chars_literal reg), _, expr, _) = arg in
				begin match expr with
				| `some expr ->
					let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source request expr in
					begin match expr with
					| Some expr ->
						derived_types, source, (reg, expr) :: rs
					| None ->
						derived_types, source, rs
					end
				| `error ->
					derived_types, source, rs
				end
			) in
			let handle_reg (derived_types, rs) reg = (
				let _, `chars_literal reg = reg in
				derived_types, reg :: rs
			) in
			let volatile =
				begin match volatile with
				| `some (_, (`VOLATILE | `__volatile__)) ->
					`volatile
				| `none ->
					`none
				end
			in
			begin match code with
			| `some (_, `chars_literal code) ->
				let derived_types, source, in_args, out_args, destructive =
					begin match in_args with
					| `some (_, (_, in_args, out_args)) ->
						let derived_types, source, in_args = Traversing.opt Traversing.fold_iaal (handle_arg `lvalue) (derived_types, source, []) in_args in
						let in_args = List.rev in_args in
						begin match out_args with
						| `some (_, (_, out_args, destructive)) ->
							let derived_types, source, out_args = Traversing.opt Traversing.fold_iaal (handle_arg `rvalue) (derived_types, source, []) out_args in
							let out_args = List.rev out_args in
							begin match destructive with
							| `some (_, (_, `some destructive)) ->
								let derived_types, destructive = Traversing.fold_iarl handle_reg (derived_types, []) destructive in
								let destructive = List.rev destructive in
								derived_types, source, in_args, out_args, destructive
							| `some (_, (_, `error)) | `none ->
								derived_types, source, in_args, out_args, []
							end
						| `none ->
							derived_types, source, in_args, [], []
						end
					| `none ->
						derived_types, source, [], [], []
					end
				in
				derived_types, source, Some (`asm (volatile, code, in_args, out_args, destructive))
			| `error ->
				derived_types, source, None
			end
		| `label ((_, `ident label), _, stmt) ->
			let derived_types, source, stmt = handle_statement_or_error ~control derived_types source stmt in
			derived_types, source, Some (`label (label, stmt))
		| `case _ ->
			error (fst x) "unimplemented!";
			assert false
		| `default _ ->
			error (fst x) "unimplemented!";
			assert false
		| `compound cs_e ->
			let derived_types, source, stmts = handle_compound_statement ~control ?return_type error predefined_types derived_types namespace source alignment (fst x, cs_e) in
			derived_types, source, Some (`compound stmts)
		| `expression (expr, _) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					derived_types, source, Some (`expression expr)
				| None ->
					derived_types, source, None
				end
			| `none ->
				derived_types, source, None
			end
		| `__builtin_va_start (_, _, arg, _, format, _, _) ->
			begin match arg, format with
			| `some arg, `some format ->
				let derived_types, source, arg = handle_expression error predefined_types derived_types namespace source `lvalue arg in
				let derived_types, source, format = handle_expression error predefined_types derived_types namespace source `rvalue format in
				begin match arg, format with
				| Some arg, Some format ->
					derived_types, source, Some (`va_start (arg, format))
				| _ ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `__builtin_va_end (_, _, arg, _, _) ->
			begin match arg with
			| `some arg ->
				let derived_types, source, arg = handle_expression error predefined_types derived_types namespace source `lvalue arg in
				begin match arg with
				| Some arg ->
					derived_types, source, Some (`va_end arg)
				| _ ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `__builtin_va_copy (_, _, dest, _, src, _, _) ->
			begin match dest, src with
			| `some dest, `some src ->
				let derived_types, source, dest = handle_expression error predefined_types derived_types namespace source `lvalue dest in
				let derived_types, source, src = handle_expression error predefined_types derived_types namespace source `rvalue src in
				begin match dest, src with
				| Some dest, Some src ->
					derived_types, source, Some (`va_copy (dest, src))
				| _ ->
					derived_types, source, None
				end
			| _ ->
				derived_types, source, None
			end
		| `if_then (_, _, expr, _, true_case) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				let derived_types, source, true_case = handle_statement_or_error ~control derived_types source true_case in
				begin match expr with
				| Some expr ->
					let expr = conv_expr `bool expr in
					derived_types, source, Some (`if_statement (expr, true_case, []))
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `if_then_else (_, _, expr, _, true_case, _, false_case) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				let derived_types, source, true_case = handle_statement_or_error ~control derived_types source true_case in
				let derived_types, source, false_case = handle_statement_or_error ~control derived_types source false_case in
				begin match expr with
				| Some expr ->
					let expr = conv_expr `bool expr in
					derived_types, source, Some (`if_statement (expr, true_case, false_case))
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `switch _ ->
			error (fst x) "unimplemented!";
			assert false
		| `while_loop (_, _, expr, _, stmt) ->
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					let derived_types, source, stmt = handle_statement_or_error ~control:`loop derived_types source stmt in
					derived_types, source, Some (`while_loop (expr, stmt))
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `do_loop (_, stmt, _, _, expr, _, _) ->
			let derived_types, source, stmt = handle_statement_or_error ~control:`loop derived_types source stmt in
			begin match expr with
			| `some expr ->
				let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
				begin match expr with
				| Some expr ->
					derived_types, source, Some (`do_loop (stmt, expr))
				| None ->
					derived_types, source, None
				end
			| `error ->
				derived_types, source, None
			end
		| `for_loop (_, _, init_expr, _, cond_expr, _, next_expr, _, stmt) ->
			let derived_types, source, init_expr =
				begin match init_expr with
				| `some init_expr ->
					handle_expression error predefined_types derived_types namespace source `rvalue init_expr
				| `none ->
					derived_types, source, None
				end
			in
			let derived_types, source, cond_expr =
				begin match cond_expr with
				| `some cond_expr ->
					handle_expression error predefined_types derived_types namespace source `rvalue cond_expr
				| `none ->
					derived_types, source, None
				end
			in
			let derived_types, source, next_expr =
				begin match next_expr with
				| `some next_expr ->
					handle_expression error predefined_types derived_types namespace source `rvalue next_expr
				| `none ->
					derived_types, source, None
				end
			in
			let derived_types, source, stmt = handle_statement_or_error ~control:`loop derived_types source stmt in
			derived_types, source, Some (`for_loop (init_expr, cond_expr, next_expr, stmt))
		| `for_with_declaration _ ->
			error (fst x) "unimplemented!";
			assert false
		| `goto (_, label, _) ->
			begin match label with
			| `some (_, `ident label) ->
				derived_types, source, Some (`goto label)
			| `error ->
				derived_types, source, None
			end
		| `continue _ ->
			error (fst x) "unimplemented!";
			assert false
		| `break _ ->
			begin match control with
			| `loop | `switch as control ->
				derived_types, source, Some (`break control)
			| `none ->
				error (fst x) "break was found out of loop or switch.";
				derived_types, source, None
			end
		| `return (_, expr, _) ->
			begin match return_type with
			| Some return_type ->
				begin match expr with
				| `some expr ->
					let derived_types, source, expr = handle_expression error predefined_types derived_types namespace source `rvalue expr in
					let expr = bind_option (conv_expr return_type) expr in
					derived_types, source, Some (`return (expr, return_type))
				| `none ->
					derived_types, source, Some (`return (None, return_type))
				end
			| None ->
				error (fst x) "return statement was not allowed here.";
				derived_types, source, None
			end
		end
	) and handle_compound_statement
		?(control: [`loop | `switch | `none] = `none)
		?(return_type: all_type option)
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.compound_statement p)
		: derived_types * source_item list * statement list =
	(
		let _, (_, items, _) = x in
		begin match items with
		| `some items ->
			let items = List.rev (Traversing.fold_bil (fun rs item -> item :: rs) [] items) in
			let rec loop derived_types namespace source (xs: Syntax.block_item p list) rs = (
				begin match xs with
				| x :: xr ->
					begin match snd x with
					| `declaration decl ->
						let derived_types, namespace, local, _ = handle_declaration error predefined_types derived_types namespace [] alignment (fst x, decl) in
						let local = List.rev local in (* source order *)
						let derived_types, source, stmts = loop derived_types namespace source xr [] in
						let local = `local (local, stmts) in
						derived_types, source, List.rev (local :: List.rev rs)
					| `statement stmt ->
						let derived_types, source, stmt = handle_statement ~control ?return_type error predefined_types derived_types namespace source alignment (fst x, stmt) in
						let rs =
							begin match stmt with
							| Some stmt -> stmt :: rs
							| None -> rs
							end
						in
						loop derived_types namespace source xr rs
					end
				| [] ->
					derived_types, source, List.rev rs
				end
			) in
			loop derived_types namespace source items []
		| `none ->
			derived_types, source, []
		end
	);;
	
	let rec handle_translation_unit
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(sources: (source_item list * extra_info) StringMap.t)
		(x: Syntax.translation_unit)
		: derived_types * namespace * (source_item list * extra_info) StringMap.t * alignment list * mapping_options =
	(
		let in_f (current_filename: string) (derived_types, namespace, sources, alignment_stack, mapping_options) = (
			let current_source, current_info =
				StringMap.find_or ~default:empty_source current_filename sources
			in
			derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
		) in
		let out_f (previous_filename: string) (derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info) = (
			let sources = StringMap.add previous_filename (current_source, current_info) sources in
			derived_types, namespace, sources, alignment_stack, mapping_options
		) in
		Traversing.fold_tu (fun (derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info) (def_p, def_e) ->
			let alignment = List.hd alignment_stack in
			begin match def_e with
			| `function_definition fdef ->
				let derived_types, namespace, current_source, _ = handle_function_definition error predefined_types derived_types namespace current_source alignment (def_p, fdef) in
				derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
			| `aliased_declaration adef ->
				let derived_types, namespace, current_source, _ = handle_aliased_declaration error predefined_types derived_types namespace current_source alignment (def_p, adef) in
				derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
			| `declaration decl ->
				let derived_types, namespace, current_source, _ = handle_declaration error predefined_types derived_types namespace current_source alignment (def_p, decl) in
				derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
			| `pragma pragma ->
				let derived_types, current_info, alignment_stack, mapping_options = handle_pragma error predefined_types derived_types namespace current_info alignment_stack mapping_options (def_p, pragma) in
				derived_types, namespace, sources, alignment_stack, mapping_options, current_source, current_info
			end
		) in_f out_f (derived_types, namespace, sources, [`default], no_mapping_options) x
	) and handle_function_definition
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.function_definition p)
		: derived_types * namespace * source_item list * [> [> function_definition] with_name] option =
	(
		let _, (spec, decl, params, stmt) = x in
		if params <> `none then (
			error (fst x) "unimplemented!";
			assert false
		);
		let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
		begin match decl with
		| `some decl ->
			let derived_types, namespace, source, analyzed_decl = handle_declarator error predefined_types derived_types namespace source base_type attributes decl in
			begin match analyzed_decl with
			| Some (ps, id, t, attr) ->
				begin match t with
				| `function_type prototype as t ->
					let st =
						begin match storage_class with
						| `static ->
							`static
						| `none ->
							`none
						| `extern when attr.at_inline <> `none ->
							`extern_inline
						| `extern | `typedef | `auto | `register ->
							error (fst x) "bad storage-class was found in function-definition.";
							`none
						end
					in
					let derived_types, source, body =
						begin match stmt with
						| `some stmt ->
							(* add parameters to namespace *)
							let _, args, _, return_type = prototype in
							let local =
								List.fold_left (fun local arg ->
									let `named (_, name, _, _) = arg in
									{local with ns_namespace = StringMap.add name (arg :> named_item) local.ns_namespace}
								) namespace args
							in
							handle_compound_statement ~return_type error predefined_types derived_types local source alignment stmt
						| `error ->
							derived_types, source, []
						end
					in
					let result = `named (ps, id, `function_definition (st, t, body), attr) in
					let namespace = {namespace with ns_namespace = StringMap.add id (result :> named_item) namespace.ns_namespace} in
					let source = (result :> source_item) :: source in
					derived_types, namespace, source, Some (result)
				| _ ->
					error ps "function-type was expected.";
					derived_types, namespace, source, None
				end
			| None ->
				derived_types, namespace, source, None
			end
		| `error ->
			derived_types, namespace, source, None
		end
	) and handle_aliased_declaration
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(alignment: alignment)
		(x: Syntax.aliased_declaration p)
		: derived_types * namespace * source_item list * named_item option =
	(
		let _, (spec, decl, _, _, alias, _, attrs, _) = x in
		let derived_types, namespace, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source alignment spec in
		let attributes = Traversing.opt Traversing.fold_al (handle_attribute error) attributes attrs in
		let derived_types, namespace, source, item =
			begin match decl with
			| `some (decl_p, decl_e) ->
				let alias =
					begin match alias with
					| `some (_, `chars_literal alias) -> `alias alias
					| `error -> `none
					end
				in
				let idecl = decl_p, `no_init decl_e in (* provisional... *)
				handle_init_declarator error predefined_types derived_types namespace source storage_class base_type attributes alias idecl
			| `error ->
				derived_types, namespace, source, None
			end
		in
		derived_types, namespace, source, item
	);;
	
	let handle_define
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(derived_types: derived_types)
		(namespace: namespace)
		(source: source_item list)
		(instances: (all_type list) StringMap.t)
		(name: string)
		(define: define p)
		: derived_types * source_item list =
	(
		let def_p, def_e = define in
		if StringMap.mem name namespace.ns_namespace &&
			(match def_e with `any _ -> false | _ -> true)
		then (
			(* suppressing inline-version macro *)
			let has_same_prototype =
				begin match def_e with
				| `function_expr (m_args, m_varargs, _)
				| `function_stmt (m_args, m_varargs, _) when List.for_all (fun (_, k) -> k = `value) m_args ->
					let existed = StringMap.find name namespace.ns_namespace in
					begin match existed with
					| `named (_, _, `function_forward (_, t), _)
					| `named (_, _, `function_definition (_, t, _), _)
					| `named (_, _, `extern ((#function_type as t), _), _) ->
						let `function_type (_, f_args, f_varargs, _) = t in
						List.length f_args = List.length m_args && f_varargs = m_varargs
					| _ ->
						false
					end
				| _ ->
					false
				end
			in
			if not has_same_prototype then (
				error def_p (name ^ " was repeated by normal symbol and macro.")
			);
			derived_types, source
		) else (
			let new_any (message: string) =
				`named (def_p, name, `defined_any message, no_attributes)
			in
			let new_local (args: (string p * [`typedef | `value]) list) = (
				let local, formal_types =
					List.fold_left (fun (local, formal_types as r) ((ft_p, formal_type), kind) ->
						if kind <> `typedef then r else
						(* dummy type *)
						let item = `named (ft_p, formal_type, `generic_type, no_attributes) in
						{local with ns_namespace = StringMap.add formal_type item local.ns_namespace},
						item :: formal_types
					) (namespace, []) args
				in
				let local, args =
					List.fold_left (fun (local, args as r) ((arg_p, arg), kind) ->
						if kind <> `value then r else
						(* dummy type *)
						let t = `named (arg_p, "", `generic_type, no_attributes) in
						(* dummy value *)
						let item = `named (def_p, arg, `generic_value t, no_attributes) in
						{local with ns_namespace = StringMap.add arg item local.ns_namespace},
						item :: args
					) (local, []) args
				in
				local, List.rev formal_types, List.rev args
			) in
			begin match def_e with
			| `operator op ->
				let item = `named (def_p, name, `defined_operator op, no_attributes) in
				let source = item :: source in
				derived_types, source
			| `declaration_specifiers spec ->
				let rec only_no_type (storage_class, type_qualifiers, attributes) (x: Syntax.declaration_specifiers): ([storage_class | `none] * type_qualifier_set * attributes) option = (
					let do_next result next = (
						begin match next with
						| `some (_, next) ->
							only_no_type result next
						| `none ->
							Some result
						end
					) in
					begin match x with
					| `type_specifier _ ->
						None
					| `extension (_, next) ->
						do_next (storage_class, type_qualifiers, attributes) next
					| `type_qualifier (tq, next) ->
						let type_qualifiers = handle_type_qualifier error type_qualifiers tq in
						do_next (storage_class, type_qualifiers, attributes) next
					| `storage_class_specifier (sc, next) ->
						let storage_class = handle_storage_class error storage_class sc in
						do_next (storage_class, type_qualifiers, attributes) next
					| `function_specifier (fs, next) ->
						let attributes = handle_function_specifier error attributes fs in
						do_next (storage_class, type_qualifiers, attributes) next
					| `attributes (attr, next) ->
						let attributes = handle_attribute error attributes attr in
						do_next (storage_class, type_qualifiers, attributes) next
					end
				) in
				begin match only_no_type (`none, no_type_qualifier_set, no_attributes) spec with
				| Some (storage_class, type_qualifiers, attributes) ->
					let type_qualifier =
						begin match type_qualifiers with
						| {tq_const = true; tq_restrict = false; tq_volatile = false} ->
							Some `const
						| {tq_const = true; tq_restrict = _; tq_volatile = _} ->
							error def_p "plural type-qualifier was found in alias macro.";
							Some `const
						| {tq_const = false; tq_restrict = true; tq_volatile = false} ->
							Some `restrict
						| {tq_const = false; tq_restrict = true; tq_volatile = _} ->
							error def_p "plural type-qualifier was found in alias macro.";
							Some `restrict
						| {tq_const = false; tq_restrict = false; tq_volatile = true} ->
							Some `volatile
						| {tq_const = false; tq_restrict = false; tq_volatile = false} ->
							None
						end
					in
					begin match type_qualifier with
					| Some type_qualifier ->
						if storage_class <> `none then (
							error def_p "storage-class was found in type-qualifier alias macro."
						);
						let item = `named (def_p, name, `defined_type_qualifier type_qualifier, attributes) in
						let source = item :: source in
						derived_types, source
					| None ->
						let item = `named (def_p, name, `defined_specifiers storage_class, attributes) in
						let source = item :: source in
						derived_types, source
					end
				| None ->
					let derived_types, n2, source, (storage_class, base_type, attributes) = handle_declaration_specifiers error predefined_types derived_types namespace source `default (def_p, spec) in
					if storage_class <> `none then (
						error def_p "storage-class was found in typedef-like macro."
					);
					if n2 != namespace then (
						error def_p "new type-specifier was found in typedef-like marco."
					);
					let item = `named (def_p, name, `defined_typedef base_type, attributes) in
					let source = item :: source in
					derived_types, source
				end
			| `initializer_t init ->
				let is_element_access (x: Syntax.initializer_t): string list = (
					let rec loop rs (x: Syntax.expression): string list = (
						begin match x with
						| `ident name ->
							name :: rs
						| `element_access ((_, obj), _, `some (_, `ident field)) ->
							loop (field :: rs) obj
						| _ ->
							[]
						end
					) in
					begin match x with
					| `expression expr -> loop [] expr
					| `list _ -> []
					end
				) in
				begin match is_element_access init with
				| leftmost :: [] when StringMap.mem leftmost namespace.ns_opaque_enum ->
					let t = (StringMap.find leftmost namespace.ns_opaque_enum :> opaque_type) in
					let item = `named (def_p, name, `defined_opaque_type t, no_attributes) in
					let source = item :: source in
					derived_types, source
				| leftmost :: [] when StringMap.mem leftmost namespace.ns_opaque_struct ->
					let t = (StringMap.find leftmost namespace.ns_opaque_struct :> opaque_type) in
					let item = `named (def_p, name, `defined_opaque_type t, no_attributes) in
					let source = item :: source in
					derived_types, source
				| leftmost :: [] when StringMap.mem leftmost namespace.ns_opaque_union ->
					let t = (StringMap.find leftmost namespace.ns_opaque_union :> opaque_type) in
					let item = `named (def_p, name, `defined_opaque_type t, no_attributes) in
					let source = item :: source in
					derived_types, source
				| leftmost :: _ as accessing when not (StringMap.mem leftmost namespace.ns_namespace) ->
					begin match inference_by_field accessing source with
					| `defined_element_access _ as result ->
						let item = `named (def_p, name, result, no_attributes) in
						let source = item :: source in
						derived_types, source
					| `error message ->
						error def_p message;
						derived_types, source
					end
				| _ ->
					let required_type =
						begin try
							begin match StringMap.find name instances with
							| t :: [] ->
								(t :> [all_type | `uninterpretable])
							| _ as ts ->
								if List.length ts > 1 then error def_p "instance of the initializer macro was overloaded.";
								raise Not_found
							end
						with Not_found ->
							if is_known_uninterpretable_macro def_p name then (
								(`uninterpretable :> [all_type | `uninterpretable])
							) else (
								`named (def_p, "", `generic_type, no_attributes) (* dummy type *)
							)
						end
					in
					begin match required_type with
					| `uninterpretable ->
						let source = new_any "uninterpretable" :: source in
						derived_types, source
					| #all_type as required_type ->
						let derived_types, source, expr = handle_initializer error predefined_types derived_types namespace source required_type (def_p, init) in
						let item =
							begin match expr with
							| Some expr ->
								begin match expr with
								| _, `named (_, "", `generic_type, _) -> (* failed to type inference *)
									new_any "plase type with #pragma instance"
								| _ ->
									`named (def_p, name, `defined_expression expr, no_attributes)
								end
							| None ->
								new_any "bad expression"
							end
						in
						let source = item :: source in
						derived_types, source
					end
				end
			| `function_expr (args, varargs, expr) ->
				if is_known_uninterpretable_macro def_p name then (
					let source = new_any "uninterpretable" :: source in
					derived_types, source
				) else (
					let local, formal_types, args = new_local args in
					let derived_types, source, expr = handle_expression error predefined_types derived_types local source `rvalue (def_p, expr) in
					let item =
						begin match expr with
						| Some expr ->
							`named (def_p, name, `defined_generic_expression (formal_types, args, varargs, expr), no_attributes)
						| None ->
							new_any "bad expression"
						end
					in
					let source = item :: source in
					derived_types, source
				)
			| `function_stmt (args, varargs, stmt) ->
				if is_known_uninterpretable_macro def_p name then (
					let source = new_any "uninterpretable" :: source in
					derived_types, source
				) else (
					let local, formal_types, args = new_local args in
					let derived_types, source, stmt = handle_statement error predefined_types derived_types local source `default (def_p, stmt) in
					let item =
						begin match stmt with
						| Some stmt ->
							`named (def_p, name, `defined_generic_statement (formal_types, args, varargs, stmt), no_attributes)
						| None ->
							new_any "bad statement"
						end
					in
					let source = item :: source in
					derived_types, source
				)
			| `any message ->
				let source = new_any message :: source in
				derived_types, source
			end
		)
	);;
	
	let analyze
		(error: ranged_position -> string -> unit)
		(lang: language)
		(sizeof: sizeof)
		(typedef: language_typedef)
		(builtin: (string *
			[< predefined_type | `pointer of [< predefined_type | `const of [< predefined_type]]] list *
			[< predefined_type | `pointer of [< predefined_type | `const of [< predefined_type]]]) list)
		(translation_unit: Syntax.translation_unit)
		(defines: (define p) StringMap.t)
		: predefined_types * derived_types * namespace * (source_item list * extra_info) StringMap.t * mapping_options =
	(
		let predefined_types = ready_predefined_types lang sizeof typedef in
		let derived_types, namespace, builtin_source =
			List.fold_left (fun (derived_types, namespace, source) (name, args, ret) ->
				let type_map t predefined_types derived_types: all_type * derived_types = (
					begin match t with
					| `pointer (#predefined_type as target_t) ->
						let target_t = find_predefined_type target_t predefined_types in
						find_pointer_type target_t derived_types
					| `pointer (`const (#predefined_type as target_t)) ->
						let target_t = find_predefined_type target_t predefined_types in
						let const_type, derived_types = find_const_type target_t derived_types in
						find_pointer_type const_type derived_types
					| #predefined_type as t ->
						find_predefined_type t predefined_types, derived_types
					end
				) in
				let derived_types, args =
					List.fold_left (fun (derived_types, rs) arg ->
						let arg, derived_types = type_map arg predefined_types derived_types in
						let item = `named (builtin_position, "", `variable (arg, None), no_attributes) in
						derived_types, item :: rs
					) (derived_types, []) args
				in
				let args = List.rev args in
				let ret, derived_types = type_map ret predefined_types derived_types in
				let t = `function_type (`cdecl, args, `none, ret) in
				let item = `named (builtin_position, name, `function_forward (`builtin, t), no_attributes) in
				let namespace = {namespace with ns_namespace = StringMap.add name item namespace.ns_namespace} in
				let source = item :: source in
				derived_types, namespace, source
			) ([], empty_namespace, []) builtin
		in
		let sources = StringMap.add builtin_name (builtin_source, no_extra_info) StringMap.empty in
		let derived_types, namespace, sources, _, mapping_options = handle_translation_unit error predefined_types derived_types namespace sources translation_unit in
		let derived_types, sources =
			StringMap.fold (fun name define (derived_types, sources) ->
				let (((current_filename, _, _, _), _), _) = define in
				let current_source, current_info =
					StringMap.find_or ~default:empty_source current_filename sources
				in
				let derived_types, current_source = handle_define error predefined_types derived_types namespace current_source mapping_options.mo_instances name define in
				let sources = StringMap.add current_filename (current_source, current_info) sources in
				derived_types, sources
			) defines (derived_types, sources)
		in
		let sources = StringMap.map (fun (items, info) -> List.rev items, info) sources in
		predefined_types, derived_types, namespace, sources, mapping_options
	);;
	
end;;
