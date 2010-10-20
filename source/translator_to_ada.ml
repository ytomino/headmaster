open C_semantics;;
open C_semantics_naming;;
open Value;;

let string_of_pp (pp: Format.formatter -> 'a -> unit) (v: 'a): string = (
	let b = Buffer.create 256 in
	let f = Format.formatter_of_buffer b in
	pp f v;
	Format.pp_print_flush f ();
	Buffer.contents b
);;

module StringSet = Set.Make (String);;

let ada_reserved_words =
	let list = [
		"abort";
		"abs";
		"access";
		"end";
		"exit";
		"in";
		"null";
		"out";
		"raise";
		"rem";
		"select";
		"type"]
	in List.fold_right StringSet.add list StringSet.empty;;

let ada_name_of_int_prec (p: int_prec): string = (
	begin match p with
	| `signed_char -> "signed_char"
	| `unsigned_char -> "unsigned_char"
	| `signed_short -> "signed_short"
	| `unsigned_short -> "unsigned_short"
	| `signed_int -> "signed_int"
	| `unsigned_int -> "unsigned_int"
	| `signed_long -> "signed_long"
	| `unsigned_long -> "unsigned_long"
	| `signed_long_long -> "signed_long_long"
	| `unsigned_long_long -> "unsigned_long_long"
	end
);;

let ada_name_of_float_prec (p: float_prec): string = (
	begin match p with
	| `float -> "float"
	| `double -> "double"
	| `long_double -> "long_double"
	end
);;

let ada_name_by_short (s: string): string = (
	let s_length = String.length s in
	let b = Buffer.create s_length in
	let rec loop s s_length i b state = (
		if i >= s_length then (
			Buffer.contents b
		) else (
			begin match s.[i] with
			| '_' ->
				begin match state with
				| `first ->
					loop s s_length (i + 1) b `first
				| `normal | `underline ->
					loop s s_length (i + 1) b `underline
				end
			| c ->
				if state = `underline then Buffer.add_char b '_';
				Buffer.add_char b c;
				loop s s_length (i + 1) b `normal
			end
		)
	) in
	loop s s_length 0 b `first
);;

let ada_name_by_substitute (s: string): string = (
	let substitute = 'q' in
	let s_length = String.length s in
	let b = Buffer.create s_length in
	let rec loop s s_length i b state = (
		if i >= s_length then (
			if state = `underline then Buffer.add_char b substitute;
			Buffer.contents b
		) else (
			begin match s.[i] with
			| '_' ->
				begin match state with
				| `substitute ->
					Buffer.add_char b substitute;
					loop s s_length (i + 1) b `substitute
				| `normal ->
					loop s s_length (i + 1) b `underline
				| `underline ->
					Buffer.add_char b substitute;
					Buffer.add_char b substitute;
					loop s s_length (i + 1) b `substitute
				end
			| '.' ->
				if state = `underline then Buffer.add_char b substitute;
				Buffer.add_char b '.';
				loop s s_length (i + 1) b `substitute
			| c ->
				if state = `underline then Buffer.add_char b '_';
				Buffer.add_char b c;
				loop s s_length (i + 1) b `normal
			end
		)
	) in
	loop s s_length 0 b `substitute
);;

let escape_ada_reserved_word ~(prefix: string) ~(postfix: string) (s: string): string = (
	if StringSet.mem (String.lowercase s) ada_reserved_words then (
		prefix ^ s ^ postfix
	) else (
		s
	)
);;

(* header filename (removed dir) -> package name *)
let special_package_name_mapping =
	let list = [] in (* currently, nothing *)
	List.fold_right (fun (k, v) r -> StringMap.add k v r) list StringMap.empty;;

let take_package_name = Triming.take_word (fun c -> c = '.');;

let ada_package_name (remove_include_dir: string -> string) (s: string): string = (
	if s.[0] = '<' then "" else
	let h = remove_include_dir s in
	begin try
		StringMap.find h special_package_name_mapping
	with Not_found ->
		let p = String.copy (Filename.chop_suffix h ".h") in
		for i = 0 to String.length p - 1 do
			if p.[i] = '/' then p.[i] <- '.' else
			if p.[i] = '-' then p.[i] <- '_'
		done;
		let p = ada_name_by_substitute p in
		(* resolving for confliction with reserved word *)
		let rec nested_package_loop p s = (
			let s, sr = take_package_name s in
			let s = escape_ada_reserved_word
				~prefix:""
				~postfix:(if sr = "" then "_h" else "_dir")
				s
			in
			if sr = "" then p ^ s else
			nested_package_loop (p ^ s ^ ".") sr
		) in
		nested_package_loop "" p
	end
);;

let make_mapmap (list: (string * (string * string) list) list): string StringMap.t StringMap.t = (
	List.fold_left (fun map (filename, xs) ->
		let set = List.fold_left (fun rs (k, v)-> StringMap.add k v rs) StringMap.empty xs in
		StringMap.add filename set map
	) StringMap.empty list
);;

(* package name -> C identifier -> Ada Identifier *)
let special_name_mapping = make_mapmap [
	"iconv", [
		"_LIBICONV_VERSION", "LIBICONV_VERSION"]; (* iconv / conflicted with extern const *)
	"inttypes", [
		"PRIX16", "PRIX16_uppercase"; (* darwin9 / conflicted with PRIx16 *)
		"PRIX32", "PRIX32_uppercase"; (* darwin9 / conflicted with PRIx32 *)
		"PRIX64", "PRIX64_uppercase"; (* darwin9 / conflicted with PRIx64 *)
		"PRIX8", "PRIX8_uppercase"; (* darwin9 / conflicted with PRIx8 *)
		"PRIXFAST16", "PRIXFAST16_uppercase"; (* darwin9 / conflicted with PRIxFAST16 *)
		"PRIXFAST32", "PRIXFAST32_uppercase"; (* darwin9 / conflicted with PRIxFAST32 *)
		"PRIXFAST64", "PRIXFAST64_uppercase"; (* darwin9 / conflicted with PRIxFAST64 *)
		"PRIXFAST8", "PRIXFAST8_uppercase"; (* darwin9 / conflicted with PRIxFAST8 *)
		"PRIXLEAST16", "PRIXLEAST16_uppercase"; (* darwin9 / conflicted with PRIxLEAST16 *)
		"PRIXLEAST32", "PRIXLEAST32_uppercase"; (* darwin9 / conflicted with PRIxLEAST32 *)
		"PRIXLEAST64", "PRIXLEAST64_uppercase"; (* darwin9 / conflicted with PRIxLEAST64 *)
		"PRIXLEAST8", "PRIXLEAST8_uppercase"; (* darwin9 / conflicted with PRIxLEAST8 *)
		"PRIXMAX", "PRIXMAX_uppercase"; (* darwin9 / conflicted with PRIxMAX *)
		"PRIXPTR", "PRIXPTR_uppercase"]; (* darwin9 / conflicted with PRIxPTR *)
	"mpfr", [
		"mpfr_version", "get_mpfr_version"]; (* mpdr / conflicted with MPFR_VERSION *)
	"sys.signal", [
		"sv_onstack", "sigvec_sv_onstack"]; (* darwin9 / conflicted with SV_ONSTACK *)
	"stdlib", [
		"system", "C_system"]; (* darwin9 / hiding System package (should use Standard prefix...) *)
	"unistd", [
		"_exit", "C_exit"; (* darwin9 / conflicted with _Exit *)
		"_Exit", "C_Exit2"; (* darwin9 / conflicted with _exit *)
		"execvP", "execvP2"]; (* darwin9 / conflicted with execvp *)
	"zlib", [
		"zlib_version", "get_zlib_version"]; (* zlib / conflicted with ZLIB_VERSION *)
	"", [ (* predefined *)
		"i386", "defined_i386"; (* darwin9 / conflicted with include dir <i386/...> *)
		"__MACH__", "defined_MACH"; (* darwin9 / conflicted with include dir <mach/...> *)
		"__PIC__", "PIC"]];; (* darwin9 / confilicted with __pic__ on gcc-4.4 *)

type precedence = [
	| `logical_and
	| `logical_and_then
	| `logical_or
	| `logical_or_else
	| `logical_xor
	| `relation
	| `simple
	| `term
	| `factor
	| `primary];;

type outside_precedence = [
	| precedence
	| `lowest
	| `simple_right
	| `term_right];;

let parenthesis_required
	~(outside: outside_precedence)
	~(inside: precedence)
	: bool =
(
	begin match outside with
	| `lowest ->
		false
	| `logical_and | `logical_and_then
	| `logical_or | `logical_or_else | `logical_xor as outside ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor as inside ->
			inside <> outside
		| `relation | `simple | `term | `factor | `primary ->
			false
		end
	| `relation | `simple ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor
		| `relation ->
			true
		| `simple | `term | `factor | `primary ->
			false
		end
	| `simple_right | `term ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor
		| `relation | `simple ->
			true
		| `term | `factor | `primary ->
			false
		end
	| `term_right ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor
		| `relation | `simple | `term ->
			true
		| `factor | `primary ->
			false
		end
	| `factor ->
		begin match inside with
		| `logical_and | `logical_and_then
		| `logical_or | `logical_or_else | `logical_xor
		| `relation | `simple | `term | `factor ->
			true
		| `primary ->
			false
		end
	| `primary ->
		true
	end
);;

module Translate
	(Literals: LiteralsType)
	(Semantics: SemanticsType (Literals).S) =
struct
	module Naming = Naming (Literals) (Semantics);;
	open Literals;;
	open Format;;
	
	(* name mapping *)
	
	let filename_without_ext_of_package_name (name: string): string = (
		if name = "" then "c" else
		let s = String.copy name in
		for i = 0 to String.length s - 1 do
			if s.[i] = '.' then s.[i] <- '-'
		done;
		"c-" ^ s
	);;
	
	let spec_filename (name: string): string = (
		filename_without_ext_of_package_name (String.lowercase name) ^ ".ads"
	);;
	
	let body_filename (name: string): string = (
		filename_without_ext_of_package_name (String.lowercase name) ^ ".adb"
	);;
	
	let filename_mapping = Naming.filename_mapping ada_package_name;;
	
	let dir_packages
		(filename_mapping: string StringMap.t)
		: string list =
	(
		let add_parent package rs = (
			begin try
				let rindex = String.rindex package '.' in
				let dir = String.sub package 0 rindex in
				if List.mem dir rs then rs else dir :: rs
			with Not_found ->
				rs
			end
		) in
		let rec loop xs = (
			let rs =
				List.fold_left (fun rs package ->
					add_parent package rs
				) xs xs
			in
			if rs == xs then rs else loop rs
		) in
		loop (StringMap.fold (fun _ -> add_parent) filename_mapping [])
	);;
	
	let items_per_package = Naming.items_per_module;;
	
	type name_mapping = Naming.name_mapping;;
	
	let name_mapping = Naming.name_mapping
		~long_f:(fun s -> escape_ada_reserved_word ~prefix:"C_" ~postfix:"" (ada_name_by_substitute s))
		~short_f:(fun s -> escape_ada_reserved_word ~prefix:"C_" ~postfix:"" (ada_name_by_short s))
		special_name_mapping;;
	
	let add_name_mapping_for_arguments = Naming.add_name_mapping_for_arguments
		~long_f:(fun s -> escape_ada_reserved_word ~prefix:"A_" ~postfix:"" (ada_name_by_substitute s))
		~short_f:(fun s -> escape_ada_reserved_word ~prefix:"A_" ~postfix:"" (ada_name_by_short s));;
	
	let name_mapping_for_struct_items = Naming.name_mapping_for_struct_items
		~long_f:(fun s -> escape_ada_reserved_word ~prefix:"F_" ~postfix:"" (ada_name_by_substitute s))
		~short_f:(fun s -> escape_ada_reserved_word ~prefix:"F_" ~postfix:"" (ada_name_by_short s));;
	
	let ada_simple_name_of
		(ps: ranged_position)
		(name: string)
		(kind: [`namespace | `opaque_enum | `opaque_struct | `opaque_union])
		(name_mapping: name_mapping)
		: string =
	(
		let (filename, _, _, _), _ = ps in
		let _, nspp = StringMap.find filename name_mapping in
		Naming.find kind name nspp
	);;
	
	let ada_name_of
		(refering_package: string)
		(ps: ranged_position)
		(name: string)
		(kind: [`namespace | `opaque_enum | `opaque_struct | `opaque_union])
		(name_mapping: name_mapping)
		: string =
	(
		let rec contained sub s level = (
			assert (sub <> "");
			let s, sr = take_package_name s in
			if s = sub then level else
			if sr = "" then -1 else
			contained sub sr (level + 1)
		) in
		let (filename, _, _, _), _ = ps in
		let package_name, nspp = StringMap.find filename name_mapping in
		let item_name = Naming.find kind name nspp in
		if refering_package = package_name || package_name = "" then item_name else
		let p, pr = take_package_name package_name in
		if pr <> "" && (
			let c = contained p refering_package 0 in
			c > 0 || (c = 0 && fst (take_package_name pr) = p))
		then (
			"Standard.C." ^ package_name ^ "." ^ item_name
		) else (
			package_name ^ "." ^ item_name
		)
	);;
	
	let hash_name (type t) (a: t): string = (
		let rec hash_loop (s: string) (i: int) (r: int32): int32 = (
			if i >= String.length s then r else
			let rotate_bits = 5 in
			let r = Int32.add (Int32.shift_left r rotate_bits) (Int32.shift_right r (32 - rotate_bits)) in
			let r = Int32.add r (Int32.of_int (int_of_char s.[i])) in
			hash_loop s (i + 1) r
		) in
		let image = Marshal.to_string a [] in (* is it unique really ??? *)
		let v = hash_loop image 0 0l in
		Format.sprintf "%.8lx" v
	);;
	
	(* type mapping *)
	
	let find_mapped_type
		(t: Semantics.type_item)
		(language_mapping: Semantics.language_mapping)
		: string =
	(
		List.assq t language_mapping.Semantics.lm_type
	);;
	
	let find_mapped_type_of_unconstrained_array
		(base_type: Semantics.not_qualified_type Semantics.item)
		(language_mapping: Semantics.language_mapping)
		: string =
	(
		snd (
			List.find (fun (t, _) ->
				begin match t with
				| `array (None, b), _ when b == base_type ->
					true
				| _ ->
					false
				end
			) language_mapping.Semantics.lm_type
		)
	);;
	
	let mapped_type_exists
		(t: Semantics.type_item)
		(language_mapping: Semantics.language_mapping)
		: bool =
	(
		List.mem_assq t language_mapping.Semantics.lm_type
	);;
	
	let rec fold_derived_types
		(f: 'a -> Semantics.derived_item -> 'a)
		(a: 'a)
		(base_type: Semantics.type_item)
		(derived_types: Semantics.derived_item list)
		: 'a =
	(
		List.fold_left (fun a dt ->
			begin match dt with
			| `pointer t, _ ->
				if t == base_type then f a dt else a
			| `array (_, t), _ ->
				if (t :> Semantics.type_item) == base_type then f a dt else a
			| `restrict t, _ ->
				if (t :> Semantics.type_item) == base_type then f a dt else a
			| `volatile t, _ ->
				if (t :> Semantics.type_item) == base_type then f a dt else a
			| `const t, _ ->
				if (t :> Semantics.type_item) == base_type then f a dt else a
			end
		) a derived_types
	);;
	
	(* dependency *)
	
	let referencing_packages_by_language_mapping
		~(language_mapping: Semantics.language_mapping)
		~(derived_types: Semantics.derived_item list)
		(items: Semantics.all_item list)
		: (string * [> `none]) list =
	(
		let rec process r (x: Semantics.type_item) = (
			let r =
				begin try
					let alias = find_mapped_type x language_mapping in
					let rindex = String.rindex alias '.' in
					let package_name = String.sub alias 0 rindex in
					if List.mem_assoc package_name r then r else
					(package_name, `none) :: r
				with Not_found -> (* find_mapped_type / String.rindex *)
					begin match fst x with
					| `__builtin_va_list
					| `pointer (`void, _)
					| `pointer (`const (`void, _), _) ->
						(* va_list / void * are mapped to System.Address *)
						let package_name = "System" in
						if List.mem_assoc package_name r then r else
						(package_name, `none) :: r
					| _ ->
						r
					end
				end
			in
			fold_derived_types (fun r x -> process r (x :> Semantics.type_item)) r x derived_types
		) in
		List.fold_left (fun r x ->
			begin match x with
			| #predefined_type, _
			| #Semantics.derived_type, _
			| #Semantics.anonymous_type, _
			| `named (_, _, #Semantics.named_type_var, _), _ as x ->
				process r x
			| _ ->
				r
			end
		) [] items
	);;
	
	let referencing_packages_from_depending
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(current: string)
		(items: Semantics.all_item list)
		: (string * [> `none]) list =
	(
		List.fold_left (fun r x ->
			begin match x with
			| `named (_, _, #Semantics.named_type_var, _), _ as t when mapped_type_exists t language_mapping ->
				r
			| `named (_, ("ptrdiff_t" | "size_t" | "wchar_t"), `typedef _, _), _ ->
				r
			| _, info ->
				List.fold_left (fun r d ->
					let `named (ps, _, _, _), _ = d in
					let package_name = Naming.module_of ps name_mapping in
					if package_name = current then r else
					let package_name = "C." ^ package_name in
					if List.mem_assoc package_name r then r else
					(package_name, `none) :: r
				) r info.Semantics.it_depending
			end
		) [] items
	);;
	
	let referencing_packages
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(derived_types: Semantics.derived_item list)
		~(current: string)
		(items: Semantics.all_item list)
		: (string * [> `none]) list =
	(
		let by_lm = referencing_packages_by_language_mapping ~language_mapping ~derived_types items in
		let by_de = referencing_packages_from_depending ~language_mapping ~name_mapping ~current items in
		List.sort compare (List.rev_append by_lm by_de)
	);;
	
	(* body requirement *)
	
	let body_required_for_single_item (item: Semantics.source_item): bool = (
		begin match item with
		| `named (_, _, `function_definition (sc, _, _), _), _ ->
			begin match sc with
			| `extern_inline -> false (* use extern version *)
			| `static | `none -> true (* static be placed into body *)
			end
		| `named (_, _, `defined_element_access _, _), _ ->
			true
		| `named (_, _, `defined_generic_expression _, _), _
		| `named (_, _, `defined_generic_statement _, _), _ ->
			false (* unimplemented *)
		| `named (_, _, `defined_expression expr, _), _ ->
			not (Semantics.is_static_expression expr)
		| _ ->
			false
		end
	);;
	
	let body_required (items: Semantics.source_item list): bool = (
		List.exists body_required_for_single_item items
	);;
	
	(* expression /statment *)
	
	let collect_assign_in_stmt
		(rs: Semantics.any_assignment_expression list)
		(stmt: Semantics.statement)
		: Semantics.any_assignment_expression list =
	(
		let stmt_f (included, excluded as result) stmt = (
			let module L = struct type aaev = Semantics.any_assignment_expression_var end in
			begin match stmt with
			| `expression (#L.aaev, _ as e) ->
				included, e :: excluded
			| `for_loop (Some (#L.aaev, _ as e1), _, Some (#L.aaev, _ as e2), _) ->
				included, e1 :: e2 :: excluded
			| `for_loop (Some (#L.aaev, _ as e), _, _, _) ->
				included, e :: excluded
			| `for_loop (_, _, Some (#L.aaev, _ as e), _) ->
				included, e :: excluded
			| _ ->
				result
			end
		) in
		let expr_f (included, excluded as result) expr = (
			begin match expr with
			| #Semantics.any_assignment_expression_var, _ as e ->
				e :: included, excluded
			| _ ->
				result
			end
		) in
		let included, excluded = Semantics.fold_statement stmt_f expr_f (rs, []) stmt in
		List.rev (List.filter (fun x -> not (List.memq x excluded)) included)
	);;
	
	let collect_cond_in_stmt
		(rs: Semantics.conditional_expression list)
		(stmt: Semantics.statement)
		: Semantics.conditional_expression list =
	(
		let stmt_f result (_: Semantics.statement) = result in
		let expr_f result expr = (
			begin match expr with
			| #Semantics.conditional_expression_var, _ as e ->
				e :: result
			| _ ->
				result
			end
		) in
		Semantics.fold_statement stmt_f expr_f rs stmt
	);;
	
	let collect_cast
		(rs : (Semantics.type_item * Semantics.type_item) list)
		(item: Semantics.source_item)
		: (Semantics.type_item * Semantics.type_item) list =
	(
		let add t1 t2 rs = (
			if List.exists (fun (u1, u2) -> u1 == t1 && u2 == t2) rs then (
				rs
			) else (
				(t1, t2) :: rs
			)
		) in
		let stmt_f rs _ = rs in
		let expr_f rs e = (
			begin match e with
			| `cast (_, (#int_prec, _) as expr), (`pointer (`void, _), _)
				when Semantics.is_static_expression expr
			->
				rs (* use System'To_Address *)
			| `cast (_, t1), (`pointer _, _ as t2)
			| `cast (_, (`pointer _, _ as t1)), t2
				when not (Semantics.is_generic_type t1) && not (Semantics.is_generic_type t2)
			->
				add t1 t2 rs
			| _ ->
				rs
			end
		) in
		begin match item with
		| `named (_, _, `function_definition (_, _, stmts), _), _ ->
			List.fold_left (Semantics.fold_statement stmt_f expr_f) rs stmts
		| `named (_, _, `defined_expression expr, _), _ ->
			Semantics.fold_expression stmt_f expr_f rs expr
		| `named (_, _, `defined_generic_expression (_, _, _, expr), _), _ ->
			Semantics.fold_expression stmt_f expr_f rs expr
		| `named (_, _, `defined_generic_statement (_, _, _, stmt), _), _ ->
			Semantics.fold_statement stmt_f expr_f rs stmt
		| _ ->
			rs
		end
	);;
	
	let collect_pointer_arithmetic
		(ptrdiff_t: Semantics.type_item)
		(rs : (Semantics.type_item * Semantics.type_item) list)
		(item: Semantics.source_item)
		: (Semantics.type_item * Semantics.type_item) list =
	(
		let add t1 t2 rs = (
			if List.exists (fun (u1, u2) -> u1 == t1 && u2 == t2) rs then (
				rs
			) else (
				(t1, t2) :: rs
			)
		) in
		let stmt_f rs _ = rs in
		let expr_f rs e = (
			begin match e with
			| `increment _, t
			| `decrement _, t
			| `post_increment _, t
			| `post_decrement _, t when Semantics.is_pointer t ->
				add t ptrdiff_t (add ptrdiff_t t rs)
			| _ ->
				rs
			end
		) in
		begin match item with
		| `named (_, _, `function_definition (_, _, stmts), _), _ ->
			List.fold_left (Semantics.fold_statement stmt_f expr_f) rs stmts
		| `named (_, _, `defined_expression expr, _), _ ->
			Semantics.fold_expression stmt_f expr_f rs expr
		| `named (_, _, `defined_generic_expression (_, _, _, expr), _), _ ->
			Semantics.fold_expression stmt_f expr_f rs expr
		| `named (_, _, `defined_generic_statement (_, _, _, stmt), _), _ ->
			Semantics.fold_statement stmt_f expr_f rs stmt
		| _ ->
			rs
		end
	);;
	
	let has_asm (item: Semantics.source_item): bool = (
		begin match item with
		| `named (_, _, `function_definition (_, _, stmts), _), _ ->
			List.exists Semantics.has_asm stmts
		| `named (_, _, `defined_generic_statement (_, _, _, stmt), _), _ ->
			Semantics.has_asm stmt
		| _ ->
			false
		end
	);;
	
	let prototype_for_element_access
		(ps: ranged_position)
		(t: Semantics.struct_or_union_type Semantics.item)
		(route: Semantics.struct_item list)
		: Semantics.prototype =
	(
		let last_route = List.fold_left (fun _ r -> r) (List.hd route) route in
		let _, result_type, _, _ = last_route in
		let args = [
			`named (ps, "Object", `variable ((t :> Semantics.type_item), None), Semantics.no_attributes),
			{Semantics.it_depending = []}]
		in
		`cdecl, args, `none, result_type
	);;
	
	(* pretty printer *)
	
	let indent = 3;;
	
	let pp_notification (ff: formatter) (version: string): unit = (
		fprintf ff "--  This file was translated by \"headmaster\" %s.@." version;
		fprintf ff "--  The original C header\'s license should be applied to this file.@.";
		fprintf ff "--  \"headmaster\" is only just editor tool. The accuracy of the translation is@.";
		fprintf ff "--    not guaranteed. The person that assume the responsibility of damage is@.";
		fprintf ff "--    you(author of an application), is not me(author of the tool = yt)@.";
		fprintf ff "--  All conditional-directives were expanded for the exclusive use of your@.";
		fprintf ff "--    environment, it is not recommended to commit this file to any repository.@.";
		fprintf ff "-------------------------------------------------------------------------------@."
	);;
	
	type package_kind = [
		| `pure
		| `preelaborate
		| `normal];;
	
	let pp_with_caluse
		(ff: formatter)
		(package, use: string * [`use | `none])
		: unit =
	(
		pp_open_box ff indent;
		fprintf ff "with %s;" package;
		begin match use with
		| `use ->
			fprintf ff "@ use %s;" package
		| `none ->
			()
		end;
		pp_close_box ff ();
		pp_print_break ff 0 0
	);;
	
	let pp_package_spec
		(ff: formatter)
		~(with_packages: (string * [`use | `none]) list)
		~(name: string)
		~(kind: package_kind)
		~(pp_contents: formatter -> unit)
		~(pp_private: (formatter -> unit) option)
		: unit =
	(
		pp_open_vbox ff 0;
		List.iter (pp_with_caluse ff) with_packages;
		pp_open_vbox ff indent;
		fprintf ff "package %s is" name;
		begin match kind with
		| `pure -> fprintf ff "@ pragma Pure;"
		| `preelaborate -> fprintf ff "@ pragma Preelaborate;"
		| `normal -> ()
		end;
		pp_contents ff;
		pp_close_box ff ();
		begin match pp_private with
		| Some pp_private ->
			pp_print_break ff 0 0;
			pp_open_vbox ff indent;
			pp_print_string ff "private";
			pp_private ff;
			pp_close_box ff ()
		| None ->
			()
		end;
		fprintf ff "@ end %s;@," name;
		pp_close_box ff ()
	);;
	
	let pp_package_body
		(ff: formatter)
		~(with_packages: (string * [`use | `none]) list)
		~(name: string)
		~(pp_contents: formatter -> unit)
		: unit =
	(
		pp_open_vbox ff 0;
		List.iter (pp_with_caluse ff) with_packages;
		pp_open_vbox ff indent;
		fprintf ff "package body %s is" name;
		pp_contents ff;
		pp_close_box ff ();
		fprintf ff "@ end %s;@," name;
		pp_close_box ff ()
	);;
	
	let pp_convention
		(ff: formatter)
		(conv: [< `ada | `intrinsic | `c_pass_by_copy | calling_convention])
		: unit =
	(
		pp_print_string ff (
			begin match conv with
			| `ada -> "Ada"
			| `intrinsic -> "Intrinsic"
			| `cdecl -> "C"
			| `c_pass_by_copy -> "C_Pass_By_Copy"
			| `stdcall -> "Stdcall"
			| `fastcall -> "Fastcall" (* will be error by gnat *)
			end
		)
	);;
	
	let pp_pragma_convention
		(ff: formatter)
		(conv: [< `ada | `intrinsic | `c_pass_by_copy | calling_convention])
		(ada_name: string)
		: unit =
	(
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "pragma Convention (%a,@ %s);"
			pp_convention conv
			ada_name;
		pp_close_box ff ()
	);;
	
	let pp_pragma_convention_identifier
		(ff: formatter)
		(alias: string)
		(conv: [< `ada | `intrinsic | `c_pass_by_copy | calling_convention])
		: unit =
	(
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "pragma Convention_Identifier (%s,@ %a);"
			alias
			pp_convention conv;
		pp_close_box ff ()
	);;
	
	let pp_pragma_import
		(ff: formatter)
		(conv: [< `ada | `intrinsic | `c_pass_by_copy | calling_convention])
		(ada_name: string)
		(c_name: string)
		: unit =
	(
		pp_print_space ff ();
		pp_open_box ff indent;
		if c_name.[0] = '\"' then (
			fprintf ff "pragma Import (%a,@ %s,@ %s);"
				pp_convention conv
				ada_name
				c_name
		) else if
			match conv with
			| `intrinsic -> ada_name = c_name
			| `ada | `c_pass_by_copy | #calling_convention -> false
		then (
			fprintf ff "pragma Import (%a,@ %s);"
				pp_convention conv
				ada_name
		) else (
			fprintf ff "pragma Import (%a,@ %s,@ \"%s\");"
				pp_convention conv
				ada_name
				c_name
		);
		pp_close_box ff ()
	);;
	
	let pp_pragma_noreturn
		(ff: formatter)
		(ada_name: string)
		: unit =
	(
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "pragma No_Return (%s);" ada_name;
		pp_close_box ff ()
	);;
	
	let pp_if
		(ff: formatter)
		~(pp_cond: formatter -> unit -> unit)
		~(pp_true_case: formatter -> unit -> unit)
		~(pp_false_case: (formatter -> unit -> unit) option)
		: unit =
	(
		pp_print_space ff ();
		pp_open_vbox ff indent;
		pp_open_box ff 0;
		pp_open_box ff indent;
		pp_print_string ff "if ";
		pp_cond ff ();
		pp_close_box ff ();
		fprintf ff "@ then";
		pp_close_box ff ();
		pp_true_case ff ();
		pp_close_box ff ();
		begin match pp_false_case with
		| Some pp_false_case ->
			pp_print_space ff ();
			pp_open_vbox ff indent;
			fprintf ff "else";
			pp_false_case ff ();
			pp_close_box ff ();
		| None ->
			()
		end;
		fprintf ff "@ end if;"
	);;
	
	let pp_predefined_type_name
		(ff: formatter)
		(item: Semantics.predefined_item)
		: unit =
	(
		begin match fst item with
		| `void ->
			pp_print_string ff "void";
		| `bool ->
			pp_print_string ff "bool";
		| #int_prec as p ->
			pp_print_string ff (ada_name_of_int_prec p)
		| #float_prec as p ->
			pp_print_string ff (ada_name_of_float_prec p)
		| `decimal32 ->
			pp_print_string ff "decimal32"
		| `decimal64 ->
			pp_print_string ff "decimal64"
		| `decimal128 ->
			pp_print_string ff "decimal128"
		| `imaginary e ->
			fprintf ff "%s_imaginary" (ada_name_of_float_prec e)
		| `complex e ->
			fprintf ff "%s_complex" (ada_name_of_float_prec e)
		| `char ->
			pp_print_string ff "char"
		| `wchar ->
			pp_print_string ff "wchar_t"
		| `__builtin_va_list ->
			pp_print_string ff "builtin_va_list"
		end
	);;
	
	type where = [`extern | `name | `argument | `subtype];;
	
	let rec pp_derived_type_name
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		~(where: where)
		(item: Semantics.derived_item)
		: unit =
	(
		let pp_pointer_type_name ff ~name_mapping ~where ~restrict t = (
			if where = `argument && (
				begin match t with 
				| `void, _
				| `const (`void, _), _
				| `named (_, _, `typedef (`function_type _, _), _), _ ->
					false
				| _ ->
					true
				end)
			then (
				begin match t with
				| `function_type _ (* prototype *), _ as t ->
					(* pragma Convention does not reach to anonymous access to subprogram... *)
					let unique_key = List.assq t anonymous_mapping in
					fprintf ff "access_%s" unique_key
				| `const t, _ ->
					fprintf ff "access constant %a"
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) (t :> Semantics.type_item)
				| _ ->
					fprintf ff "access %a"
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
				end
			) else (
				let postfix = if restrict then "_restrict_ptr" else "_ptr" in
				begin match t with
				| `function_type _, _ as t ->
					let unique_key = List.assq t anonymous_mapping in
					fprintf ff "access_%s" unique_key
				| `const t, _ ->
					fprintf ff "%a_const%s"
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`name) (t :> Semantics.type_item)
						postfix
				| _ ->
					fprintf ff "%a%s"
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`name) t
						postfix
				end
			)
		) in
		begin match item with
		| `pointer t, _ ->
			pp_pointer_type_name ff ~name_mapping ~where ~restrict:false t
		| `array (n, t), _ ->
			if where = `argument then (
				pp_pointer_type_name ff ~name_mapping ~where ~restrict:false (t :> Semantics.type_item)
			) else (
				fprintf ff "%a_array"
					(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`name) (t :> Semantics.type_item);
				if where = `subtype || where = `extern then (
					begin match n with
					| Some n ->
						fprintf ff " (0 .. %d)" (Integer.to_int n - 1)
					| None ->
						if where = `extern then (
							fprintf ff " (size_t)"
						)
					end
				)
			)
		| `restrict (`pointer t, _), _ ->
			pp_pointer_type_name ff ~name_mapping ~where ~restrict:true t
		| `volatile _, _ ->
			fprintf ff "@ **** volatile / unimplemented. ****\n";
			assert false
		| `const t, _ ->
			pp_type_name ff ~name_mapping ~anonymous_mapping ~current ~where (t :> Semantics.type_item)
		end
	) and pp_anonymous_type_name
		(ff: formatter)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		(item: Semantics.anonymous_item)
		: unit =
	(
		let unique_key = List.assq item anonymous_mapping in
		begin match item with
		| `anonymous (_, `enum _), _ ->
			fprintf ff "enum_%s" unique_key
		| `anonymous (_, `struct_type _), _ ->
			fprintf ff "struct_%s" unique_key
		| `anonymous (_, `union _), _ ->
			fprintf ff "union_%s" unique_key
		| `function_type _, _ ->
			assert false (* does not come here *)
		end
	) and pp_type_name
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		~(where: where)
		(item: Semantics.type_item)
		: unit =
	(
		begin match item with
		| #predefined_type, _ as item ->
			pp_predefined_type_name ff item
		| #Semantics.anonymous_type, _ as item ->
			pp_anonymous_type_name ff ~anonymous_mapping item
		| #Semantics.derived_type, _ as item ->
			pp_derived_type_name ff ~name_mapping ~anonymous_mapping ~current ~where item
		| `named (_, _, `typedef (#Semantics.derived_type, _ as item), _), _ when where = `argument ->
			pp_derived_type_name ff ~name_mapping ~anonymous_mapping ~current ~where item
		| `named (ps, name, (`opaque_enum | `enum _), _), _ ->
			let name = ada_name_of current ps name `opaque_enum name_mapping in
			pp_print_string ff name
		| `named (ps, name, (`opaque_struct | `struct_type _), _), _ ->
			let name = ada_name_of current ps name `opaque_struct name_mapping in
			pp_print_string ff name
		| `named (ps, name, (`opaque_union | `union _), _), _ ->
			let name = ada_name_of current ps name `opaque_union name_mapping in
			pp_print_string ff name
		| `named (ps, ("ptrdiff_t" | "size_t" | "wchar_t" as name), _, _), _
			when (let (filename, _, _, _), _ = ps in filename.[0] = '<')
		->
			pp_print_string ff name
		| `named (ps, name, _, _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_string ff name
		end
	) and pp_prototype
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		~(name: string option)
		(prototype: Semantics.prototype)
		: unit =
	(
		let pp_args ff args = (
			if args <> [] then (
				pp_print_string ff " (";
				let (_: int) =
					List.fold_left (fun num arg ->
						if num >= 2 then fprintf ff ";@ " else pp_print_break ff 0 0;
						let `named (ps, arg_name, `variable (arg_t, _), _), _ = arg in
						let arg_name =
							if arg_name = "" then (
								"A" ^ string_of_int num
							) else (
								ada_simple_name_of ps arg_name `namespace name_mapping
							)
						in
						fprintf ff "%s : %a" arg_name
							(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`argument) arg_t;
						num + 1
					) 1 args
				in
				pp_print_string ff ")"
			)
		) in
		let _, args, _, ret = prototype in
		if fst ret = `void then (
			pp_print_string ff "procedure";
			begin match name with
			| Some name -> fprintf ff " %s" name;
			| None -> ()
			end;
			pp_args ff args
		) else (
			pp_print_string ff "function";
			begin match name with
			| Some name -> fprintf ff " %s" name;
			| None -> ()
			end;
			pp_args ff args;
			fprintf ff "@ return %a"
				(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) ret
		)
	);;
	
	let pp_predefined_type
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		(item: Semantics.predefined_item)
		: unit =
	(
		begin try
			let alias = find_mapped_type (item :> Semantics.type_item) language_mapping in
			fprintf ff "@ subtype %a is %s;" pp_predefined_type_name item alias
		with Not_found ->
			begin match fst item with
			| `void ->
				fprintf ff "@ --  type void (<>) is limited private;"
			| `bool ->
				fprintf ff "@ type bool is new Boolean;";
				pp_pragma_convention ff `cdecl "bool"
			| #int_prec as p ->
				let body =
					begin match p with
					| `signed_char -> "new Short_Short_Integer"
					| `unsigned_char -> "mod 2 ** signed_char'Size"
					| `signed_short -> "new Short_Integer"
					| `unsigned_short -> "mod 2 ** signed_short'Size"
					| `signed_int -> "new Integer"
					| `unsigned_int -> "mod 2 ** signed_int'Size"
					| `signed_long -> "new Long_Integer"
					| `unsigned_long -> "mod 2 ** signed_long'Size"
					| `signed_long_long -> "new Long_Long_Integer"
					| `unsigned_long_long -> "mod 2 ** signed_long_long'Size"
					end
				in
				let name = ada_name_of_int_prec p in
				fprintf ff "@ type %s is %s;" name body;
				pp_pragma_convention ff `cdecl name
			| #float_prec as p ->
				let body =
					begin match p with
					| `float -> "new Standard.Float"
					| `double -> "new Long_Float"
					| `long_double -> "new Long_Long_Float"
					end
				in
				let name = ada_name_of_float_prec p in
				fprintf ff "@ type %s is %s;" name body;
				pp_pragma_convention ff `cdecl name
			| `decimal32 ->
				fprintf ff "@ --  type decimal32 is ..."
			| `decimal64 ->
				fprintf ff "@ --  type decimal64 is ..."
			| `decimal128 ->
				fprintf ff "@ --  type decimal128 is ..."
			| `imaginary e ->
				let e_name = ada_name_of_float_prec e in
				let name = e_name ^ "_imaginary" in
				fprintf ff "@ type %s is new %s;" name e_name;
				pp_pragma_convention ff `cdecl name
			| `complex e ->
				let e_name = ada_name_of_float_prec e in
				let name = e_name ^ "_complex" in
				pp_print_space ff ();
				pp_open_vbox ff indent;
				fprintf ff "type %s is record" name;
				fprintf ff "@ Re, Im : %s'Base;" e_name;
				pp_close_box ff ();
				fprintf ff "@ end record;";
				fprintf ff "@ pragma Complex_Representation (%s);" name;
				pp_pragma_convention ff `cdecl name
			| `char ->
				fprintf ff "@ type char is new Character;";
				(* dirty hack: pragma Convention set word-size alignment...so omitted... *)
			| `wchar ->
				fprintf ff "@ type wchar_t is new Wide_Character;";
				(* dirty hack: pragma Convention set word-size alignment...so omitted... *)
			| `__builtin_va_list ->
				fprintf ff "@ type builtin_va_list is new System.Address;"
			end
		end;
		begin match fst item with
		| #int_prec as p ->
			let name = ada_name_of_int_prec p in
			pp_print_space ff ();
			pp_open_box ff indent;
			fprintf ff "function Shift_Left (Left : %s; Right : Natural)@ return %s;" name name;
			pp_close_box ff ();
			begin match p with
			| #signed_int_prec ->
				pp_print_space ff ();
				pp_open_box ff indent;
				fprintf ff "function Shift_Right_Arithmetic (Left : %s; Right : Natural)@ return %s;" name name;
				pp_close_box ff ()
			| #unsigned_int_prec ->
				pp_print_space ff ();
				pp_open_box ff indent;
				fprintf ff "function Shift_Right (Left : %s; Right : Natural)@ return %s;" name name;
				pp_close_box ff ()
			end
		| _ ->
			()
		end
	);;
	
	let pp_newtype_of_predefind_type
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		(name: string)
		(t: Semantics.predefined_item)
		: unit =
	(
		begin try
			let _, alias =
				List.find (fun (x, _) ->
					begin match x with
					| `named (_, "size_t", _, _), _ -> true
					| _ -> false
					end
				) language_mapping.Semantics.lm_type
			in
			fprintf ff "@ subtype %s is %s;" name alias
		with Not_found ->
			fprintf ff "@ type %s is new %a;" name pp_predefined_type_name t
		end
	);;
	
	let rec pp_derived_type
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(item: Semantics.derived_item)
		: unit =
	(
		begin try
			let alias = find_mapped_type (item :> Semantics.type_item) language_mapping in
			fprintf ff "@ subtype %a is %s;"
				(pp_derived_type_name ~name_mapping ~anonymous_mapping ~current ~where:`name) item
				alias
		with Not_found ->
			let pp_pointer_type ff ~name_mapping name ~restrict t = (
				pp_print_space ff ();
				begin match t with
				| `function_type prototype, _
				| `named (_, _, `typedef (`function_type prototype, _), _), _ ->
					pp_open_box ff indent;
					let _, args, _, _ = prototype in
					let local = add_name_mapping_for_arguments args name_mapping in
					fprintf ff "type %s is access %a;" name
						(pp_prototype ~name_mapping:local ~anonymous_mapping ~current ~name:None) prototype;
					pp_close_box ff ();
					let conv, _, _, _ = prototype in
					pp_pragma_convention ff conv name
				| `void, _
				| `const (`void, _), _ ->
					fprintf ff "type %s is new System.Address;" name
				| _ ->
					pp_open_box ff indent;
					begin match t with
					| `const t, _ ->
						fprintf ff "type %s is@ access constant %a;" name
							(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) (t :> Semantics.type_item);
					| _ ->
						fprintf ff "type %s is@ access all %a;" name
							(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
					end;
					pp_close_box ff ();
					fprintf ff "@ for %s'Storage_Size use 0;" name;
					if not restrict then (
						fprintf ff "@ pragma No_Strict_Aliasing (%s);" name
					);
					pp_pragma_convention ff `cdecl name
				end
			) in
			begin match item with
			| `pointer t, _ ->
				let name = string_of_pp
					(pp_derived_type_name ~name_mapping ~anonymous_mapping ~current ~where:`name) item
				in
				pp_pointer_type ff ~name_mapping name ~restrict:false t
			| `array (_, base_type), _ ->
				let base_name = string_of_pp
					(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`name) (base_type :> Semantics.type_item)
				in
				let name = base_name ^ "_array" in
				begin try
					let alias = find_mapped_type_of_unconstrained_array base_type language_mapping in
					fprintf ff "@ subtype %s is %s;" name alias
				with Not_found ->
					pp_print_space ff ();
					pp_open_box ff indent;
					fprintf ff "type %s is array (size_t range <>) of@ aliased %s;" name base_name;
					pp_close_box ff ();
					pp_pragma_convention ff `cdecl name
				end
			| `restrict (`pointer t, _), _ ->
				let name = string_of_pp
					(pp_derived_type_name ~name_mapping ~anonymous_mapping ~current ~where:`name) item
				in
				pp_pointer_type ff ~name_mapping name ~restrict:true t
			| `volatile _, _ ->
				fprintf ff "@ **** volatile / unimplemented. ****\n";
				assert false
			| `const _, _ ->
				() (* only "access constant" form *)
			end
		end
	);;
	
	let pp_unchecked_conversion
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(t1, t2: Semantics.type_item * Semantics.type_item)
		: unit =
	(
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "function Cast is@ new Ada.Unchecked_Conversion (";
		pp_type_name ff ~name_mapping ~anonymous_mapping ~current ~where:`name t1;
		fprintf ff ", ";
		pp_type_name ff ~name_mapping ~anonymous_mapping ~current ~where:`name t2;
		fprintf ff ");";
		pp_close_box ff ()
	);;
	
	let rec pp_derived_types_for_the_type
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(casts: (Semantics.type_item * Semantics.type_item) list)
		~(current: string)
		(base_type: Semantics.type_item)
		(derived_types: Semantics.derived_item list)
		: unit =
	(
		let (_: 'a list) =
			List.fold_left (fun arrays dt ->
				let process () = (
					pp_derived_type ff ~language_mapping ~name_mapping ~anonymous_mapping ~current dt;
					List.iter (fun (x, y as pair) ->
						if x == (dt :> Semantics.type_item) || y == (dt :> Semantics.type_item) then (
							pp_unchecked_conversion ff ~name_mapping ~anonymous_mapping ~current pair
						)
					) casts;
					pp_derived_types_for_the_type ff ~language_mapping ~name_mapping ~anonymous_mapping ~casts ~current
						(dt :> Semantics.type_item) derived_types
				) in
				begin match dt with
				| `pointer t, _ ->
					if t == base_type then process ();
					arrays
				| `array (_, t), _ ->
					if (t :> Semantics.type_item) == base_type && not (List.memq base_type arrays) then (
						process ();
						base_type :: arrays
					) else (
						arrays
					)
				| `restrict t, _ ->
					if (t :> Semantics.type_item) == base_type then process ();
					arrays
				| `volatile t, _ ->
					if (t :> Semantics.type_item) == base_type then process ();
					arrays
				| `const t, _ ->
					if (t :> Semantics.type_item) == base_type then process ();
					arrays
				end
			) [] derived_types
		in
		()
	);;
	
	let pp_enum
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(name: string)
		(t: Semantics.full_enum_item)
		: unit =
	(
		let items =
			begin match t with
			| `anonymous (_, `enum items), _ ->
				items
			| `named (_, _, `enum items, _), _ ->
				items
			end
		in
		(* split elements has same representation *)
		let items, duplicated =
			List.fold_left (fun (items, duplicated) x ->
				let `named (_, _, `enum_element x_value, _), _ = x in
				if
					List.exists (fun y ->
						let `named (_, _, `enum_element y_value, _), _ = y in
						y_value = x_value
					) items
				then (
					items, x :: duplicated
				) else (
					x :: items, duplicated
				)
			) ([], []) items
		in
		(* sort by representation *)
		let items =
			List.sort (fun x y ->
				let `named (_, _, `enum_element x_value, _), _ = x in
				let `named (_, _, `enum_element y_value, _), _ = y in
				Integer.compare x_value y_value
			) items
		in
		(* source order *)
		let duplicated = List.rev duplicated in
		(* printing type *)
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "type %s is (" name;
		let rec loop index xs = (
			begin match xs with
			| (`named (item_ps, item_name, _, _), _) :: xr ->
				if index > 0 then fprintf ff ",@ ";
				let item_name = ada_name_of current item_ps item_name `namespace name_mapping in
				pp_print_string ff item_name;
				loop (index + 1) xr
			| _ ->
				()
			end
		) in
		loop 0 items;
		fprintf ff ");";
		pp_close_box ff ();
		pp_print_space ff ();
		pp_open_box ff indent;
		fprintf ff "for %s use (" name;
		let rec loop index xs = (
			begin match xs with
			| (`named (item_ps, item_name, `enum_element repr, _), _) :: xr ->
				if index > 0 then fprintf ff ",@ ";
				let item_name = ada_name_of current item_ps item_name `namespace name_mapping in
				fprintf ff "%s => %s" item_name (Integer.to_based_string ~base:10 repr);
				loop (index + 1) xr
			| _ ->
				()
			end
		) in
		loop 0 items;
		fprintf ff ");";
		pp_close_box ff ();
		pp_pragma_convention ff `cdecl name;
		(* printing duplicated *)
		List.iter (fun x ->
			let `named (item_ps, item_name, `enum_element x_value, _), _ = x in
			let item_name =
				ada_name_of current item_ps item_name `namespace name_mapping
			in
			let prototype = `cdecl, [], `none, (t :> Semantics.type_item) in
			pp_print_space ff ();
			pp_open_box ff indent;
			pp_prototype ff ~name_mapping ~anonymous_mapping
				~current ~name:(Some item_name) prototype;
			let same_repr_item_name =
				let y =
					List.find (fun y ->
						let `named (_, _, `enum_element y_value, _), _ = y in
						y_value = x_value
					) items
				in
				let `named (item_ps, item_name, _, _), _ = y in
				ada_name_of current item_ps item_name `namespace name_mapping
			in
			fprintf ff "@ renames %s;" same_repr_item_name;
			pp_close_box ff ()
		) duplicated
	);;
	
	let pp_struct
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(name: string)
		(items: Semantics.struct_item list)
		(attributes: Semantics.attributes)
		: unit =
	(
		let field_map = name_mapping_for_struct_items items in
		pp_print_space ff ();
		pp_open_vbox ff indent;
		fprintf ff "type %s is " name;
		if items = [] then (
			fprintf ff "null record;";
			pp_close_box ff ()
		) else (
			fprintf ff "record";
			List.iter (fun (item_name, item_type, bits, _) ->
				if item_name <> "" then (
					let item_name = StringMap.find item_name field_map in
					fprintf ff "@ %s : " item_name;
					begin match bits with
					| Some _ ->
						()
					| None ->
						pp_print_string ff "aliased "
					end;
					pp_type_name ff ~name_mapping ~anonymous_mapping ~current ~where:`subtype item_type;
					begin match bits with
					| Some bits ->
						begin match Semantics.resolve_typedef item_type with
						| #signed_int_prec, _ ->
							fprintf ff " range %d .. %d" 
								(- (1 lsl (bits - 1)))
								(1 lsl (bits - 1) - 1)
						| #unsigned_int_prec, _ ->
							fprintf ff " range 0 .. %d"
								(1 lsl bits - 1)
						| _ ->
							assert false (* bit field has non-integer type *)
						end
					| None ->
						()
					end;
					pp_print_char ff ';'
				)
			) items;
			pp_close_box ff ();
			fprintf ff "@ end record;"
		);
		let is_bit_field =
			begin match items with
			| (_, _, Some _, _) :: _ -> true
			| (_, _, None, _) :: _ | [] -> false
			end
		in
		if is_bit_field then (
			pp_print_space ff ();
			pp_open_vbox ff indent;
			fprintf ff "for %s use record" name;
			let total_bits =
				List.fold_left (fun offset (item_name, _, bits, _) ->
					let bits =
						begin match bits with
						| Some bits -> bits
						| None -> assert false
						end
					in
					if item_name <> "" then (
						let item_name = StringMap.find item_name field_map in
						fprintf ff "@ %s at %d range %d .. %d;" item_name
							0
							offset
							(offset + bits - 1)
					);
					offset + bits
				) 0 items
			in
			pp_close_box ff ();
			fprintf ff "@ end record;";
			fprintf ff "@ for %s'Size use %d;" name total_bits;
			begin match attributes.Semantics.at_aligned with
			| `default ->
				()
			| `explicit_aligned | `aligned _ | `packed ->
				fprintf ff "@ **** alignment / unimplemented ****\n";
				assert false
			end
		) else (
			begin match attributes.Semantics.at_aligned with
			| `default ->
				()
			| `explicit_aligned ->
				fprintf ff "@ for %s'Alignment use Standard'Maximum_Alignment;" name (* ??? *) 
			| `aligned n ->
				fprintf ff "@ for %s'Alignment use %d * Standard'Storage_Unit;" name n
			| `packed ->
				fprintf ff "@ pragma Pack (%s);" name
			end
		);
		pp_pragma_convention ff `c_pass_by_copy name
	);;
	
	let pp_union
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(name: string)
		(items: Semantics.struct_item list)
		: unit =
	(
		let field_map = name_mapping_for_struct_items items in
		pp_print_space ff ();
		pp_open_vbox ff indent;
		fprintf ff "type %s (Unchecked_Tag : unsigned_int := 0) is " name;
		if items = [] then (
			fprintf ff "null record;";
			pp_close_box ff ()
		) else (
			fprintf ff "record@ ";
			pp_open_vbox ff indent;
			fprintf ff "case Unchecked_Tag is";
			let rec loop index xs = (
				begin match xs with
				| (item_name, item_type, _, _) :: xr ->
					pp_print_space ff ();
					pp_open_vbox ff indent;
					if xr <> [] then (
						fprintf ff "when %d =>" index
					) else (
						fprintf ff "when others =>"
					);
					let item_name = StringMap.find item_name field_map in
					fprintf ff "@ %s : %a;" item_name
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) item_type;
					pp_close_box ff ();
					loop (index + 1) xr
				| [] ->
					()
				end
			) in
			loop 0 items;
			pp_close_box ff ();
			fprintf ff "@ end case;";
			pp_close_box ff ();
			fprintf ff "@ end record;"
		);
		fprintf ff "@ pragma Unchecked_Union (%s);" name;
		pp_pragma_convention ff `c_pass_by_copy name
	);;
	
	let pp_anonymous_type
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(item: Semantics.anonymous_item)
		: unit =
	(
		let unique_key = List.assq item anonymous_mapping in
		begin match item with
		| `anonymous (_, `enum _), _ as t ->
			let name = "enum_" ^ unique_key in
			pp_enum ff ~name_mapping ~anonymous_mapping ~current name t
		| `anonymous (_, `struct_type (alignment, items)), _ ->
			let name = "struct_" ^ unique_key in
			let attrs = {Semantics.no_attributes with Semantics.at_aligned = alignment} in
			pp_struct ff ~name_mapping ~anonymous_mapping ~current name items attrs
		| `anonymous (_, `union items), _ ->
			let name = "union_" ^ unique_key in
			pp_union ff ~name_mapping ~anonymous_mapping ~current name items
		| `function_type _, _ ->
			() (* only subprogram or access to subprogram *)
		end
	);;
	
	let pp_typedef
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(item: Semantics.typedef_item)
		: unit =
	(
		begin try
			let alias = find_mapped_type (item :> Semantics.type_item) language_mapping in
			let `named (_, name, _, _), _ = item in
			fprintf ff "@ subtype %s is %s;" name alias
		with Not_found ->
			let `named (ps, name, `typedef t, _), _ = item in
			begin match t with
			| `function_type _, _ ->
				fprintf ff "@ --  subtype %s is ... (function type)" name
			| _ ->
				let name = ada_name_of current ps name `namespace name_mapping in
				fprintf ff "@ subtype %s is " name;
				begin match name with
				| "ptrdiff_t" ->
					pp_print_string ff "Standard.C.ptrdiff_t"
				| "size_t" ->
					pp_print_string ff "Standard.C.size_t"
				| "wchar_t" ->
					pp_print_string ff "Standard.C.wchar_t"
				| _ ->
					pp_type_name ff ~name_mapping ~anonymous_mapping ~current ~where:`subtype t
				end;
				pp_print_char ff ';'
			end
		end
	);;
	
	let pp_named_type
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(opaque_types: Semantics.opaque_types)
		~(current: string)
		(item: Semantics.named_type Semantics.item)
		: unit =
	(
		begin try
			let alias = find_mapped_type (item :> Semantics.type_item) language_mapping in
			let `named (_, name, _, _), _ = item in
			fprintf ff "@ subtype %s is %s;" name alias
		with Not_found ->
			begin match item with
			| `named (ps, name, (`opaque_enum | `opaque_struct | `opaque_union as kind), _), _ as item ->
				let name = ada_name_of current ps name kind name_mapping in
				if Semantics.is_opaque item opaque_types then (
					fprintf ff "@ type %s (<>) is limited private;" name;
				) else (
					fprintf ff "@ type %s;" name
				)
			| `named (ps, name, `enum _, _), _ as t ->
				let name = ada_name_of current ps name `opaque_enum name_mapping in
				pp_enum ff ~name_mapping ~anonymous_mapping ~current name t
			| `named (ps, name, `struct_type (_, items), attrs), _ ->
				let name = ada_name_of current ps name `opaque_struct name_mapping in
				pp_struct ff ~name_mapping ~anonymous_mapping ~current name items attrs
			| `named (ps, name, `union items, _), _ ->
				let name = ada_name_of current ps name `opaque_union name_mapping in
				pp_union ff ~name_mapping ~anonymous_mapping ~current name items
			| `named (_, _, `typedef _, _), _ as t ->
				pp_typedef ff ~language_mapping ~name_mapping ~anonymous_mapping ~current t
			| `named (_, _, `generic_type, _), _ ->
				assert false (* does not come here *)
			end
		end
	);;
	
	let pp_alias
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(name: string)
		(source_item: Semantics.named_item)
		: unit =
	(
		let `named (source_ps, source_name, _, _), _ = source_item in
		let source_name = ada_name_of current source_ps source_name `namespace name_mapping in
		begin match source_item with
		| `named (_, _, `enum_element _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, #Semantics.named_type_var, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `extern _, _), _ as source_item ->
			begin match source_item with
			| `named (_, _, `extern ((`function_type prototype, _), _), _), _ as source_item ->
				pp_print_space ff ();
				pp_open_box ff indent;
				let _, args, _, _ = prototype in
				let local = add_name_mapping_for_arguments args name_mapping in
				pp_prototype ff ~name_mapping:local ~anonymous_mapping
					~current ~name:(Some name) prototype;
				fprintf ff "@ renames %s;" source_name;
				pp_close_box ff ();
				begin try
					let overload = List.assq source_item language_mapping.Semantics.lm_overload in
					List.iter (fun prototype ->
						pp_print_space ff ();
						pp_open_box ff indent;
						let _, args, _, _ = prototype in
						let local = add_name_mapping_for_arguments args name_mapping in
						pp_prototype ff ~name_mapping:local ~anonymous_mapping
							~current ~name:(Some name) prototype;
						fprintf ff "@ renames %s;" source_name;
						pp_close_box ff ()
					) overload
				with Not_found ->
					()
				end
			| `named (_, _, `extern (t, _), _), _ ->
				pp_print_space ff ();
				pp_open_box ff indent;
				fprintf ff "%s : %a@ renames %s;" name
					(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
					source_name;
				pp_close_box ff ()
			end
		| `named (_, _, `variable _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `function_forward (_, (`function_type _, _)), _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `function_definition _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_operator _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_specifiers _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_type_qualifier _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_typedef _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_element_access _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_expression expr, _), _ ->
			if Semantics.is_static_expression expr then (
				begin match expr with
				| `int_literal _, _
				| `float_literal _, _ ->
					fprintf ff "@ %s : constant := %s;" name source_name
				| _, t ->
					fprintf ff "@ %s : constant %a renames \"%s\";" name
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
						source_name
				end
			) else (
				pp_print_space ff ();
				pp_open_box ff indent;
				let prototype = `cdecl, [], `none, snd expr in (* calling-convention be ignored *)
				pp_prototype ff ~name_mapping ~anonymous_mapping
					~current ~name:(Some name) prototype;
				fprintf ff " renames %s;" source_name;
				pp_close_box ff ()
			)
		| `named (_, _, `defined_generic_expression _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_generic_statement _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `defined_any message, _), _ ->
			fprintf ff "@ --  %s renames %s (%s)" name source_name message
		| `named (_, _, `defined_alias _, _), _ ->
			fprintf ff "@ **** %s renames %s / unimplemented. ****\n" name source_name;
			assert false
		| `named (_, _, `generic_value _, _), _ ->
			assert false (* does not come here *)
		end
	);;
	
	let pp_char_literal (ff: formatter) (c: char): unit = (
		begin match c with
		| '\x00' .. '\x1f' ->
			fprintf ff "char'Val (%d)" (int_of_char c)
		| _ ->
			fprintf ff "\'%c\'" c
		end
	);;
	
	let pp_string_literal (ff: formatter) (s: string): unit = (
		let rec loop q i = (
			if i >= String.length s then (
				if q then pp_print_char ff '\"'
			) else (
				begin match s.[i] with
				| '\"' ->
					if not q then pp_print_char ff '\"';
					pp_print_string ff "\"\"";
					loop true (i + 1)
				| '\t' ->
					if q then pp_print_string ff "\" & ";
					fprintf ff "ASCII.HT & ";
					loop false (i + 1)
				| '\n' ->
					if q then pp_print_string ff "\" & ";
					fprintf ff "ASCII.LF &@ ";
					loop false (i + 1)
				| c ->
					if not q then pp_print_char ff '\"';
					pp_print_char ff c;
					loop true (i + 1)
				end
			)
		) in
		pp_open_vbox ff 0;
		if s = "" then (
			fprintf ff "\"\""
		) else (
			loop false 0
		);
		pp_close_box ff ()
	);;
	
	let rec pp_expression
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(current: string)
		~(outside: outside_precedence)
		(expr: Semantics.expression)
		: unit =
	(
		let pp_bit_op ff inside left right t = (
			let is_signed, unsigned_of_signed =
				begin match Semantics.resolve_typedef t with
				| #unsigned_int_prec as p, _ -> false, p
				| #signed_int_prec as p, _ -> true, unsigned_of_signed p
				| _ -> assert false
				end
			in
			if is_signed then (
				pp_type_name ff ~name_mapping ~anonymous_mapping:[]
					~current ~where:`name t;
				pp_print_char ff ' '
			);
			let paren = parenthesis_required ~outside ~inside:(inside :> precedence) in
			if is_signed || paren then pp_print_char ff '(';
			if is_signed then (
				pp_print_string ff (ada_name_of_int_prec unsigned_of_signed);
				pp_print_string ff " (";
				pp_expression ff ~name_mapping ~current ~outside:`lowest left;
				pp_print_char ff ')'
			) else (
				pp_expression ff ~name_mapping ~current ~outside:`logical_and left
			);
			fprintf ff " %s@ " (
				begin match inside with
				| `logical_and -> "and"
				| `logical_or -> "or"
				| `logical_xor -> "xor"
				end);
			if is_signed then (
				pp_print_string ff (ada_name_of_int_prec unsigned_of_signed);
				pp_print_string ff " (";
				pp_expression ff ~name_mapping ~current ~outside:`lowest right;
				pp_print_char ff ')'
			) else (
				pp_expression ff ~name_mapping ~current ~outside:`logical_and right
			);
			if is_signed || paren then pp_print_char ff ')'
		) in
		begin match expr with
		| `int_literal (_, value), _ ->
			pp_print_string ff (Integer.to_based_string ~base:10 value)
		| `float_literal (_, value), _ ->
			let _, e = Real.frexp value in
			let e16 = e / 4 in
			let m = Real.scale value ~base:16 ~exponent:(- e16) in
			fprintf ff "16#%s#e%+d" (Real.to_based_string ~base:16 m) e16
		| `imaginary_literal _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `char_literal value, _ ->
			pp_char_literal ff value
		| `chars_literal value, _ ->
			fprintf ff "%a &@ char'Val (0)" pp_string_literal value
		| `wchar_literal _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `wchars_literal _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `objc_string_literal _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `enumerator (`named (ps, name, _, _), _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_string ff name
		| `ref_object ((`named (ps, name, _, _), _), _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_string ff name
		| `ref_function (`named (ps, name, _, _), _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_string ff name
		| `__FILE__, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `__LINE__, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `__func__, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `statement _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `function_call (func, args), _ ->
			pp_expression ff ~name_mapping ~current ~outside:`primary func;
			if args <> [] then (
				pp_print_string ff " (";
				let (_: int) =
					List.fold_left (fun num arg ->
						if num >= 2 then fprintf ff ",@ " else pp_print_break ff 0 0;
						pp_expression ff ~name_mapping ~current ~outside:`lowest arg;
						num + 1
					) 1 args
				in
				pp_print_string ff ")"
			)
		| `va_arg _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `element_access (expr, (field_name, _, _, _)), _ ->
			let t = snd expr in
			let items =
				begin match t with
				| `anonymous (_, `struct_type (_, items)), _
				| `anonymous (_, `union items), _
				| `named (_, _, `struct_type (_, items), _), _
				| `named (_, _, `union items, _), _ ->
					items
				| _ ->
					assert false (* does not come here *)
				end
			in
			let field_map = name_mapping_for_struct_items items in
			begin match expr with
			| `dereference expr, _ (* omit .all *)
			| expr ->
				fprintf ff "%a.%s"
					(pp_expression ~name_mapping ~current ~outside:`primary) expr
					(StringMap.find field_name field_map)
			end
		| `dereference expr, _ ->
			begin match expr with
			| `add ((_, (`array _, _) as a), index), _
			| `add (index, (_, (`array _, _) as a)), _ ->
				pp_expression ff ~name_mapping ~current ~outside:`primary a;
				pp_print_string ff " (size_t (";
				pp_expression ff ~name_mapping ~current ~outside:`lowest index;
				pp_print_string ff "))"
			| `add _, t | `sub _, t ->
				(* ex: int_ptr (p + 1).all *)
				pp_type_name ff ~name_mapping ~anonymous_mapping:[]
					~current ~where:`name t;
				fprintf ff " %a.all"
					(pp_expression ~name_mapping ~current ~outside:`primary) expr
			| _ ->
				fprintf ff "%a.all"
					(pp_expression ~name_mapping ~current ~outside:`primary) expr
			end
		| `post_increment _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Assign_%s" hash
		| `post_decrement _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `compound _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `increment _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `decrement _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Assign_%s" hash
		| `address _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `neg _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `bit_not _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `not _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `sizeof_formal_type _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `real _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `imag _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `cast _, _ as expr ->
			begin match expr with
			| `cast (`float_literal _, _ as expr), (#real_prec, _ as t2) ->
				fprintf ff "%a'(%a)"
					pp_predefined_type_name t2
					(pp_expression ~name_mapping ~current ~outside:`lowest) expr
			| `cast expr, (`char, _) ->
				fprintf ff "char'Val (%a)"
					(pp_expression ~name_mapping ~current ~outside:`lowest) expr
			| `cast (_, (#int_prec, _) as expr), (`pointer (`void, _), _)
					when Semantics.is_static_expression expr
				->
				begin match Semantics.integer_of_expression expr with
				| Some (_, value) ->
					fprintf ff "void_ptr (System'To_Address (%s))"
						(Integer.to_based_string ~base:10 value)
				| None ->
					assert false (* does not come here *)
				end
			| `cast expr, (`pointer _, _)
			| `cast (_, (`pointer _, _) as expr), _ ->
				fprintf ff "Cast (%a)"
					(pp_expression ~name_mapping ~current ~outside:`lowest) expr
			| _ ->
				fprintf ff "@ **** unimplemented. ****\n";
				assert false
			end
		| `mul (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`term in
			if paren then pp_print_char ff '(';
			pp_expression ff ~name_mapping ~current ~outside:`term left;
			fprintf ff " * ";
			pp_expression ff ~name_mapping ~current ~outside:`term right;
			if paren then pp_print_char ff ')'
		| `div (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`term in
			if paren then pp_print_char ff '(';
			pp_expression ff ~name_mapping ~current ~outside:`term left;
			fprintf ff " / ";
			pp_expression ff ~name_mapping ~current ~outside:`term_right right;
			if paren then pp_print_char ff ')'
		| `rem (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`term in
			if paren then pp_print_char ff '(';
			pp_expression ff ~name_mapping ~current ~outside:`term left;
			fprintf ff " rem ";
			pp_expression ff ~name_mapping ~current ~outside:`term_right right;
			if paren then pp_print_char ff ')'
		| `add (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`simple in
			if paren then pp_print_char ff '(';
			pp_expression ff ~name_mapping ~current ~outside:`simple left;
			fprintf ff " + ";
			pp_expression ff ~name_mapping ~current ~outside:`simple right;
			if paren then pp_print_char ff ')'
		| `sub (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`simple in
			if paren then pp_print_char ff '(';
			pp_expression ff ~name_mapping ~current ~outside:`simple left;
			fprintf ff " - ";
			pp_expression ff ~name_mapping ~current ~outside:`simple_right right;
			if paren then pp_print_char ff ')'
		| `l_shift (left, right), t ->
			pp_type_name ff ~name_mapping ~anonymous_mapping:[]
				~current ~where:`name t;
			pp_print_string ff "'(Shift_Left (";
			pp_expression ff ~name_mapping ~current ~outside:`lowest left;
			pp_print_string ff ", ";
			let static_shift_bits = Semantics.is_static_expression right in
			if not static_shift_bits then pp_print_string ff "Natural (";
			pp_expression ff ~name_mapping ~current ~outside:`lowest right;
			if not static_shift_bits then pp_print_char ff ')';
			pp_print_string ff "))"
		| `r_shift (left, right), t ->
			pp_type_name ff ~name_mapping ~anonymous_mapping:[]
				~current ~where:`name t;
			begin match Semantics.resolve_typedef t with
			| #signed_int_prec, _ ->
				fprintf ff "'(Shift_Right_Arithmetic ("
			| #unsigned_int_prec, _ ->
				fprintf ff "'(Shift_Right ("
			| _ ->
				assert false (* error! *)
			end;
			pp_expression ff ~name_mapping ~current ~outside:`lowest left;
			pp_print_string ff ", ";
			let static_shift_bits = Semantics.is_static_expression right in
			if not static_shift_bits then pp_print_string ff "Natural (";
			pp_expression ff ~name_mapping ~current ~outside:`lowest right;
			if not static_shift_bits then pp_print_char ff ')';
			pp_print_string ff "))"
		| `lt _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `gt (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`relation in
			if paren then pp_print_char ff '(';
			pp_expression ff ~name_mapping ~current ~outside:`relation left;
			fprintf ff " > ";
			pp_expression ff ~name_mapping ~current ~outside:`relation right;
			if paren then pp_print_char ff ')'
		| `le _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `ge (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`relation in
			if paren then pp_print_char ff '(';
			pp_expression ff ~name_mapping ~current ~outside:`relation left;
			fprintf ff " >= ";
			pp_expression ff ~name_mapping ~current ~outside:`relation right;
			if paren then pp_print_char ff ')'
		| `eq _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `ne (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`relation in
			if paren then pp_print_char ff '(';
			pp_expression ff ~name_mapping ~current ~outside:`relation left;
			fprintf ff " /= ";
			pp_expression ff ~name_mapping ~current ~outside:`relation right;
			if paren then pp_print_char ff ')'
		| `bit_and (left, right), t ->
			pp_bit_op ff `logical_and left right t
		| `bit_xor (left, right), t ->
			pp_bit_op ff `logical_xor left right t
		| `bit_or (left, right), t ->
			pp_bit_op ff `logical_or left right t
		| `and_then (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`logical_and_then in
			if paren then fprintf ff "(@[";
			pp_expression ff ~name_mapping ~current ~outside:`logical_and_then left;
			fprintf ff "@ and then ";
			pp_expression ff ~name_mapping ~current ~outside:`logical_and_then right;
			if paren then fprintf ff "@])"
		| `or_else (left, right), _ ->
			let paren = parenthesis_required ~outside ~inside:`logical_or_else in
			if paren then fprintf ff "(@[";
			pp_expression ff ~name_mapping ~current ~outside:`logical_or_else left;
			fprintf ff "@ or else ";
			pp_expression ff ~name_mapping ~current ~outside:`logical_or_else right;
			if paren then fprintf ff "@])"
		| `cond _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Cond_%s" hash
		| `assign _, _ as expr ->
			let hash = hash_name expr in
			fprintf ff "Assign_%s" hash
		| `comma _, _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		end
	) and pp_expression_wanted
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(current: string)
		~(outside: outside_precedence)
		~(wanted_type: Semantics.type_item)
		(expr: Semantics.expression)
		: unit =
	(
		let _, t = expr in
		let resolved_wanted_type = Semantics.resolve_typedef wanted_type in
		if resolved_wanted_type != Semantics.resolve_typedef t then (
			begin match resolved_wanted_type with
			| #int_prec, _ ->
				pp_type_name ff ~name_mapping ~anonymous_mapping:[] ~current ~where:`name wanted_type;
				pp_print_string ff " (";
				pp_expression ff ~name_mapping ~current ~outside:`lowest expr;
				pp_print_string ff ")"
			| _ ->
				fprintf ff "@ **** unimplemented. ****\n";
				assert false
			end
		) else (
			pp_expression ff ~name_mapping ~current ~outside expr
		)
	);;
	
	let rec pp_assignment_expression
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(current: string)
		~(pp_left: formatter -> outside:outside_precedence -> Semantics.expression -> unit)
		(expr: Semantics.assignment_expression)
		: unit =
	(
		let `assign (left, op, right), _ = expr in
		fprintf ff "%a := " (pp_left ~outside:`lowest) left;
		begin match op with
		| `assign ->
			pp_expression_wanted ff ~name_mapping ~current ~outside:`lowest ~wanted_type:(snd left) right;
			pp_print_string ff ";"
		| `mul_assign ->
			fprintf ff "%a * %a;"
				(pp_left ~outside:`term) left
				(pp_expression ~name_mapping ~current ~outside:`term) right
		| `div_assign ->
			fprintf ff "%a / %a;"
				(pp_left ~outside:`term_right) left
				(pp_expression ~name_mapping ~current ~outside:`term_right) right
		| `rem_assign ->
			fprintf ff "%a rem %a;"
				(pp_left ~outside:`term_right) left
				(pp_expression ~name_mapping ~current ~outside:`term_right) right
		| `add_assign ->
			fprintf ff "%a + %a;"
				(pp_left ~outside:`simple) left
				(pp_expression ~name_mapping ~current ~outside:`simple) right
		| `sub_assign ->
			fprintf ff "%a - %a;"
				(pp_left ~outside:`simple) left
				(pp_expression ~name_mapping ~current ~outside:`simple_right) right
		| `l_shift_assign ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `r_shift_assign ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `and_assign ->
			fprintf ff "%a and %a;"
				(pp_left ~outside:`logical_and) left
				(pp_expression ~name_mapping ~current ~outside:`logical_and) right
		| `or_assign ->
			fprintf ff "%a or %a;"
				(pp_left ~outside:`logical_or) left
				(pp_expression ~name_mapping ~current ~outside:`logical_or) right
		| `xor_assign ->
			fprintf ff "%a xor %a;"
				(pp_left ~outside:`logical_xor) left
				(pp_expression ~name_mapping ~current ~outside:`logical_xor) right
		end
	);;
	
	let rec pp_statement
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(current: string)
		(stmt: Semantics.statement)
		: unit =
	(
		begin match stmt with
		| `asm (volatile, template, out_args, in_args, destructive) ->
			pp_print_space ff ();
			pp_open_box ff indent;
			fprintf ff "Asm (%a" pp_string_literal template;
			if out_args <> [] then (
				let handle (n, expr) = (
					pp_type_name ff ~name_mapping ~anonymous_mapping:[]
						~current ~where:`name (snd expr);
					fprintf ff "\'Asm_Output (\"%s\", %a)" n
						(pp_expression ~name_mapping ~current ~outside:`lowest) expr
				) in
				fprintf ff ",@ ";
				pp_open_box ff indent;
				fprintf ff "Outputs =>@ ";
				begin match out_args with
				| x :: [] ->
					handle x
				| _ ->
					fprintf ff "(";
					let (_: int) =
						List.fold_left (fun i x ->
							if i > 0 then fprintf ff "@ ,";
							handle x;
							i + 1
						) 0 in_args
					in
					fprintf ff ")"
				end;
				pp_close_box ff ()
			);
			if in_args <> [] then (
				let handle (n, expr) = (
					pp_type_name ff ~name_mapping ~anonymous_mapping:[]
						~current ~where:`name (snd expr);
					fprintf ff "\'Asm_Input (\"%s\", %a)" n
						(pp_expression ~name_mapping ~current ~outside:`lowest) expr
				) in
				fprintf ff ",@ Inputs => ";
				begin match in_args with
				| x :: [] ->
					handle x
				| _ ->
					fprintf ff "(";
					let (_: int) =
						List.fold_left (fun i x ->
							if i > 0 then fprintf ff "@ ,";
							handle x;
							i + 1
						) 0 in_args
					in
					fprintf ff ")"
				end
			);
			if destructive <> [] then (
				fprintf ff ",@ Clobber =>@ ";
				fprintf ff "@ **** unimplemented. ****\n";
				assert false
			);
			if volatile = `volatile ||
				in_args = [] (* suppressing warning by gnat *)
			then (
				fprintf ff ",@ Volatile => True"
			);
			pp_print_string ff ");";
			pp_close_box ff ()
		| `local _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `compound _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `expression _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `va_start _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `va_end _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `va_copy _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `label _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `if_statement (cond, true_case, false_case) ->
			pp_if ff
				~pp_cond:(fun ff () -> pp_expression ff ~name_mapping ~current ~outside:`lowest cond)
				~pp_true_case:(
					begin fun ff () ->
						if true_case <> [] then (
							List.iter (fun stmt ->
								pp_statement ff ~name_mapping ~current stmt
							) true_case;
						) else (
							fprintf ff "@ null;"
						)
					end)
				~pp_false_case:(if false_case = [] then None else Some (
					begin fun ff () ->
						List.iter (fun stmt ->
							pp_statement ff ~name_mapping ~current stmt
						) false_case
					end))
		| `while_loop _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `do_loop _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `for_loop _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `goto _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `break _ ->
			fprintf ff "@ **** unimplemented. ****\n";
			assert false
		| `return (expr, return_type) ->
			pp_print_space ff ();
			pp_open_box ff indent;
			pp_print_string ff "return";
			begin match expr with
			| Some expr ->
				pp_print_space ff ();
				pp_expression_wanted ff ~name_mapping ~current ~outside:`lowest ~wanted_type:return_type expr
			| None ->
				()
			end;
			pp_print_char ff ';';
			pp_close_box ff ()
		end
	);;
	
	let pp_named
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(opaque_types: Semantics.opaque_types)
		~(current: string)
		(item: Semantics.named_item)
		: unit =
	(
		begin match item with
		| `named (_, _, `enum_element _, _), _ ->
			() (* it shell be declared with enum-type *)
		| `named (_, _, #Semantics.named_type_var, _), _ as item ->
			pp_named_type ff ~language_mapping ~name_mapping ~anonymous_mapping ~opaque_types ~current item
		| `named (ps, name, `extern (t, alias), attrs), _ ->
			let ada_name = ada_name_of current ps name `namespace name_mapping in
			begin match item with
			| `named (_, _, `extern ((`function_type prototype, _), _), _), _ as item ->
				pp_print_space ff ();
				pp_open_box ff indent;
				let _, args, _, _ = prototype in
				let local = add_name_mapping_for_arguments args name_mapping in
				pp_prototype ff ~name_mapping:local ~anonymous_mapping
					~current ~name:(Some ada_name) prototype;
				pp_print_string ff ";";
				pp_close_box ff ();
				begin try
					let overload = List.assq item language_mapping.Semantics.lm_overload in
					List.iter (fun prototype ->
						pp_print_space ff ();
						pp_open_box ff indent;
						let _, args, _, _ = prototype in
						let local = add_name_mapping_for_arguments args name_mapping in
						pp_prototype ff ~name_mapping:local ~anonymous_mapping
							~current ~name:(Some ada_name) prototype;
						pp_print_string ff ";";
						pp_close_box ff ()
					) overload
				with Not_found ->
					()
				end;
				if attrs.Semantics.at_noreturn then pp_pragma_noreturn ff ada_name
			| _ ->
				fprintf ff "@ %s : %a;" ada_name
					(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`extern) t
			end;
			let c_name =
				begin match alias with
				| `alias c_name ->
					if c_name.[0] = '_' then String.sub c_name 1 (String.length c_name - 1) else
					c_name
				| `none ->
					name
				end
			in
			pp_pragma_import ff attrs.Semantics.at_conventions ada_name c_name
		| `named (_, name, `variable _, _), _ ->
			fprintf ff "@ **** %s / unimplemented. ****\n" name;
			assert false
		| `named (ps, name, `function_forward (k, (`function_type prototype, _)), _), _ ->
			begin match k with
			| `builtin ->
				let ada_name = ada_name_of current ps name `namespace name_mapping in
				pp_print_space ff ();
				pp_open_box ff indent;
				let _, args, _, _ = prototype in
				let local = add_name_mapping_for_arguments args name_mapping in
				pp_prototype ff ~name_mapping:local ~anonymous_mapping
					~current ~name:(Some ada_name) prototype;
				pp_print_string ff ";";
				pp_close_box ff ();
				pp_pragma_import ff `intrinsic ada_name name
			| `static ->
				(* function will be translated from `function_definition *)
				fprintf ff "@ --  function %s ..." name
			end
		| `named (ps, name, `function_definition (storage_class, (`function_type prototype, _), _), attrs), _ ->
			let ada_name = ada_name_of current ps name `namespace name_mapping in
			pp_print_space ff ();
			pp_open_box ff indent;
			let _, args, _, _ = prototype in
			let local = add_name_mapping_for_arguments args name_mapping in
			pp_prototype ff ~name_mapping:local ~anonymous_mapping
				~current ~name:(Some ada_name) prototype;
			pp_print_string ff ";";
			pp_close_box ff ();
			begin match storage_class with
			| `extern_inline ->
				(* use extern, ignore body *)
				pp_pragma_import ff attrs.Semantics.at_conventions ada_name name
			| `static ->
				pp_pragma_convention ff attrs.Semantics.at_conventions ada_name;
				begin match attrs.Semantics.at_inline with
				| `inline ->
					fprintf ff "@ pragma Inline (%s);" ada_name
				| `always_inline ->
					fprintf ff "@ pragma Inline_Always (%s);" ada_name
				| `noinline | `none ->
					()
				end
			| `none ->
				fprintf ff "@ **** %s / unimplemented. ****\n" name;
				assert false
			end
		| `named (_, name, `defined_operator _, _), _ ->
			fprintf ff "@ **** %s / unimplemented. ****\n" name;
			assert false
		| `named (ps, name, `defined_specifiers storage_class, attrs), _ ->
			begin match storage_class with
			| `none ->
				if attrs = {Semantics.no_attributes with Semantics.at_conventions = attrs.Semantics.at_conventions} then (
					let ada_name = ada_name_of current ps name `namespace name_mapping in
					let conv = attrs.Semantics.at_conventions in
					pp_pragma_convention_identifier ff ada_name conv
				) else (
					(* available.h (darwin9) has very long identifiers *)
					let name =
						if String.length name <= 60 then name else
						String.sub name 0 57 ^ "..."
					in
					fprintf ff "@ --  %s (attribute)" name
				)
			| `auto ->
				fprintf ff "@ --  %s (alias of static)" name
			| `extern ->
				fprintf ff "@ --  %s (alias of extern)" name
			| `register ->
				fprintf ff "@ --  %s (alias of register)" name
			| `static ->
				fprintf ff "@ --  %s (alias of static)" name
			| `typedef ->
				fprintf ff "@ --  %s (alias of typedef)" name
			end
		| `named (_, name, `defined_type_qualifier q, _), _ ->
			begin match q with
			| `const ->
				fprintf ff "@ --  %s (alias of const)" name
			| `restrict ->
				fprintf ff "@ --  %s (alias of restrict)" name
			| `volatile ->
				fprintf ff "@ --  %s (alias of volatile)" name
			end
		| `named (ps, name, `defined_typedef t, _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			fprintf ff "@ subtype %s is %a;" name
				(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
		| `named (ps, name, `defined_element_access (t, route), _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_space ff ();
			pp_open_box ff indent;
			let prototype = prototype_for_element_access ps t route in
			let _, args, _, _ = prototype in
			let local = add_name_mapping_for_arguments args name_mapping in
			pp_prototype ff ~name_mapping:local ~anonymous_mapping
				~current ~name:(Some name) prototype;
			pp_print_string ff ";";
			pp_close_box ff ();
			fprintf ff "@ pragma Inline (%s);" name
		| `named (ps, name, `defined_expression expr, _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			if Semantics.is_static_expression expr then (
				begin match expr with
				| `ref_function func, _ ->
					pp_alias ff ~language_mapping ~name_mapping ~anonymous_mapping ~current name (func :> Semantics.named_item)
				| `int_literal _, _ | `float_literal _, _ ->
					pp_print_space ff ();
					pp_open_box ff indent;
					fprintf ff "%s : constant :=@ %a;" name
						(pp_expression ~name_mapping ~current ~outside:`lowest) expr;
					pp_close_box ff ()
				| `cast (_, t1 as e1), t2 when List.length (collect_cast [] (item :> Semantics.source_item)) = 1 ->
					(* calling Unchecked_Conversion is not static expression, use trick *)
					pp_print_space ff ();
					pp_open_box ff indent;
					fprintf ff "function %s (Value : %a := %a) return %a@ renames Cast;" name
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t1
						(pp_expression ~name_mapping ~current ~outside:`lowest) e1
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t2;						
					pp_close_box ff ()
				| _, t ->
					pp_print_space ff ();
					pp_open_box ff indent;
					fprintf ff "%s : constant %a :=@ %a;" name
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
						(pp_expression ~name_mapping ~current ~outside:`lowest) expr;
					pp_close_box ff ()
				end
			) else (
				pp_print_space ff ();
				pp_open_box ff indent;
				let prototype = `cdecl, [], `none, snd expr in (* calling-convention be ignored *)
				pp_prototype ff ~name_mapping ~anonymous_mapping
					~current ~name:(Some name) prototype;
				pp_print_string ff ";";
				pp_close_box ff ();
				fprintf ff "@ pragma Inline (%s);" name
			)
		| `named (_, name, `defined_generic_expression _, _), _ ->
			fprintf ff "@ --  %s (function macro)" name
		| `named (_, name, `defined_generic_statement _, _), _ ->
			fprintf ff "@ --  %s (function macro)" name
		| `named (_, name, `defined_any message, _), _ ->
			fprintf ff "@ --  %s (%s)" name message
		| `named (ps, name, `defined_alias source_item, _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_alias ff ~language_mapping ~name_mapping ~anonymous_mapping ~current name source_item
		| `named (_, _, `generic_value _, _), _ ->
			assert false (* does not come here *)
		end
	);;
	
	let pp_named_body
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(anonymous_mapping: (Semantics.anonymous_item * string) list)
		~(current: string)
		(item: Semantics.named_item)
		: unit =
	(
		begin match item with
		| `named (ps, name, `function_definition (storage_class, (`function_type prototype, _), stmts), _), _ ->
			begin match storage_class with
			| `extern_inline ->
				()
			| `static ->
				let ada_name = ada_name_of current ps name `namespace name_mapping in
				let _, args, _, _ = prototype in
				let local = add_name_mapping_for_arguments args name_mapping in
				let modified_arguments =
					List.filter (fun arg ->
						List.exists (Semantics.lvalue_referenced arg) stmts
					) args
				in
				let assignment_expressions = List.fold_left collect_assign_in_stmt [] stmts in
				let conditional_expressions = List.fold_left collect_cond_in_stmt [] stmts in
				let has_local =
					modified_arguments <> [] ||
					assignment_expressions <> [] ||
					conditional_expressions <> []
				in
				pp_print_space ff ();
				if has_local then pp_open_vbox ff indent;
				if has_local then pp_open_box ff 0;
				pp_open_box ff indent;
				pp_prototype ff ~name_mapping:local ~anonymous_mapping
					~current ~name:(Some ada_name) prototype;
				(* start of local declarations *)
				if has_local then pp_close_box ff ();
				fprintf ff "@ is";
				pp_close_box ff ();
				(* modified arguments to local variable *)
				let local =
					List.fold_left (fun local arg ->
						pp_print_space ff ();
						let `named (ps, arg_name, `variable (arg_t, _), _), _ = arg in
						let ada_name = ada_simple_name_of ps arg_name `namespace local in
						let mutable_name = "Mutable_" ^ ada_name in
						fprintf ff "%s : %a := %s;"
							mutable_name
							(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) arg_t
							ada_name;
						let (filename, _, _, _), _ = ps in
						let package_name, map = StringMap.find filename local in
						let map = Naming.add `namespace arg_name mutable_name map in
						StringMap.add filename (package_name, map) local
					) local modified_arguments
				in
				(* specs of inner functions *)
				List.iter (fun (_, t as expr) ->
					pp_print_space ff ();
					let hash = hash_name expr in
					fprintf ff "function Assign_%s return %a;" hash
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t;
				) assignment_expressions;
				List.iter (fun (_, t as expr) ->
					pp_print_space ff ();
					let hash = hash_name expr in
					fprintf ff "function Cond_%s return %a;" hash
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t;
				) conditional_expressions;
				(* bodies of inner functions *)
				List.iter (fun (_, t as expr) ->
					let pp_incdec ff op t = (
						pp_print_space ff ();
						pp_open_box ff indent;
						if Semantics.is_pointer t then (
							fprintf ff "Left := Cast (Cast (Left)@ %c %a'Size / Standard'Storage_Unit);" op
								(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
						) else (
							fprintf ff "Left := Left %c 1;" op
						);
						pp_close_box ff ()
					) in
					pp_print_space ff ();
					let hash = hash_name expr in
					pp_open_vbox ff indent;
					fprintf ff "function Assign_%s return %a is" hash
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t;
					begin match expr with
					| `assign (ebody, _, _), _
					| `increment ebody, _
					| `decrement ebody, _ as expr ->
						fprintf ff "@ Left : %a renames %a;"
							(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
							(pp_expression ~name_mapping:local ~current ~outside:`lowest) ebody;
						pp_close_box ff ();
						pp_print_space ff ();
						pp_open_vbox ff indent;
						fprintf ff "begin";
						begin match expr with
						| `assign _, _ as expr ->
							pp_print_space ff ();
							pp_assignment_expression ff ~name_mapping:local ~current
								~pp_left:(fun ff ~outside _ -> ignore outside; fprintf ff "Left") expr
						| `increment _, t ->
							pp_incdec ff '+' t
						| `decrement _, t ->
							pp_incdec ff '-' t
						end;
						fprintf ff "@ return Left;"
					| `post_increment ebody, _
					| `post_decrement ebody, _ as expr ->
						fprintf ff "@ Left : %a renames %a;"
							(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t
							(pp_expression ~name_mapping:local ~current ~outside:`lowest) ebody;
						fprintf ff "@ Previous : constant %a := Left;"
							(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t;
						pp_close_box ff ();
						pp_print_space ff ();
						pp_open_vbox ff indent;
						fprintf ff "begin";
						begin match expr with
						| `post_increment _, t ->
							pp_incdec ff '+' t
						| `post_decrement _, t ->
							pp_incdec ff '-' t
						end;
						fprintf ff "@ return Previous;"
					end;
					pp_close_box ff ();
					fprintf ff "@ end Assign_%s;" hash
				) assignment_expressions;
				List.iter (fun (`cond (cond, true_case, false_case), t as expr) ->
					pp_print_space ff ();
					let hash = hash_name expr in
					pp_open_vbox ff indent;
					fprintf ff "function Cond_%s return %a is" hash
						(pp_type_name ~name_mapping ~anonymous_mapping ~current ~where:`subtype) t;
					pp_close_box ff ();
					pp_print_space ff ();
					pp_open_vbox ff indent;
					fprintf ff "begin";
					pp_if ff
						~pp_cond:(fun ff () -> pp_expression ff ~name_mapping:local ~current ~outside:`lowest cond)
						~pp_true_case: (
							begin fun ff () ->
								pp_print_space ff ();
								pp_print_string ff "return ";
								pp_expression ff ~name_mapping:local ~current ~outside:`lowest true_case;
								pp_print_char ff ';'
							end)
						~pp_false_case: (Some (
							begin fun ff () ->
								pp_print_space ff ();
								pp_print_string ff "return ";
								pp_expression ff ~name_mapping:local ~current ~outside:`lowest false_case;
								pp_print_char ff ';'
							end));
					pp_close_box ff ();
					fprintf ff "@ end Cond_%s;" hash
				) conditional_expressions;
				(* end of local declarations *)
				if has_local then pp_close_box ff ();
				pp_print_space ff ();
				pp_open_vbox ff indent;
				pp_print_string ff "begin";
				List.iter (fun stmt ->
					pp_statement ff ~name_mapping:local ~current stmt
				) stmts;
				pp_close_box ff ();
				fprintf ff "@ end %s;" ada_name
			| `none ->
				fprintf ff "@ **** %s / unimplemented. ****\n" name;
				assert false
			end
		| `named (ps, name, `defined_element_access (t, route), _), _ ->
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_space ff ();
			pp_open_box ff indent;
			let prototype = prototype_for_element_access ps t route in
			let _, args, _, _ = prototype in
			let local = add_name_mapping_for_arguments args name_mapping in
			pp_prototype ff ~name_mapping:local ~anonymous_mapping
				~current ~name:(Some name) prototype;
			fprintf ff " is";
			pp_close_box ff ();
			pp_print_space ff ();
			pp_open_vbox ff indent;
			fprintf ff "begin@ ";
			pp_open_box ff indent;
			pp_print_string ff "return Object";
			let (_: Semantics.type_item) =
				List.fold_left (fun (t: Semantics.type_item) field ->
					let items =
						begin match t with
						| `anonymous (_, `struct_type (_, items)), _
						| `anonymous (_, `union items), _
						| `named (_, _, `struct_type (_, items), _), _
						| `named (_, _, `union items, _), _ ->
							items
						| _ ->
							assert false (* does not come here *)
						end
					in
					let field_map = name_mapping_for_struct_items items in
					let name, field_t, _, _ = field in
					fprintf ff ".%s" (StringMap.find name field_map);
					field_t
				) (t :> Semantics.type_item) route
			in
			pp_print_string ff ";";
			pp_close_box ff ();
			pp_close_box ff ();
			fprintf ff "@ end %s;" name
		| `named (_, name, `defined_generic_expression _, _), _ ->
			fprintf ff "@ **** %s / unimplemented. ****\n" name;
			assert false
		| `named (_, name, `defined_generic_statement _, _), _ ->
			fprintf ff "@ **** %s / unimplemented. ****\n" name;
			assert false
		| `named (ps, name, `defined_expression expr, _), _ ->
			assert (not (Semantics.is_static_expression expr));
			let name = ada_name_of current ps name `namespace name_mapping in
			pp_print_space ff ();
			pp_open_box ff indent;
			let prototype = `cdecl, [], `none, snd expr in (* calling-convention be ignored *)
			let _, args, _, _ = prototype in
			let local = add_name_mapping_for_arguments args name_mapping in
			pp_prototype ff ~name_mapping:local ~anonymous_mapping
				~current ~name:(Some name) prototype;
			fprintf ff " is";
			pp_close_box ff ();
			pp_print_space ff ();
			pp_open_vbox ff indent;
			fprintf ff "begin@ ";
			pp_open_box ff indent;
			pp_print_string ff "return ";
			pp_expression ff ~name_mapping ~current ~outside:`lowest expr;
			pp_print_string ff ";";
			pp_close_box ff ();
			pp_close_box ff ();
			fprintf ff "@ end %s;" name
		| _ ->
			assert false (* does not come here *)
		end
	);;
	
	let pp_dir_package_spec
		(ff: formatter)
		~(name: string)
		: unit =
	(
		pp_package_spec
			ff
			~with_packages:[]
			~name:("C." ^ name)
			~kind:`preelaborate
			~pp_contents:ignore
			~pp_private:None
	);;
	
	let pp_translated_package_spec
		(ff: formatter)
		~(language_mapping: Semantics.language_mapping)
		~(name_mapping: name_mapping)
		~(predefined_types: (Semantics.predefined_item * int) list * Semantics.typedef_item list)
		~(derived_types: Semantics.derived_item list)
		~(opaque_types: Semantics.opaque_types)
		~(name: string)
		(items: Semantics.source_item list)
		: unit =
	(
		let ptrdiff_t = Semantics.find_ptrdiff_t predefined_types in
		let casts = List.fold_left collect_cast [] items in
		let casts = List.fold_left (collect_pointer_arithmetic ptrdiff_t) casts items in
		let with_packages =
			let items = (items :> Semantics.all_item list) in
			let items =
				if name <> "" then items else (
					let items = List.rev_append (fst (List.split (fst predefined_types)) :> Semantics.all_item list) items in
					let items = List.rev_append (snd predefined_types :> Semantics.all_item list) items in
					items
				)
			in
			referencing_packages ~language_mapping ~name_mapping ~derived_types ~current:name items
		in
		let with_packages =
			if casts <> [] then (
				("Ada.Unchecked_Conversion", `none) :: with_packages
			) else (
				with_packages
			)
		in
		let has_private_part = 
			List.exists (fun item ->
				begin match item with
				| `named (_, _, (`opaque_enum | `opaque_struct | `opaque_union), _), _ as item ->
					Semantics.is_opaque item opaque_types
				| _ ->
					false
				end
			) items
		in
		pp_package_spec
			ff
			~with_packages
			~name:(if name = "" then "C" else "C." ^ name)
			~kind:`preelaborate
			~pp_contents:(fun ff ->
				if name = "" then (
					(* predefined types *)
					List.iter (fun (item, _) ->
						pp_predefined_type ff ~language_mapping item;
					) (fst predefined_types);
					pp_pragma_import ff `intrinsic "Shift_Left" "Shift_Left";
					pp_pragma_import ff `intrinsic "Shift_Right" "Shift_Right";
					pp_pragma_import ff `intrinsic "Shift_Right_Arithmetic" "Shift_Right_Arithmetic";
					(* special-typedefs *)
					List.iter (fun item ->
						begin match item with
						| `named (_, name, `typedef (#predefined_type, _ as t), _), _ ->
							pp_newtype_of_predefind_type ff ~language_mapping name t
						| _ ->
							assert false
						end
					) (snd predefined_types);
					(* derived types of predefined types *)
					List.iter (fun (item, _) ->
						pp_derived_types_for_the_type ff ~language_mapping ~name_mapping ~anonymous_mapping:[] ~casts ~current:name
							(item :> Semantics.type_item) derived_types
					) (fst predefined_types)
				) else (
					(* casts between types belong to another packages *)
					let belongs_to (t: Semantics.type_item) (items: Semantics.source_item list): bool = (
						begin match t with
						| #Semantics.derived_type, _ as t ->
							List.exists (fun item ->
								begin match item with
								| #Semantics.anonymous_type, _
								| `named (_, _, #Semantics.named_type_var, _), _ as item ->
									Semantics.is_derived_type t item
								| _ ->
									false
								end
							) items
						| _ ->
							List.exists (fun item ->
								begin match item with
								| #Semantics.anonymous_type, _
								| `named (_, _, #Semantics.named_type_var, _), _ as item ->
									item == t
								| _ ->
									false
								end
							) items
						end
					) in
					List.iter (fun (x, y as pair) ->
						if not (belongs_to x items) && not (belongs_to y items) then (
							pp_unchecked_conversion ff ~name_mapping ~anonymous_mapping:[] ~current:name pair
						)
					) casts
				);
				(* items *)
				ignore (
					List.fold_left (fun anonymous_mapping item ->
						(* source item *)
						let anonymous_mapping =
							begin match item with
							| #Semantics.anonymous_type, _ as item ->
								let hash = hash_name item in
								let anonymous_mapping = (item, hash) :: anonymous_mapping in
								pp_anonymous_type ff ~name_mapping ~anonymous_mapping ~current:name item;
								anonymous_mapping
							| `named _, _ as item ->
								pp_named ff ~language_mapping ~name_mapping ~anonymous_mapping ~opaque_types ~current:name item;
								anonymous_mapping
							end
						in
						(* derived types *)
						begin match item with
						| #Semantics.anonymous_type, _
						| `named (_, _, #Semantics.named_type_var, _), _ as t ->
							pp_derived_types_for_the_type ff ~language_mapping ~name_mapping ~anonymous_mapping ~casts ~current:name
								(t :> Semantics.type_item) derived_types
						| _ ->
							()
						end;
						anonymous_mapping
					) [] items
				)
			)
			~pp_private:(
				if has_private_part then Some (
					begin fun ff ->
						List.iter (fun item ->
							begin match item with
							| `named (ps, t_name, (`opaque_enum | `opaque_struct | `opaque_union as kind), _), _ as item
								when Semantics.is_opaque item opaque_types
							->
								let t_name = ada_name_of name ps t_name kind name_mapping in
								fprintf ff "@ type %s is null record;" t_name
							| _ ->
								()
							end
						) items
					end
				) else None
			)
	);;
	
	let pp_translated_package_body
		(ff: formatter)
		~(name_mapping: name_mapping)
		~(name: string)
		(items: Semantics.source_item list)
		: unit =
	(
		let has_asm =
			List.exists (fun item ->
				body_required_for_single_item item && has_asm item
			) items
		in
		let with_packages =
			if has_asm then (
				["System.Machine_Code", `use]
			) else (
				[]
			)
		in
		pp_package_body
			ff
			~with_packages
			~name:("C." ^ name)
			~pp_contents:(fun ff ->
				ignore (
					List.fold_left (fun anonymous_mapping item ->
						begin match item with
						| #Semantics.anonymous_type, _ as item ->
							let hash = hash_name item in
							(item, hash) :: anonymous_mapping
						| `named _, _ as item when body_required_for_single_item item ->
							pp_named_body ff ~name_mapping ~anonymous_mapping ~current:name item;
							anonymous_mapping
						| _ ->
							anonymous_mapping
						end
					) [] items
				)
			)
	);;
	
end;;
