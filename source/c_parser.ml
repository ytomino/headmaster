open C_lexical;;
open C_lexical_firstset;;
open C_literals;;
open C_syntax;;
open C_version;;
open Position;;
open! PositionOperators;;

module type ParserType = sig
	module Literals: LiteralsType
	module LexicalElement: LexicalElementType
		with module Literals := Literals
	module Syntax: SyntaxType
		with module Literals := Literals
	module Language: LanguageType
	
	type 'a in_t = (ranged_position, LexicalElement.t, 'a) LazyList.t
	type 'a nil = (ranged_position, 'a) LazyList.nil
	
	type typedef_set = StringSet.t
	
	val parse_expression_or_error: (ranged_position -> string -> unit) ->
		typedef_set -> 'a in_t -> Syntax.expression Syntax.e * 'a in_t
	
	val parse_declaration_specifiers_or_error: ?has_type:bool ->
		(ranged_position -> string -> unit) -> typedef_set -> 'a in_t ->
		Syntax.declaration_specifiers Syntax.e * 'a in_t
	
	val parse_initializer_or_error: (ranged_position -> string -> unit) ->
		typedef_set -> 'a in_t -> Syntax.initializer_t Syntax.e * 'a in_t
	
	val parse_statement_or_error: ?semicolon_need:bool ->
		(ranged_position -> string -> unit) -> typedef_set -> 'a in_t ->
		Syntax.statement Syntax.e * 'a in_t
	
	val parse_translation_unit: (ranged_position -> string -> unit) -> 'a in_t ->
		Syntax.translation_unit * typedef_set * 'a nil
end;;

module Parser
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType
		with module Literals := Literals)
	(Syntax: SyntaxType
		with module Literals := Literals)
	(Language: LanguageType)
	: ParserType
		with module Literals := Literals
		with module LexicalElement := LexicalElement
		with module Syntax := Syntax
		with module Language := Language =
struct
	module FirstSet = FirstSet (Literals);;
	open Literals;;
	open Syntax;;
	
	type 'a in_t = (ranged_position, LexicalElement.t, 'a) LazyList.t;;
	type 'a nil = (ranged_position, 'a) LazyList.nil;;
	
	type typedef_set = StringSet.t
	
	let add_declarators_to_typedef_set (xs: init_declarator_list opt) (set: typedef_set): typedef_set = (
		let add (x: init_declarator) (set: typedef_set): typedef_set = (
			let rec name_of_declarator (dd: direct_declarator): string option = (
				begin match dd with
				| `ident name ->
					Some name
				| `paren (_, _, `some (_, (_, `some (_, dd), _)), _)
				| `array ((_, dd), _, _, _, _)
				| `static_array1 ((_, dd), _, _, _, _, _)
				| `static_array2 ((_, dd), _, _, _, _, _)
				| `dynamic_array ((_, dd), _, _, _, _)
				| `function_type ((_, dd), _, _, _)
				| `old_function_type ((_, dd), _, _, _) ->
					name_of_declarator dd
				| `paren (_, _, `some (_, (_, `error, _)), _)
				| `paren (_, _, `error, _) ->
					None
				end
			) in
			let decl: declarator =
				begin match x with
				| `no_init decl -> decl
				| `with_init ((_, decl), _, _) -> decl
				end
			in
			let name =
				begin match decl with
				| _, `some (_, dd), _ ->
					name_of_declarator dd
				| _, `error, _ ->
					None
				end
			in
			begin match name with
			| Some name ->
				StringSet.add name set
			| None ->
				set
			end
		) in
		let rec loop (xs: init_declarator_list p) (set: typedef_set): typedef_set = (
			begin match snd xs with
			| `nil x ->
				add x set
			| `cons (xr, _, `some (_, x)) ->
				loop xr (add x set)
			| `cons (xr, _, `error) ->
				loop xr set
			end
		) in
		begin match xs with
		| `some xs ->
			loop xs set
		| `none ->
			set
		end
	);;
	
	let rec has_typedef (spec: declaration_specifiers p): bool = (
		begin match snd spec with
		| `storage_class_specifier ((_, `TYPEDEF), _) ->
			true
		| `storage_class_specifier (_, `some rspec)
		| `type_specifier (_, `some rspec)
		| `type_qualifier (_, `some rspec)
		| `function_specifier (_, `some rspec)
		| `attributes (_, `some rspec)
		| `extension (_, `some rspec) ->
			has_typedef rspec
		| `storage_class_specifier (_, `none)
		| `type_specifier (_, `none)
		| `type_qualifier (_, `none)
		| `function_specifier (_, `none)
		| `attributes (_, `none)
		| `extension (_, `none) ->
			false
		end
	);;
	
	let skip_end_of_line (xs: 'a in_t): 'a in_t = (
		begin match xs with
		| lazy (`cons (_, `end_of_line, xs)) ->
			xs
		| _ ->
			xs
		end
	);;
	
	let rec skip_until_end_of_line (xs: 'a in_t): 'a in_t = (
		begin match xs with
		| lazy (`nil _) ->
			xs
		| lazy (`cons (_, `end_of_line, xs)) ->
			xs
		| lazy (`cons (_, _, xs)) ->
			skip_until_end_of_line xs
		end
	);;
	
	let rec skip_until_semicolon (xs: 'a in_t): [`semicolon] e * 'a in_t = (
		begin match xs with
		| lazy (`nil _) ->
			`error, xs
		| lazy (`cons (sc_p, (`semicolon as sc_e), xs)) ->
			`some (sc_p, sc_e), xs
		| lazy (`cons (_, _, xs)) ->
			skip_until_semicolon xs
		end
	);;
	
	let parse_identifier_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: identifier e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (id_p, (#FirstSet.identifier as id_e), xs)) ->
			`some (id_p, id_e), xs
		| _ ->
			error (LazyList.hd_a xs) "identifier was expected.";
			`error, xs
		end
	);;
	
	let parse_keyword_or_error (type v)
		(vs: (string * v) list)
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: v e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (id_p, (`ident s), xs)) ->
			begin match Listtbl.assocs String.equal s vs with
			| (_, value) :: _ ->
				`some (id_p, value), xs
			| [] ->
				error (LazyList.hd_a xs) ("identifier \"" ^ s ^ "\" is not expected.");
				`error, xs
			end
		| _ ->
			error (LazyList.hd_a xs) "identifier was expected.";
			`error, xs
		end
	);;
	
	let parse_l_paren_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`l_paren] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
			`some (lp_p, lp_e), xs
		| _ ->
			error (LazyList.hd_a xs) "\"(\" was expected.";
			`error, xs
		end
	);;
	
	let parse_r_paren_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`r_paren] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (rp_p, (`r_paren as rp_e), xs)) ->
			`some (rp_p, rp_e), xs
		| _ ->
			let rec skip_until_r_paren (xs: 'a in_t): [`r_paren] e * 'a in_t = (
				begin match xs with
				| lazy (`cons (rp_p, (`r_paren as rp_e), xs)) ->
					error (LazyList.hd_a xs) "the extra token(s) were skipped before \")\".";
					`some (rp_p, rp_e), xs
				| lazy (`cons (_, `semicolon, _)) | lazy (`nil _) ->
					error (LazyList.hd_a xs) "\")\" was expected.";
					`error, xs
				| lazy (`cons (_, _, xs)) ->
					skip_until_r_paren xs
				end
			) in
			skip_until_r_paren xs (* error recovery *)
		end
	);;
	
	let parse_r_bracket_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`r_bracket] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (rb_p, (`r_bracket as rb_e), xs)) ->
			`some (rb_p, rb_e), xs
		| _ ->
			error (LazyList.hd_a xs) "\"]\" was expected.";
			`error, xs
		end
	);;
	
	let parse_l_curly_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`l_curly] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (lc_p, (`l_curly as lc_e), xs)) ->
			`some (lc_p, lc_e), xs
		| _ ->
			error (LazyList.hd_a xs) "\"{\" was expected.";
			`error, xs
		end
	);;
	
	let parse_r_curly_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`r_curly] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (rc_p, (`r_curly as rc_e), xs)) ->
			`some (rc_p, rc_e), xs
		| _ ->
			error (LazyList.hd_a xs) "\"}\" was expected.";
			`error, xs
		end
	);;
	
	let parse_comma_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`comma] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (c_p, (`comma as c_e), xs)) ->
			`some (c_p, c_e), xs
		| _ ->
			error (LazyList.hd_a xs) "\",\" was expected.";
			`error, xs
		end
	);;
	
	let parse_colon_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`colon] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (c_p, (`colon as c_e), xs)) ->
			`some (c_p, c_e), xs
		| _ ->
			error (LazyList.hd_a xs) "\":\" was expected.";
			`error, xs
		end
	);;
	
	let parse_semicolon_or_error
		?(semicolon_need = true)
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`semicolon] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (sc_p, (`semicolon as sc_e), xs)) ->
			`some (sc_p, sc_e), xs
		| _ ->
			if semicolon_need then (
				error (LazyList.hd_a xs) "\";\" was expected."
			);
			`error, xs
		end
	);;
	
	let parse_while_or_error
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: [`WHILE] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (w_p, (`WHILE as w_e), xs)) ->
			`some (w_p, w_e), xs
		| _ ->
			error (LazyList.hd_a xs) "\"while\" was expected.";
			`error, xs
		end
	);;
	
	let rec parse_chars_literal
		(_: ranged_position -> string -> unit)
		(_: typedef_set)
		(xs: [`cons of ranged_position * [`chars_literal of string] * 'a in_t] lazy_t)
		: [`chars_literal of string] p * 'a in_t =
	(
		let rec loop (result: [`chars_literal of string] p) xs = (
			begin match xs with
			| lazy (`cons (cs_p, (`chars_literal str2 as cs_e), xs)) ->
				let _, `chars_literal str1 = result in
				let ps, () = result & (cs_p, cs_e) in
				loop (ps, `chars_literal (str1 ^ str2)) xs
			| _ ->
				result, xs
			end
		) in
		let xs = (xs :> 'a in_t) in
		let next_p = LazyList.hd_a xs in
		loop (next_p, `chars_literal "") xs
	) and parse_chars_literal_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: [`chars_literal of string] e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (`chars_literal _ as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_chars_literal error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "char[] literal was expected.";
			`error, xs
		end
	);;
	
	let parse_wchars_literal
		(_: ranged_position -> string -> unit)
		(_: typedef_set)
		(xs: [`cons of ranged_position * [`wchars_literal of WideString.t] * 'a in_t] lazy_t)
		: [`wchars_literal of WideString.t] p * 'a in_t =
	(
		let rec loop (result: [`wchars_literal of WideString.t] p) xs = (
			begin match xs with
			| lazy (`cons (cs_p, (`wchars_literal str2 as cs_e), xs)) ->
				let _, `wchars_literal str1 = result in
				let length1 = WideString.length str1 in
				let length2 = WideString.length str2 in
				let m = Array.make (length1 + length2) (WideChar.of_int 0) in
				for i = 0 to length1 - 1 do
					m.(i) <- WideString.get str1 i
				done;
				for i = 0 to length2 - 1 do
					m.(length1 + i) <- WideString.get str2 i
				done;
				let ps, () = result & (cs_p, cs_e) in
				loop (ps, `wchars_literal (WideString.of_array m)) xs
			| lazy (`cons (cs_p, (`chars_literal str2 as cs_e), xs)) ->
				let _, `wchars_literal str1 = result in
				let length1 = WideString.length str1 in
				let length2 = String.length str2 in
				let m = Array.make (length1 + length2) (WideChar.of_int 0) in
				for i = 0 to length1 - 1 do
					m.(i) <- WideString.get str1 i
				done;
				for i = 0 to length2 - 1 do
					m.(length1 + i) <- WideChar.of_int (int_of_char str2.[i])
				done;
				let ps, () = result & (cs_p, cs_e) in
				loop (ps, `wchars_literal (WideString.of_array m)) xs
			| _ ->
				result, xs
			end
		) in
		let xs = (xs :> 'a in_t) in
		let next_p = LazyList.hd_a xs in
		loop (next_p, `wchars_literal (WideString.of_array [| |])) xs
	);;
	
	let rec parse_pragma
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [`sharp_PRAGMA] * 'a in_t] lazy_t)
		: pragma p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (prag_p, (`sharp_PRAGMA as prag_e), xs)) ->
			let pragma = prag_p, prag_e in
			let directive, xs =
				begin match xs with
				| lazy (`cons (gcc_p, `ident "GCC", xs)) ->
					let gcc = gcc_p, `GCC in
					let gcc_directive, xs =
						begin match xs with
						| lazy (`cons (fenv_p, `ident "fenv", xs)) ->
							`some (fenv_p, `fenv), skip_end_of_line xs
						| lazy (`cons (poison_p, `ident "poison", xs)) ->
							let poison = poison_p, `POISON in
							begin match xs with
							| lazy (`cons (id_p, (#FirstSet.identifier as id_e), xs)) ->
								let rec loop (rs: poison_identifier_list p) (xs: 'a in_t) = (
									begin match xs with
									| lazy (`cons (id_p, (#FirstSet.identifier as id_e), xs)) ->
										let id = id_p, id_e in
										let ps, () = rs & id in
										loop (ps, `cons (rs, id)) xs
									| _ ->
										rs, xs
									end
								) in
								let ids, xs = loop (id_p, `nil id_e) xs in
								let ps, () = poison & ids in
								`some (ps, `poison (poison, `some ids)), skip_end_of_line xs
							| _ ->
								error (LazyList.hd_a xs) ("poison-identifier-list was expected.");
								`some (poison_p, `poison (poison, `error)), skip_end_of_line xs
							end
						| lazy (`cons (v_p, `ident "visibility", xs)) ->
							let v = v_p, `VISIBILITY in
							let (visibility_argument: visibility_argument e), xs =
								begin match xs with
								| lazy (`cons (push_p, `ident "push", xs)) ->
									let push = push_p, `PUSH in
									let l_paren, xs = parse_l_paren_or_error error xs in
									let default, xs =
										begin match xs with
										| lazy (`cons (d_p, `DEFAULT, xs)) ->
											`some (d_p, `DEFAULT), xs
										| _ ->
											error (LazyList.hd_a xs) ("default was expected.");
											`error, xs
										end
									in
									let r_paren, xs = parse_r_paren_or_error error xs in
									let ps, () = push &^ l_paren &^ default &^ r_paren in
									`some (ps, `push (push, l_paren, default, r_paren)), xs
								| lazy (`cons (pop_p, `ident "pop", xs)) ->
									`some (pop_p, `pop), xs
								| _ ->
									error (LazyList.hd_a xs) ("push or pop was expected.");
									`error, xs
								end
							in
							let ps, () = v &^ visibility_argument in
							`some (ps, `visibility (v, visibility_argument)), skip_end_of_line xs
						| lazy (`cons (sh_p, `ident "system_header", xs)) ->
							`some (sh_p, `system_header), skip_end_of_line xs
						| lazy (`cons (_, `ident keyword, xs)) ->
							error (LazyList.hd_a xs) ("unknown pragma GCC \"" ^ keyword ^ "\".");
							`error, skip_until_end_of_line xs
						| _ ->
							error (LazyList.hd_a xs) ("pragma GCC sub-keyword was expected.");
							`error, skip_until_end_of_line xs
						end
					in
					let ps, () = gcc &^ gcc_directive in
					`some (ps, `gcc (gcc, gcc_directive)), xs
				| lazy (`cons (pack_p, `ident "pack", xs)) ->
					let pack = pack_p, `PACK in
					let l_paren, xs = parse_l_paren_or_error error xs in
					let (pack_argument: pack_argument opt), xs =
						begin match xs with
						| lazy (`cons (push_p, `ident "push", xs)) ->
							let push = push_p, `PUSH in
							begin match xs with
							| lazy (`cons (comma_p, (`comma as comma_e), xs)) ->
								let comma = comma_p, comma_e in
								let n, xs =
									begin match xs with
									| lazy (`cons (a_p, `numeric_literal (_, `int_literal (_, a_e)), xs)) ->
										let n = a_p, a_e in
										`some n, xs
									| _ ->
										error (LazyList.hd_a xs) ("alignment-width was expected.");
										`error, xs
									end
								in
								let ps, () = push & comma &^ n in
								`some (ps, `push (push, `some comma, n)), xs
							| _ ->
								error (LazyList.hd_a xs) ("\",\" was expected.");
								`some (push_p, `push (push, `error, `error)), xs
							end
						| lazy (`cons (pop_p, `ident "pop", xs)) ->
							`some (pop_p, `pop), xs
						| lazy (`cons (a_p, `numeric_literal (_, `int_literal (_, a_e)), xs)) ->
							`some (a_p, `set a_e), xs
						| _ ->
							`none, xs
						end
					in
					let r_paren, xs = parse_r_paren_or_error error xs in
					let ps, () = pack &^ l_paren &^ pack_argument &^ r_paren in
					`some (ps, `pack (pack, l_paren, pack_argument, r_paren)), skip_end_of_line xs
				| lazy (`cons (instance_p, (`ident "instance"), xs)) ->
					let instance = instance_p, `INSTANCE in
					let sql, xs = parse_specifier_qualifier_list_or_error error typedefs xs in
					let decl, xs = parse_declarator_or_error ~use_string:true error typedefs xs in
					let ps, () = instance &^ sql &^ decl in
					`some (ps, `instance (instance, sql, decl)), skip_end_of_line xs
				| lazy (`cons (_, `ident keyword, xs)) ->
					error (LazyList.hd_a xs) ("unknown pragma \"" ^ keyword ^ "\".");
					`error, (skip_until_end_of_line xs)
				| lazy (`cons (for_p, (`FOR as for_e), xs)) ->
					let for_token = for_p, for_e in
					let target_language, xs =
						begin match xs with
						| lazy (`cons (tl_p, `ident tl_e, xs)) ->
							let target_language = tl_p, tl_e in
							`some target_language, xs
						| _ ->
							error (LazyList.hd_a xs) "target language was expected.";
							`error, xs
						end
					in
					let (mapping: language_mapping e), xs =
						begin match xs with
						| lazy (`cons (type_p, `ident "type", xs)) ->
							let type_token = type_p, `TYPE in
							let typename, xs = parse_type_name_or_error error typedefs xs in
							begin match xs with
							| lazy (`cons (ass_p, (`assign as ass_e), xs)) ->
								let assign = ass_p, ass_e in
								let repr, xs = parse_chars_literal_or_error error typedefs xs in
								let ps, () = type_token & assign &^ repr in
								`some (ps, `type_mapping (type_token, typename, `some assign, repr)), xs
							| _ ->
								error (LazyList.hd_a xs) "\"=\" was expected.";
								let ps, () = type_token &^ typename in
								`some (ps, `type_mapping (type_token, typename, `error, `error)), xs
							end
						| lazy (`cons (type_p, `ident "overload", xs)) ->
							let overload_token = type_p, `OVERLOAD in
							let sql, xs = parse_specifier_qualifier_list_or_error error typedefs xs in
							let decl, xs = parse_declarator_or_error error typedefs xs in
							let ps, () = overload_token &^ sql &^ decl in
							`some (ps, `overload (overload_token, sql, decl)), xs
						| lazy (`cons (a, (`chars_literal _ as it), xr)) ->
							let xs = lazy (`cons (a, it, xr)) in
							let file1, xs = parse_chars_literal error typedefs xs in
							begin match xs with
							| lazy (`cons (inc_p, `ident "include", xs)) ->
								let inc = inc_p, `INCLUDE in
								let file2, xs = parse_chars_literal_or_error error typedefs xs in
								let ps, () = file1 & inc &^ file2 in
								`some (ps, `includes (file1, `some inc, file2)), xs
							| lazy (`cons (inc_p, `ident "monolithic_include", xs)) ->
								let inc = inc_p, `MONOLITHIC_INCLUDE in
								let file2, xs = parse_chars_literal_or_error error typedefs xs in
								let ps, () = file1 & inc &^ file2 in
								`some (ps, `monolithic_include (file1, `some inc, file2)), xs
							| _ ->
								error (LazyList.hd_a xs) "\"include\" was expected.";
								let ps = fst file1 in
								`some (ps, `includes (file1, `error, `error)), xs
							end
						| _ ->
							error (LazyList.hd_a xs) "pragma \"for\" syntax error.";
							`error, xs
						end
					in
					let ps, () = for_token &^ target_language &^ mapping in
					`some (ps, `language_mapping (for_token, target_language, mapping)), skip_end_of_line xs
				| _ ->
					error (LazyList.hd_a xs) "pragma sub-keyword was expected.";
					`error, (skip_until_end_of_line xs)
				end
			in
			let ps, () = pragma &^ directive in
			(ps, (pragma, directive)), xs
		end
	) and parse_attribute_list
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [`__attribute__] * 'a in_t] lazy_t)
		: attribute_list p * 'a in_t =
	(
		let rec loop (rs: attribute_list p) (xs: 'a in_t) = (
			begin match xs with
			| lazy (`cons (a, (`__attribute__ as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				let attr, xs = parse_attribute error typedefs xs in
				let ps, () = rs & attr in
				loop (ps, `cons (rs, attr)) xs
			| _ ->
				rs, xs
			end
		) in
		let (ps, attr), xs = parse_attribute error typedefs xs in
		loop (ps, `nil attr) xs
	) and parse_attribute_list_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: attribute_list opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (`__attribute__ as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let list, xs = parse_attribute_list error typedefs xs in
			`some list, xs
		| _ ->
			`none, xs
		end
	) and parse_attribute
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [`__attribute__] * 'a in_t] lazy_t)
		: attribute p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (attr_p, (`__attribute__ as attr_e), xs)) ->
			let attr = attr_p, attr_e in
			let l_paren1, xs = parse_l_paren_or_error error xs in
			let l_paren2, xs = parse_l_paren_or_error error xs in
			let items, xs = parse_attribute_item_list_or_error error typedefs xs in
			let r_paren2, xs = parse_r_paren_or_error error xs in
			let r_paren1, xs = parse_r_paren_or_error error xs in
			let ps, () = attr &^ l_paren1 &^ l_paren2 &^ items &^ r_paren2 &^ r_paren1 in
			(ps, (attr, l_paren1, l_paren2, items, r_paren2, r_paren1)), xs
		end
	) and parse_attribute_item_list_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: attribute_item_list e * 'a in_t =
	(
		let rec loop rs xs = (
			begin match xs with
			| lazy (`cons (comma_p, (`comma as comma_e), xs)) ->
				let comma = comma_p, comma_e in
				let second, xs = parse_attribute_item_or_error error typedefs xs in
				let ps, () = rs & comma &^ second in
				loop (ps, `cons (rs, comma, second)) xs
			| _ ->
				`some rs, xs
			end
		) in
		begin match parse_attribute_item_or_error error typedefs xs with
		| `some (first_p, first_e), xs ->
			loop (first_p, `nil first_e) xs
		| `error, _ as result ->
			(* an error has been already reported by parse_initializer_or_error *)
			result
		end
	) and parse_attribute_item_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: attribute_item e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (p4, `ident attr_keyword, xs)) ->
			begin match attr_keyword with
			| "aligned" | "__aligned__" ->
				let n = p4, attr_keyword in
				let args, xs =
					begin match xs with
					| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
						let l_paren = lp_p, lp_e in
						let arg, xs = parse_assignment_expression_or_error error typedefs xs in
						let r_paren, xs = parse_r_paren_or_error error xs in
						let param_p, () = l_paren &^ arg &^ r_paren in
						let param = param_p, (l_paren, arg, r_paren) in
						`some param, xs
					| _ ->
						`none, xs
					end
				in
				let ps, () = n &^ args in
				`some (ps, `aligned (n, args)), xs
			| "alloc_size" | "__alloc_size__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let args, xs = parse_argument_expression_list_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ args &^ r_paren in
				`some (ps, `alloc_size (n, l_paren, args, r_paren)), xs
			| "always_inline" | "__always_inline__" ->
				`some (p4, `always_inline attr_keyword), xs
			| "__artificial__" ->
				`some (p4, `artificial), xs
			| "blocks" | "__blocks__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_keyword_or_error ["byref", `BYREF] error xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `blocks (n, l_paren, arg, r_paren)), xs
			| "cdecl" | "__cdecl__" ->
				`some (p4, `cdecl attr_keyword), xs
			| "__const__" ->
				`some (p4, `const attr_keyword), xs
			| "deprecated" | "__deprecated__" ->
				let n = p4, attr_keyword in
				let args, xs =
					begin match xs with
					| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
						let l_paren = lp_p, lp_e in
						let arg, xs = parse_assignment_expression_or_error error typedefs xs in
						let r_paren, xs = parse_r_paren_or_error error xs in
						let param_p, () = l_paren &^ arg &^ r_paren in
						let param = param_p, (l_paren, arg, r_paren) in
						`some param, xs
					| _ ->
						`none, xs
					end
				in
				let ps, () = n &^ args in
				`some (ps, `deprecated (n, args)), xs
			| "dllimport" | "__dllimport__" ->
				`some (p4, `dllimport attr_keyword), xs
			| "dllexport" | "__dllexport__" ->
				`some (p4, `dllexport attr_keyword), xs
			| "__fastcall__" ->
				`some (p4, `fastcall), xs
			| "format" | "__format__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg1, xs = parse_identifier_or_error error xs in
				let comma1, xs = parse_comma_or_error error xs in
				let arg2, xs = parse_assignment_expression_or_error error typedefs xs in
				let comma2, xs = parse_comma_or_error error xs in
				let arg3, xs = parse_assignment_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg1 &^ comma1 &^ arg2 &^ comma2 &^ arg3 &^ r_paren in
				`some (ps, `format (n, l_paren, arg1, comma1, arg2, comma2, arg3, r_paren)), xs
			| "format_arg" | "__format_arg__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_assignment_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `format_arg (n, l_paren, arg, r_paren)), xs
			| "__gnu_inline__" ->
				`some (p4, `gnu_inline), xs
			| "__leaf__" ->
				`some (p4, `leaf), xs
			| "__malloc__" ->
				`some (p4, `malloc), xs
			| "__may_alias__" ->
				`some (p4, `may_alias), xs
			| "mode" | "__mode__" ->
				let modes: (string * bit_width_mode) list = [
					"__QI__", `__QI__;
					"__HI__", `__HI__;
					"__SI__", `__SI__;
					"__DI__", `__DI__;
					"__pointer__", `__pointer__;
					"__unwind_word__", `__unwind_word__;
					"__word__", `__word__]
				in
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_keyword_or_error modes error xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `mode (n, l_paren, arg, r_paren)), xs
			| "noinline" | "__noinline__" ->
				`some (p4, `noinline attr_keyword), xs
			| "nonnull" | "__nonnull__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let args, xs = parse_argument_expression_list_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ args &^ r_paren in
				`some (ps, `nonnull (n, l_paren, args, r_paren)), xs
			| "nonstring" | "__nonstring__" ->
				`some (p4, `noreturn attr_keyword), xs
			| "noreturn" | "__noreturn__" ->
				`some (p4, `noreturn attr_keyword), xs
			| "nothrow" | "__nothrow__" ->
				`some (p4, `nothrow attr_keyword), xs
			| "objc_gc" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_keyword_or_error ["weak", `WEAK] error xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `objc_gc (n, l_paren, arg, r_paren)), xs
			| "optimize" | "__optimize__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_assignment_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `optimize (n, l_paren, arg, r_paren)), xs
			| "packed" | "__packed__" ->
				`some (p4, `packed attr_keyword), xs
			| "__pure__" ->
				`some (p4, `pure), xs
			| "regparm" | "__regparm__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_assignment_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `regparm (n, l_paren, arg, r_paren)), xs
			| "__returns_nonnull__" ->
				`some (p4, `returns_nonnull), xs
			| "__returns_twice__" ->
				`some (p4, `returns_twice), xs
			| "section" | "__section__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_assignment_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `section (n, l_paren, arg, r_paren)), xs
			| "selectany" ->
				`some (p4, `selectany), xs
			| "sentinel" | "__sentinel__" ->
				`some (p4, `sentinel), xs
			| "__stdcall__" ->
				`some (p4, `stdcall), xs
			| "__thiscall__" ->
				`some (p4, `thiscall), xs
			| "__transparent_union__" ->
				`some (p4, `transparent_union), xs
			| "unavailable" ->
				`some (p4, `unavailable), xs
			| "unused" | "__unused__" ->
				`some (p4, `unused attr_keyword), xs
			| "used" | "__used__" ->
				`some (p4, `used attr_keyword), xs
			| "vector_size" | "__vector_size__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_assignment_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `vector_size (n, l_paren, arg, r_paren)), xs
			| "visibility" | "__visibility__" ->
				let n = p4, attr_keyword in
				let l_paren, xs = parse_l_paren_or_error error xs in
				let arg, xs = parse_assignment_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = n &^ l_paren &^ arg &^ r_paren in
				`some (ps, `visibility (n, l_paren, arg, r_paren)), xs
			| "warn_unused_result" | "__warn_unused_result__" ->
				`some (p4, `warn_unused_result attr_keyword), xs
			| "__weak__" ->
				`some (p4, `weak), xs
			| "weak_import" ->
				`some (p4, `weak_import), xs
			| _ ->
				error p4 ("unknown attribute \"" ^ attr_keyword ^ "\".");
				`error, xs
			end
		| lazy (`cons (p4, `CONST, xs)) ->
			let attr_keyword = string_of_rw `CONST in
			`some (p4, `const attr_keyword), xs
		| _ ->
			error (LazyList.hd_a xs) "identifier was expected.";
			`error, xs
		end
	) and parse_inline_assembler
		?(semicolon_need: bool = true)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.asm * 'a in_t] lazy_t)
		: inline_assembler p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (asm_p, (#FirstSet.asm as asm_e), xs)) ->
			let asm = asm_p, asm_e in
			let volatile, xs =
				begin match xs with
				| lazy (`cons (v_p, (`VOLATILE | `__volatile__ as v_e), xs)) ->
					`some (v_p, v_e), xs
				| _ ->
					`none, xs
				end
			in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let template, xs = parse_chars_literal_or_error error typedefs xs in
			let in_args, xs = parse_ia_out_option error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
			let ps, () = asm &^ volatile &^ l_paren &^ template &^ in_args &^ r_paren &^ semicolon in
			(ps, (asm, volatile, l_paren, template, in_args, r_paren, semicolon)), xs
		end
	) and parse_ia_out_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: ia_out opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (colon_p, (`colon as colon_e), xs)) ->
			let colon = colon_p, colon_e in
			let args, xs = parse_ia_argument_list_option error typedefs xs in
			let out, xs = parse_ia_in_option error typedefs xs in
			let ps, () = colon &^ args &^ out in
			`some (ps, (colon, args, out)), xs
		| _ ->
			`none, xs
		end
	) and parse_ia_in_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: ia_in opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (colon_p, (`colon as colon_e), xs)) ->
			let colon = colon_p, colon_e in
			let args, xs = parse_ia_argument_list_option error typedefs xs in
			let dest, xs = parse_ia_destructive_option error typedefs xs in
			let ps, () = colon &^ args &^ dest in
			`some (ps, (colon, args, dest)), xs
		| _ ->
			`none, xs
		end
	) and parse_ia_argument_list_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: ia_argument_list opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (`chars_literal _ | `l_bracket as it), xr)) ->
			let rec loop (rs: ia_argument_list p) xs = (
				begin match xs with
				| lazy (`cons (comma_p, (`comma as comma_e), xs)) ->
					let comma = comma_p, comma_e in
					let second, xs = parse_ia_argument_or_error error typedefs xs in
					let ps, () = rs & comma &^ second in
					loop (ps, `cons (rs, comma, second)) xs
				| _ ->
					`some rs, xs
				end
			) in
			let xs = lazy (`cons (a, it, xr)) in
			let (first_p, first_e), xs = parse_ia_argument error typedefs xs in
			loop (first_p, `nil first_e) xs
		| _ ->
			`none, xs
		end
	) and parse_ia_argument
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [`chars_literal of string | `l_bracket] * 'a in_t] lazy_t)
		: ia_argument p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a_p, (`chars_literal _ as a_e), xs)) ->
			let a = a_p, a_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let arg, xs = parse_assignment_expression_or_error error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let ps, () = a &^ l_paren &^ arg &^ r_paren in
			(ps, (`none, `some a, l_paren, arg, r_paren)), xs
		| lazy (`cons (l_bracket_p, (`l_bracket as l_bracket_e), xs)) ->
			let l_bracket = l_bracket_p, l_bracket_e in
			let sym, xs = parse_identifier_or_error error xs in
			let r_bracket, xs = parse_r_bracket_or_error error xs in
			let symbol_p, () = l_bracket &^ sym &^ r_bracket in
			let symbol = symbol_p, (l_bracket, sym, r_bracket) in
			let a, xs = parse_chars_literal_or_error error typedefs xs in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let arg, xs = parse_assignment_expression_or_error error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let ps, () = symbol &^ a &^ l_paren &^ arg &^ r_paren in
			(ps, (`some symbol, a, l_paren, arg, r_paren)), xs
		end
	) and parse_ia_argument_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: ia_argument e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (`chars_literal _ | `l_bracket as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_ia_argument error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "inline-assembler argument syntax error.";
			`error, xs
		end
	) and parse_ia_destructive_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: ia_destructive opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (colon_p, (`colon as colon_e), xs)) ->
			let colon = colon_p, colon_e in
			let regs, xs = parse_ia_register_list_option error typedefs xs in
			let ps, () = colon &^ regs in
			`some (ps, (colon, regs)), xs
		| _ ->
			`none, xs
		end
	) and parse_ia_register_list_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: ia_register_list opt * 'a in_t =
	(
		let rec loop rs xs = (
			begin match xs with
			| lazy (`cons (comma_p, (`comma as comma_e), xs)) ->
				let comma = comma_p, comma_e in
				let second, xs = parse_chars_literal_or_error error typedefs xs in
				let ps, () = rs & comma &^ second in
				loop (ps, `cons (rs, comma, second)) xs
			| _ ->
				`some rs, xs
			end
		) in
		begin match xs with
		| lazy (`cons (a, (`chars_literal _ as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let (first_p, first_e), xs = parse_chars_literal error typedefs xs in
			loop (first_p, `nil first_e) xs
		| _ ->
			`none, xs
		end
	)
	(* A.2.1 Expressions *)
	and parse_primary_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_primary_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (id_p, (#FirstSet.identifier as id_e), xs)) ->
			(id_p, id_e), xs
		| lazy (`cons (nl_p, `numeric_literal (_, nl), xs)) ->
			(nl_p, (nl :> expression)), xs
		| lazy (`cons (cl_p, (`char_literal _ as cl_e), xs)) ->
			(cl_p, cl_e), xs
		| lazy (`cons (a, (`chars_literal _ as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			(parse_chars_literal error typedefs xs :> expression p * 'a in_t)
		| lazy (`cons (wcl_p, (`wchar_literal _ as wcl_e), xs)) ->
			(wcl_p, wcl_e), xs
		| lazy (`cons (a, (`wchars_literal _ as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			(parse_wchars_literal error typedefs xs :> expression p * 'a in_t)
		| lazy (`cons (osl_p, (`objc_string_literal _ as osl_e), xs)) ->
			(osl_p, osl_e), xs
		| lazy (`cons (file_p, `__FILE__, xs)) ->
			let filename, _, _, _ = fst file_p in
			(file_p, `__FILE__ filename), xs
		| lazy (`cons (line_p, `__LINE__, xs)) ->
			let _, _, line, _ = fst line_p in
			(line_p, `__LINE__ (Integer.of_int line)), xs
		| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
			let l_paren = lp_p, lp_e in
			begin match xs with
			| lazy (`cons (a, (`l_curly as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				let block, xs = parse_compound_statement error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = l_paren & block &^ r_paren in
				(ps, `statement (l_paren, block, r_paren)), xs
			| _ ->
				let expr, xs = parse_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = l_paren &^ expr &^ r_paren in
				(ps, `paren (l_paren, expr, r_paren)), xs
			end
		end
	) and parse_postfix_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_postfix_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let handle_initializer (l_paren: [`l_paren] p) xs = (
			let tn, xs = parse_type_name error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let l_curly, xs = parse_l_curly_or_error error xs in
			let ilist, xs = parse_initializer_list_or_error error typedefs xs in
			let r_curly, xs = parse_r_curly_or_error error xs in
			let ps, () = l_paren & tn &^ r_paren &^ l_curly &^ ilist &^ r_curly in
			(ps, `compound (l_paren, tn, r_paren, l_curly, ilist, r_curly)), xs
		) in
		begin match xs with
		| lazy (`cons (lp_p, (`l_paren as lp_e),
			lazy (`cons (a, (#FirstSet.firstset_of_type_specifier' as it), xr)))) ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_initializer (lp_p, lp_e) xs
		| lazy (`cons (lp_p, (`l_paren as lp_e),
			lazy (`cons (a, (`ident name as it), xr))))
			when StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_initializer (lp_p, lp_e) xs
		| lazy (`cons (b_p, (`__builtin_va_arg as b_e), xs)) ->
			let builtin = b_p, b_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let arg, xs = parse_assignment_expression_or_error error typedefs xs in
			begin match xs with
			| lazy (`cons (cm_p, (`comma as cm_e), xs)) ->
				let comma = cm_p, cm_e in
				let typename, xs = parse_type_name_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = builtin & comma &^ typename &^ r_paren in
				(ps, `__builtin_va_arg (builtin, l_paren, arg, `some comma, typename, r_paren)), xs
			| _ ->
				error (LazyList.hd_a xs) "arguments mismatch for __builtin_va_arg(va_list, type).";
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = builtin &^ l_paren &^ arg &^ r_paren in
				(ps, `__builtin_va_arg (builtin, l_paren, arg, `error, `error, r_paren)), xs
			end
		| lazy (`cons (a, (#FirstSet.firstset_of_primary_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let rec loop (left, xs as result) = (
				begin match xs with
				| lazy (`cons (lb_p, (`l_bracket as lb_e), xs)) ->
					let l_bracket = lb_p, lb_e in
					let index, xs = parse_expression_or_error error typedefs xs in
					let r_bracket, xs = parse_r_bracket_or_error error xs in
					let ps, () = left & l_bracket &^ index &^ r_bracket in
					loop ((ps, `array_access (left, l_bracket, index, r_bracket)), xs)
				| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
					let l_paren = lp_p, lp_e in
					let args, xs = parse_argument_expression_list_option error typedefs xs in
					let r_paren, xs = parse_r_paren_or_error error xs in
					let item =
						let builtin_compare (fn: ranged_position * builtin_comparator) = (
							begin match args with
							| `some (_, `cons ((left_p, `nil left_e), comma, right)) ->
								let left = left_p, left_e in
								`__builtin_compare (fn, `some l_paren, `some left, `some comma, right, r_paren)
							| _ ->
								error (LazyList.hd_a xs) "arguments mismatch for builtin comparator.";
								`__builtin_compare (fn, `error, `error, `error, `error, `error)
							end
						) in
						begin match left with
						| fn_p, `ident "__builtin_constant_p" ->
							let fn = fn_p, `__builtin_constant_p in
							begin match args with
							| `some (arg_p, `nil arg_e) ->
								let arg = arg_p, arg_e in
								`__builtin_constant_p (fn, `some l_paren, `some arg, r_paren)
							| _ ->
								error (LazyList.hd_a xs) "arguments mismatch for __builtin_constant_p.";
								`__builtin_constant_p (fn, `error, `error, `error)
							end
						| fn_p, `ident "__builtin_expect" ->
							let fn = fn_p, `__builtin_expect in
							begin match args with
							| `some (_, `cons ((arg1_p, `nil arg1_e), comma, arg2)) ->
								let arg1 = arg1_p, arg1_e in
								`__builtin_expect (fn, `some l_paren, `some arg1, `some comma, arg2, r_paren)
							| _ ->
								error (LazyList.hd_a xs) "arguments mismatch for __builtin_expect.";
								`__builtin_expect (fn, `error, `error, `error, `error, `error)
							end
						| fn_p, `ident "__builtin_object_size" ->
							let fn = fn_p, `__builtin_object_size in
							begin match args with
							| `some (_, `cons ((arg1_p, `nil arg1_e), comma, arg2)) ->
								let arg1 = arg1_p, arg1_e in
								`__builtin_object_size (fn, `some l_paren, `some arg1, `some comma, arg2, r_paren)
							| _ ->
								error (LazyList.hd_a xs) "arguments mismatch for __builtin_object_size.";
								`__builtin_object_size (fn, `error, `error, `error, `error, `error)
							end
						| fn_p, `ident "__builtin_isgreater" ->
							builtin_compare (fn_p, `__builtin_isgreater)
						| fn_p, `ident "__builtin_isgreaterequal" ->
							builtin_compare (fn_p, `__builtin_isgreaterequal)
						| fn_p, `ident "__builtin_isless" ->
							builtin_compare (fn_p, `__builtin_isless)
						| fn_p, `ident "__builtin_islessequal" ->
							builtin_compare (fn_p, `__builtin_islessequal)
						| fn_p, `ident "__builtin_islessgreater" ->
							builtin_compare (fn_p, `__builtin_islessgreater)
						| fn_p, `ident "__builtin_isunordered" ->
							builtin_compare (fn_p, `__builtin_isunordered)
						| _ ->
							`function_call (left, l_paren, args, r_paren)
						end
					in
					let ps, () = left & l_paren &^ args &^ r_paren in
					loop ((ps, item), xs)
				| lazy (`cons (period_p, (`period as period_e), xs)) ->
					let period = period_p, period_e in
					let id, xs = parse_identifier_or_error error xs in
					let ps, () = left & period &^ id in
					loop ((ps, `element_access (left, period, id)), xs)
				| lazy (`cons (arrow_p, (`arrow as arrow_e), xs)) ->
					let arrow = arrow_p, arrow_e in
					let id, xs = parse_identifier_or_error error xs in
					let ps, () = left & arrow &^ id in
					loop ((ps, `dereferencing_element_access (left, arrow, id)), xs)
				| lazy (`cons (op_p, (`increment as op_e), xs)) ->
					let op = op_p, op_e in
					let ps, () = left & op in
					(ps, `post_increment (left, op)), xs
				| lazy (`cons (op_p, (`decrement as op_e), xs)) ->
					let op = op_p, op_e in
					let ps, () = left & op in
					(ps, `post_decrement (left, op)), xs
				| _ ->
					result
				end
			) in
			let result = parse_primary_expression error typedefs xs in
			loop result
		end
	) and parse_argument_expression_list
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_va_expression * 'a in_t] lazy_t)
		: argument_expression_list p * 'a in_t =
	(
		let rec loop rs xs = (
			begin match xs with
			| lazy (`cons (c_p, (`comma as c_e), xs)) ->
				let comma = c_p, c_e in
				let second, xs = parse_va_expression_or_error error typedefs xs in
				let ps, () = rs & comma &^ second in
				loop (ps, `cons (rs, comma, second)) xs
			| _ ->
				rs, xs
			end
		) in
		let (f_p, f_e), xs = parse_va_expression error typedefs xs in
		loop (f_p, `nil f_e) xs
	) and parse_argument_expression_list_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: argument_expression_list opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_va_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let result, xs = parse_argument_expression_list error typedefs xs in
			`some result, xs
		| _ ->
			`none, xs
		end
	) and parse_argument_expression_list_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: argument_expression_list e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_va_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let result, xs = parse_argument_expression_list error typedefs xs in
			`some result, xs
		| _ ->
			error (LazyList.hd_a xs) "argument-expression-list was expected.";
			`error, xs
		end
	) and parse_unary_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_unary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (incr_p, (`increment as incr_e), xs)) ->
			let op = incr_p, incr_e in
			let right, xs = parse_unary_expression_or_error error typedefs xs in
			let ps, () = op &^ right in
			(ps, `increment (op, right)), xs
		| lazy (`cons (decr_p, (`decrement as decr_e), xs)) ->
			let op = decr_p, decr_e in
			let right, xs = parse_unary_expression_or_error error typedefs xs in
			let ps, () = op &^ right in
			(ps, `decrement (op, right)), xs
		| lazy (`cons (unary_p, (#FirstSet.unary_operator as unary_e), xs)) ->
			let op = unary_p, unary_e in
			(* many compilers accept cast-expression, e.g. (~ (unsigned) 0) *)
			let right, xs = parse_cast_expression_or_error error typedefs xs in
			let ps, () = op &^ right in
			(ps, `unary (op, right)), xs
		| lazy (`cons (sizeof_p, (`SIZEOF as sizeof_e), xs)) ->
			let sizeof = sizeof_p, sizeof_e in
			let handle_sizeof_type sizeof l_paren xs = (
				let t, xs = parse_type_name error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let ps, () = sizeof & t &^ r_paren in
				(ps, `sizeof_type (sizeof, l_paren, t, r_paren)), xs
			) in
			begin match xs with
			| lazy (`cons (lp_p, (`l_paren as lp_e),
				lazy (`cons (a, (#FirstSet.firstset_of_type_name' as it), xr)))) ->
				let xs = lazy (`cons (a, it, xr)) in
				handle_sizeof_type sizeof (lp_p, lp_e) xs
			| lazy (`cons (lp_p, (`l_paren as lp_e),
				lazy (`cons (a, (`ident name as it), xr))))
				when StringSet.mem name typedefs ->
				let xs = lazy (`cons (a, it, xr)) in
				handle_sizeof_type sizeof (lp_p, lp_e) xs
			| _ ->
				let expr, xs = parse_unary_expression_or_error error typedefs xs in
				let ps, () = sizeof &^ expr in
				(ps, `sizeof_expr (sizeof, expr)), xs
			end
		| lazy (`cons (re_p, (`__real__ as re_e), xs)) ->
			let op = re_p, re_e in
			let right, xs = parse_unary_expression_or_error error typedefs xs in
			let ps, () = op &^ right in
			(ps, `real (op, right)), xs
		| lazy (`cons (im_p, (`__imag__ as im_e), xs)) ->
			let op = im_p, im_e in
			let right, xs = parse_unary_expression_or_error error typedefs xs in
			let ps, () = op &^ right in
			(ps, `imag (op, right)), xs
		| lazy (`cons (a, (#FirstSet.firstset_of_postfix_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			parse_postfix_expression error typedefs xs
		end
	) and parse_unary_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_unary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_unary_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "unary-expression was expected.";
			`error, xs
		end
	) and parse_cast_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_cast_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let handle_cast l_paren xs = (
			let tn, xs = parse_type_name error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			begin match xs with
			| lazy (`cons (lc_p, (`l_curly as lc_e), xs)) -> (* compound literal *)
				let l_curly = lc_p, lc_e in
				let ilist, xs = parse_initializer_list_or_error error typedefs xs in
				let r_curly, xs = parse_r_curly_or_error error xs in
				let ps, () = l_paren & l_curly &^ ilist &^ r_curly in
				(ps, `compound (l_paren, tn, r_paren, `some l_curly, ilist, r_curly)), xs
			| _ ->
				let expr, xs = parse_cast_expression_or_error error typedefs xs in
				let ps, () = l_paren & tn &^ r_paren &^ expr in
				(ps, `cast (l_paren, tn, r_paren, expr)), xs
			end
		) in
		begin match xs with
		| lazy (`cons (lp_p, (`l_paren as lp_e),
			lazy (`cons (a, (#FirstSet.firstset_of_type_name' as it), xr)))) ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_cast (lp_p, lp_e) xs
		| lazy (`cons (lp_p, (`l_paren as lp_e),
			lazy (`cons (a, (`ident name as it), xr))))
			when StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_cast (lp_p, lp_e) xs
		| lazy (`cons (ex_p, (`__extension__ as ex_e), xs)) ->
			let op = ex_p, ex_e in
			let right, xs = parse_cast_expression_or_error error typedefs xs in
			let ps, () = op &^ right in
			(ps, `extension (op, right)), xs
		| lazy (`cons (a, (#FirstSet.firstset_of_unary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			parse_unary_expression error typedefs xs
		end
	) and parse_cast_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_cast_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_cast_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "cast-expression was expected.";
			`error, xs
		end
	) and parse_multicative_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`asterisk as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_cast_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`mul (left, op, right))), xs)
			| lazy (`cons (op_p, (`slash as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_cast_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`div (left, op, right))), xs)
			| lazy (`cons (op_p, (`percent as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_cast_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`rem (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_cast_expression error typedefs xs in
		loop result
	) and parse_multicative_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_multicative_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "multicative-expression was expected.";
			`error, xs
		end
	) and parse_additive_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`plus as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_multicative_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`add (left, op, right))), xs)
			| lazy (`cons (op_p, (`minus as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_multicative_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`sub (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_multicative_expression error typedefs xs in
		loop result
	) and parse_additive_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_additive_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "additive-expression was expected.";
			`error, xs
		end
	) and parse_shift_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`l_shift as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_additive_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`l_shift (left, op, right))), xs)
			| lazy (`cons (op_p, (`r_shift as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_additive_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`r_shift (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_additive_expression error typedefs xs in
		loop result
	) and parse_shift_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_shift_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "shift-expression was expected.";
			`error, xs
		end
	) and parse_relational_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`lt as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_shift_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`lt (left, op, right))), xs)
			| lazy (`cons (op_p, (`gt as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_shift_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`gt (left, op, right))), xs)
			| lazy (`cons (op_p, (`le as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_shift_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`le (left, op, right))), xs)
			| lazy (`cons (op_p, (`ge as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_shift_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`ge (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_shift_expression error typedefs xs in
		loop result
	) and parse_relational_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_relational_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "relational-expression was expected.";
			`error, xs
		end
	) and parse_equality_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (eq_p, (`eq as eq_e), xs)) ->
				let op = eq_p, eq_e in
				let right, xs = parse_relational_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`eq (left, op, right))), xs)
			| lazy (`cons (ne_p, (`ne as ne_e), xs)) ->
				let op = ne_p, ne_e in
				let right, xs = parse_relational_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`ne (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_relational_expression error typedefs xs in
		loop result
	) and parse_equality_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_equality_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "equality-expression was expected.";
			`error, xs
		end
	) and parse_and_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`ampersand as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_equality_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`bit_and (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_equality_expression error typedefs xs in
		loop result
	) and parse_and_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_and_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "AND-expression was expected.";
			`error, xs
		end
	) and parse_exclusive_or_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`caret as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_and_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`bit_xor (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_and_expression error typedefs xs in
		loop result
	) and parse_exclusive_or_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_exclusive_or_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "exclusive-OR-expression was expected.";
			`error, xs
		end
	) and parse_inclusive_or_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`vertical as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_exclusive_or_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`bit_or (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_exclusive_or_expression error typedefs xs in
		loop result
	) and parse_inclusive_or_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_inclusive_or_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "inclusive-OR-expression was expected.";
			`error, xs
		end
	) and parse_logical_and_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`and_then as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_inclusive_or_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`and_then (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_inclusive_or_expression error typedefs xs in
		loop result
	) and parse_logical_and_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_binary_op_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_logical_and_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "logical-AND-expression was expected.";
			`error, xs
		end
	) and parse_logical_or_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_binary_op_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`or_else as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_logical_and_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`or_else (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_logical_and_expression error typedefs xs in
		loop result
	) and parse_conditional_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_conditional_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let cond, xs = parse_logical_or_expression error typedefs xs in
		begin match xs with
		| lazy (`cons (q_p, (`question as q_e), xs)) ->
			let q = q_p, q_e in
			let true_case, xs = parse_expression_or_error error typedefs xs in
			let colon, xs = parse_colon_or_error error xs in
			let false_case, xs = parse_conditional_expression_or_error error typedefs xs in
			let ps, () = cond &^ true_case &^ colon &^ false_case in
			(ps, `cond (cond, q, true_case, colon, false_case)), xs
		| _ ->
			cond, xs
		end
	) and parse_conditional_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_conditional_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_conditional_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "conditional-expression was expected.";
			`error, xs
		end
	) and parse_assignment_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_assignment_expression * 'a in_t] lazy_t)
		: assignment_expression p * 'a in_t =
	(
		(* informal... in RM, unary-expression = assinment-expression when `assign case *)
		let left, xs = parse_conditional_expression error typedefs xs in
		begin match xs with
		| lazy (`cons (op_p, (#FirstSet.assignment_operator as op_e), xs)) ->
			let op = op_p, op_e in
			let right, xs = parse_assignment_expression_or_error error typedefs xs in
			let ps, () = left & op &^ right in
			(ps, `assign (left, op, right)), xs
		| _ ->
			left, xs
		end
	) and parse_assignment_expression_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: assignment_expression opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_assignment_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_assignment_expression error typedefs xs in
			`some r, xs
		| _ ->
			`none, xs
		end
	) and parse_assignment_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: assignment_expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_assignment_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_assignment_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "assignment-expression was expected.";
			`error, xs
		end
	) and parse_va_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_va_expression * 'a in_t] lazy_t)
		: assignment_expression p * 'a in_t =
	(
		(* assignment-expression or __VA_ARGS__ *)
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_assignment_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			parse_assignment_expression error typedefs xs
		| lazy (`cons (va_p, (`__VA_ARGS__ as va_e), xs)) ->
			let va = va_p, va_e in
			va, xs
		end
	) and parse_va_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: assignment_expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_va_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let result, xs = parse_va_expression error typedefs xs in
			`some result, xs
		| _ ->
			error (LazyList.hd_a xs) "assignment-expression was expected.";
			`error, xs
		end
	) and parse_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_expression * 'a in_t] lazy_t)
		: expression p * 'a in_t =
	(
		let rec loop (left, xs as result) = (
			begin match xs with
			| lazy (`cons (op_p, (`comma as op_e), xs)) ->
				let op = op_p, op_e in
				let right, xs = parse_assignment_expression_or_error error typedefs xs in
				let ps, () = left & op &^ right in
				loop ((ps, (`comma (left, op, right))), xs)
			| _ ->
				result
			end
		) in
		let result = parse_assignment_expression error typedefs xs in
		loop result
	) and parse_expression_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_expression error typedefs xs in
			`some r, xs
		| _ ->
			`none, xs
		end
	) and parse_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "expression was expected.";
			`error, xs
		end
	) and parse_constant_expression
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_constant_expression * 'a in_t] lazy_t)
		: constant_expression p * 'a in_t =
	(
		parse_conditional_expression error typedefs xs
	) and parse_constant_expression_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: constant_expression e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_constant_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_constant_expression error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "constant-expression was expected.";
			`error, xs
		end
	) and parse_declaration
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_declaration_specifiers' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: declaration p * typedef_set * 'a in_t =
	(
		let spec, xs = parse_declaration_specifiers error typedefs xs in
		let decl, xs = parse_init_declarator_list_option error typedefs xs in
		let typedefs =
			if has_typedef spec then add_declarators_to_typedef_set decl typedefs else
			typedefs
		in
		let semicolon, xs = parse_semicolon_or_error error xs in
		let ps, () = spec &^ decl &^ semicolon in
		(ps, (spec, decl, semicolon)), typedefs, xs
	) and parse_declaration_specifiers
		?(has_type: bool = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_declaration_specifiers' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: declaration_specifiers p * 'a in_t =
	(
		let handle_type_specifier xs = (
			let spec, xs = parse_type_specifier error typedefs xs in
			let next, xs = parse_declaration_specifiers_option ~has_type:true error typedefs xs in
			let ps, () = spec &^ next in
			(ps, `type_specifier (spec, next)), xs
		) in
		begin match xs with
		| lazy (`cons (spec_p, (#FirstSet.storage_class_specifier as spec_e), xs)) ->
			let spec = (spec_p, spec_e) in
			let next, xs = parse_declaration_specifiers_option ~has_type error typedefs xs in
			let ps, () = spec &^ next in
			(ps, `storage_class_specifier (spec, next)), xs
		| lazy (`cons (a, (#FirstSet.firstset_of_type_specifier' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_type_specifier xs
		| lazy (`cons (a, (`ident name as it), xr))
			when not has_type && StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_type_specifier xs
		| lazy (`cons (q_p, (#FirstSet.type_qualifier as q_e), xs)) ->
			let q = (q_p, q_e) in
			let next, xs = parse_declaration_specifiers_option ~has_type error typedefs xs in
			let ps, () = q &^ next in
			(ps, `type_qualifier (q, next)), xs
		| lazy (`cons (spec_p, (`INLINE | `__inline | `__inline__ as spec_k), xs)) ->
			let spec_e = if Language.gnu_inline then `gnu_inline spec_k else `inline spec_k in
			let spec = (spec_p, spec_e) in
			let next, xs = parse_declaration_specifiers_option ~has_type error typedefs xs in
			let ps, () = spec &^ next in
			(ps, `function_specifier (spec, next)), xs
		| lazy (`cons (a, (`__attribute__ as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let attr, xs = parse_attribute error typedefs xs in
			let next, xs = parse_declaration_specifiers_option ~has_type error typedefs xs in
			let ps, () = attr &^ next in
			(ps, `attributes (attr, next)), xs
		| lazy (`cons (ex_p, (`__extension__ as ex_e), xs)) ->
			let ex = ex_p, ex_e in
			let next, xs = parse_declaration_specifiers_option ~has_type error typedefs xs in
			let ps, () = ex &^ next in
			(ps, `extension (ex, next)), xs
		| lazy (`cons ((first, _ as ps), (`ident _), _)) ->
			error ps "declaration-specifiers was expected.";
			let f_p = first, first in
			(f_p, `type_specifier ((f_p, `typedef_name (`ident "")), `none)), (xs :> 'a in_t)
		end
	) and parse_declaration_specifiers_option
		?(has_type: bool = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: declaration_specifiers opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_declaration_specifiers ~has_type error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`ident name as it), xr))
			when not has_type && StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_declaration_specifiers ~has_type error typedefs xs in
			`some r, xs
		| _ ->
			`none, xs
		end
	) and parse_declaration_specifiers_or_error
		?(has_type: bool = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: declaration_specifiers e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_declaration_specifiers ~has_type error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`ident name as it), xr))
			when not has_type && StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_declaration_specifiers ~has_type error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "declaration-specifiers was expected.";
			`error, xs
		end
	) and parse_init_declarator_list_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: init_declarator_list opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let rec loop rs typedefs xs = (
				begin match xs with
				| lazy (`cons (cm_p, (`comma as cm_e), xs)) ->
					let comma = cm_p, cm_e in
					let second, xs = parse_init_declarator_or_error error typedefs xs in
					let ps, () = rs & comma &^ second in
					loop (ps, (`cons (rs, comma, second))) typedefs xs
				| _ ->
					`some rs, xs
				end
			) in
			let (f_p, f_e), xs = parse_init_declarator error typedefs xs in
			loop (f_p, (`nil f_e)) typedefs xs
		| _ ->
			`none, xs
		end
	) and parse_init_declarator
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_declarator * 'a in_t] lazy_t)
		: init_declarator p * 'a in_t =
	(
		let decl, xs = parse_declarator error typedefs
			(xs :> [`cons of ranged_position * [FirstSet.firstset_of_declarator | `chars_literal of string] * 'a in_t] lazy_t)
		in
		begin match xs with
		| lazy (`cons (ass_p, (`assign as ass_e), xs)) ->
			let assign = ass_p, ass_e in
			let init, xs = parse_initializer_or_error error typedefs xs in
			let ps, () = decl & assign &^ init in
			(ps, `with_init (decl, assign, init)), xs
		| _ ->
			let ps, decl = decl in
			(ps, `no_init decl), xs
		end
	) and parse_init_declarator_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: init_declarator e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_init_declarator error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "init-declarator was expected.";
			`error, xs
		end
	) and parse_type_specifier
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_type_specifier' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: type_specifier p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (spec_p, (#FirstSet.simple_type_specifier as spec_e), xs)) ->
			(spec_p, spec_e), xs
		| lazy (`cons (a, (#FirstSet.struct_or_union as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let (ps, sous), xs = parse_struct_or_union_specifier error typedefs xs in
			(ps, `struct_or_union_specifier sous), xs
		| lazy (`cons (a, (`ENUM as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let (ps, spec), xs = parse_enum_specifier error typedefs xs in
			(ps, `enum_specifier spec), xs
		| lazy (`cons (td_p, (`ident n as td_e), xs)) ->
			if not (StringSet.mem n typedefs) then failwith "parse_type_specifier" else
			(td_p, `typedef_name td_e), xs
		end
	) and parse_struct_or_union_specifier
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.struct_or_union * 'a in_t] lazy_t)
		: struct_or_union_specifier p * 'a in_t =
	(
		let handle_body h attrs1 n l_c xs = (
			let decls, xs = parse_struct_declaration_list_or_error error typedefs xs in
			let r_curly, xs = parse_r_curly_or_error error xs in
			let attrs2, xs = parse_attribute_list_option error typedefs xs in
			let ps, () = h & l_c &^ decls &^ r_curly &^ attrs2 in
			(ps, `with_body (h, attrs1, n, l_c, decls, r_curly, attrs2)), xs
		) in
		begin match xs with
		| lazy (`cons (su_p, (#FirstSet.struct_or_union as su_e), xs)) ->
			let h = (su_p, su_e) in
			let attrs1, xs = parse_attribute_list_option error typedefs xs in
			begin match xs with
			| lazy (`cons (lc_p, (`l_curly as lc_e), xs)) ->
				let l_c = lc_p, lc_e in
				handle_body h attrs1 `none l_c xs
			| _ ->
				let id, xs = parse_identifier_or_error error xs in
				begin match xs, id with
				| lazy (`cons (lc_p, (`l_curly as lc_e), xs)), (`some _ as id) ->
					let l_curly = lc_p, lc_e in
					handle_body h attrs1 id l_curly xs
				| _ ->
					begin match attrs1 with
					| `some (attr1_p, _) ->
						error attr1_p "attribute-list was specified without body."
					| `none ->
						()
					end;
					let ps, () = h &^ id in
					(ps, `no_body (h, id)), xs
				end
			end
		end
	) and parse_struct_declaration_list
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_struct_declaration' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: struct_declaration_list p * 'a in_t =
	(
		let rec loop (rs: struct_declaration_list p) (xs: 'a in_t): struct_declaration_list p * 'a in_t = (
			let handle_cons xs = (
				let param, xs = parse_struct_declaration error typedefs xs in
				let ps, () = rs & param in
				loop (ps, `cons (rs, param)) xs
			) in
			begin match xs with
			| lazy (`cons (a, (#FirstSet.firstset_of_struct_declaration' as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				handle_cons xs
			| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
				let xs = lazy (`cons (a, it, xr)) in
				handle_cons xs
			| _ ->
				rs, xs
			end
		) in
		let (rp, r), xs = parse_struct_declaration error typedefs xs in
		loop (rp, `nil r) xs
	) and parse_struct_declaration_list_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: struct_declaration_list e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_struct_declaration' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_struct_declaration_list error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_struct_declaration_list error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "struct-declaration-list was expected.";
			`error, xs
		end
	) and parse_struct_declaration
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_struct_declaration' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: struct_declaration p * 'a in_t =
	(
		let merge_specifier_qualifier_list sou q = (
			let sou_p, sou_e = sou in
			let q1 = sou_p, `struct_or_union_specifier sou_e in
			let q_ps, () = q1 &^ q in
			q_ps, `type_specifier (q1, q)
		) in
		begin match xs with
		| lazy (`cons (ex_p, (`__extension__ as ex_e), xs)) ->
			let ex = ex_p, ex_e in
			begin match xs with
			| lazy (`cons (a, (#FirstSet.struct_or_union as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				let sou, xs = parse_struct_or_union_specifier error typedefs xs in
				begin match xs with
				| lazy (`cons (semicolon_p, (`semicolon as semicolon_e), xs)) ->
					let semicolon = semicolon_p, semicolon_e in
					let ps, () = ex & semicolon in
					(ps, `anonymous_struct_or_union (`some ex, sou, `some semicolon)), xs
				| _ ->
					let q, xs = parse_specifier_qualifier_list_option ~has_type:true error typedefs xs in
					let q = merge_specifier_qualifier_list sou q in
					let d, xs = parse_struct_declarator_list_or_error error typedefs xs in
					let semicolon, xs = parse_semicolon_or_error error xs in
					let ps, () = ex & q &^ d &^ semicolon in
					(ps, `named (`none, `some q, d, semicolon)), xs
				end
			| _ ->
				let q, xs = parse_specifier_qualifier_list_or_error error typedefs xs in
				let d, xs = parse_struct_declarator_list_or_error error typedefs xs in
				let semicolon, xs = parse_semicolon_or_error error xs in
				let ps, () = ex &^ q &^ d &^ semicolon in
				(ps, `named (`some ex, q, d, semicolon)), xs
			end
		| lazy (`cons (a, (#FirstSet.struct_or_union as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let sou, xs = parse_struct_or_union_specifier error typedefs xs in
			begin match xs with
			| lazy (`cons (semicolon_p, (`semicolon as semicolon_e), xs)) ->
				let semicolon = semicolon_p, semicolon_e in
				let ps, () = sou & semicolon in
				(ps, `anonymous_struct_or_union (`none, sou, `some semicolon)), xs
			| _ ->
				let q, xs = parse_specifier_qualifier_list_option ~has_type:true error typedefs xs in
				let q = merge_specifier_qualifier_list sou q in
				let d, xs = parse_struct_declarator_list_or_error error typedefs xs in
				let semicolon, xs = parse_semicolon_or_error error xs in
				let ps, () = q &^ d &^ semicolon in
				(ps, `named (`none, `some q, d, semicolon)), xs
			end
		| lazy (`cons (a, (#FirstSet.firstset_of_specifier_qualifier_list' | #FirstSet.typedef_name as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let q, xs = parse_specifier_qualifier_list error typedefs xs in
			let d, xs = parse_struct_declarator_list_or_error error typedefs xs in
			let semicolon, xs = parse_semicolon_or_error error xs in
			let ps, () = q &^ d &^ semicolon in
			(ps, `named (`none, `some q, d, semicolon)), xs
		end
	) and parse_specifier_qualifier_list
		?(has_type: bool = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_specifier_qualifier_list' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: specifier_qualifier_list p * 'a in_t =
	(
		let handle_type_specifier xs = (
			let spec, xs = parse_type_specifier error typedefs xs in
			let next, xs = parse_specifier_qualifier_list_option ~has_type:true error typedefs xs in
			let ps, () = spec &^ next in
			(ps, `type_specifier (spec, next)), xs
		) in
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_type_specifier' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_type_specifier xs
		| lazy (`cons (q_p, (#FirstSet.type_qualifier as q_e), xs)) ->
			let q = q_p, q_e in
			let next, xs = parse_specifier_qualifier_list_option ~has_type error typedefs xs in
			let ps, () = q &^ next in
			(ps, `type_qualifier (q, next)), xs
		| lazy (`cons (a, (`ident name as it), xr)) ->
			if has_type || not (StringSet.mem name typedefs)
			then failwith "parse_specifier_qualifier_list" else
			let xs = lazy (`cons (a, it, xr)) in
			handle_type_specifier xs
		end
	) and parse_specifier_qualifier_list_option
		?(has_type: bool = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: specifier_qualifier_list opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_specifier_qualifier_list' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_specifier_qualifier_list ~has_type error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`ident name as it), xr))
			when not has_type && StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_specifier_qualifier_list ~has_type error typedefs xs in
			`some r, xs
		| _ ->
			`none, xs
		end
	) and parse_specifier_qualifier_list_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: specifier_qualifier_list e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_specifier_qualifier_list' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_specifier_qualifier_list error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_specifier_qualifier_list error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "specifier-qualifier-list was expected.";
			`error, xs
		end
	) and parse_struct_declarator_list_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: struct_declarator_list e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_struct_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let rec loop rs xs = (
				begin match xs with
				| lazy (`cons (c_p, (`comma as c_e), xs)) ->
					let comma = c_p, c_e in
					let next, xs = parse_struct_declarator_or_error error typedefs xs in
					let ps, () = rs & comma &^ next in
					let r = ps, (`cons (rs, comma, next)) in
					loop r xs
				| _ ->
					`some rs, xs
				end
			) in
			let (p, d), xs = parse_struct_declarator error typedefs xs in
			loop (p, (`nil d)) xs
		| _ ->
			let ps = LazyList.hd_a xs in
			error ps "struct-declarator-list was expected.";
			`error, xs
		end
	) and parse_struct_declarator
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_struct_declarator * 'a in_t] lazy_t)
		: struct_declarator p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (c_p, (`colon as c_e), xs)) ->
			let colon = c_p, c_e in
			let bw, xs = parse_constant_expression_or_error error typedefs xs in
			let ps, () = colon &^ bw in
			(ps, (`bit_width (`none, colon, bw))), xs
		| lazy (`cons (a, (#FirstSet.firstset_of_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let d, xs = parse_declarator error typedefs xs in
			begin match xs with
			| lazy (`cons (c_p, (`colon as c_e), xs)) ->
				let colon = c_p, c_e in
				let bw, xs = parse_constant_expression_or_error error typedefs xs in
				let ps, () = d & colon &^ bw in
				(ps, (`bit_width (`some d, colon, bw))), xs
			| _ ->
				let p, d = d in
				(p, (`item d)), xs
			end
		end
	) and parse_struct_declarator_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: struct_declarator e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_struct_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_struct_declarator error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "struct-declarator was expected.";
			`error, xs
		end
	) and parse_enum_specifier
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [`ENUM] * 'a in_t] lazy_t)
		: enum_specifier p * 'a in_t =
	(
		let handle_body enum id l_curly xs = (
			let list, xs = parse_enumerator_list_or_error error typedefs xs in
			let comma, xs =
				begin match xs with
				| lazy (`cons (cm_p, (`comma as cm_e), xs)) ->
					let comma = cm_p, cm_e in
					`some comma, xs
				| _ ->
					`none, xs
				end
			in
			let r_curly, xs = parse_r_curly_or_error error xs in
			let ps, () = enum & l_curly &^ list &^ comma &^ r_curly in
			(ps, `with_body (enum, id, l_curly, list, comma, r_curly)), xs
		) in
		begin match xs with
		| lazy (`cons (enum_p, (`ENUM as enum_e), xs)) ->
			let enum = enum_p, enum_e in
			begin match xs with
			| lazy (`cons (lc_p, (`l_curly as lc_e), xs)) ->
				let l_curly = lc_p, lc_e in
				handle_body enum `none l_curly xs
			| _ ->
				let id, xs = parse_identifier_or_error error xs in
				begin match xs, id with
				| lazy (`cons (lc_p, (`l_curly as lc_e), xs)), (`some _ as id) ->
					let l_curly = lc_p, lc_e in
					handle_body enum id l_curly xs
				| _ ->
					let ps, () = enum &^ id in
					(ps, `no_body (enum, id)), xs
				end
			end
		end
	) and parse_enumerator_list_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: enumerator_list e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.identifier as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let rec loop rs xs = (
				begin match xs with
				| lazy (`cons (cm_p, (`comma as cm_e),
					(lazy (`cons (_, `ident _, _)) as xs))) -> (* last comma would be parsed in parse_enum_specifier *)
					let comma = cm_p, cm_e in
					let second, xs = parse_enumerator_or_error error typedefs xs in
					let ps, () = rs & comma &^ second in
					loop (ps, (`cons (rs, comma, second))) xs
				| _ ->
					`some rs, xs
				end
			) in
			let (first_p, first_e), xs = parse_enumerator error typedefs xs in
			loop (first_p, (`nil first_e)) xs
		| _ ->
			error (LazyList.hd_a xs) "enumerator-list was expected.";
			`error, xs
		end
	) and parse_enumerator
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.identifier * 'a in_t] lazy_t)
		: enumerator p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (ec_p, `ident ec_e, xs)) ->
			let ec = ec_p, ec_e in
			begin match xs with
			| lazy (`cons (ass_p, (`assign as ass_e), xs)) ->
				let assign = ass_p, ass_e in
				let expr, xs = parse_constant_expression_or_error error typedefs xs in
				let ps, () = ec & assign &^ expr in
				(ps, `with_repr (ec, assign, expr)), xs
			| _ ->
				(ec_p, `no_repr ec_e), xs
			end
		end
	) and parse_enumerator_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: enumerator e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.identifier as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_enumerator error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "enumeration-constant was expected.";
			`error, xs
		end
	) and parse_declarator
		?(use_string = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_declarator | `chars_literal of string] * 'a in_t] lazy_t)
		: declarator p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_pointer as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let pointer, xs = parse_pointer error typedefs xs in
			let dd, xs = parse_direct_declarator_or_error ~use_string error typedefs xs in
			let attrs, xs = parse_attribute_list_option error typedefs xs in
			let ps, () = pointer &^ dd &^ attrs in
			(ps, (`some pointer, dd, attrs)), xs
		| lazy (`cons (a, (#FirstSet.firstset_of_direct_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let dd, xs = parse_direct_declarator ~use_string error typedefs xs in
			let attrs, xs = parse_attribute_list_option error typedefs xs in
			let ps, () = dd &^ attrs in
			(ps, (`none, `some dd, attrs)), xs
		| lazy (`cons (a, (`chars_literal _ as it), xr)) ->
			if not use_string then failwith "parse_declarator" else
			let xs = lazy (`cons (a, it, xr)) in
			let dd, xs = parse_direct_declarator ~use_string error typedefs xs in
			let attrs, xs = parse_attribute_list_option error typedefs xs in
			let ps, () = dd &^ attrs in
			(ps, (`none, `some dd, attrs)), xs
		end
	) and parse_declarator_or_error
		?(use_string = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: declarator e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_declarator ~use_string error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`chars_literal _ as it), xr)) when use_string ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_declarator ~use_string error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "declarator was expected.";
			`error, xs
		end
	) and parse_direct_declarator
		?(use_string = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_direct_declarator | `chars_literal of string] * 'a in_t] lazy_t)
		: direct_declarator p * 'a in_t =
	(
		let rec loop (item: direct_declarator p) (xs: 'a in_t) = (
			begin match xs with
			| lazy (`cons (lb_p, (`l_bracket as lb_e), xs)) ->
				let l_bracket = lb_p, lb_e in
				let tql, xs = parse_type_qualifier_list_option error typedefs xs in
				let ass, xs = parse_assignment_expression_option error typedefs xs in
				let r_bracket, xs = parse_r_bracket_or_error error xs in
				let ps, () = item & l_bracket &^ tql &^ ass &^ r_bracket in
				loop (ps, `array (item, l_bracket, tql, ass, r_bracket)) xs
			| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
				let l_paren = lp_p, lp_e in
				let handle_function_type xs = (
					let params, xs = parse_parameter_type_list error typedefs xs in
					let r_paren, xs = parse_r_paren_or_error error xs in
					let ps, () = item & params &^ r_paren in
					loop (ps, `function_type (item, l_paren, params, r_paren)) xs
				) in
				begin match xs with
				| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' as it), xr)) ->
					let xs = lazy (`cons (a, it, xr)) in
					handle_function_type xs
				| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
					let xs = lazy (`cons (a, it, xr)) in
					handle_function_type xs
				| _ ->
					(* old style function *)
					let params, xs = parse_identifier_list_option error typedefs xs in
					let r_paren, xs = parse_r_paren_or_error error xs in
					let ps, () = item & l_paren &^ params &^ r_paren in
					loop (ps, `old_function_type (item, l_paren, params, r_paren)) xs
				end
			| _ ->
				item, xs
			end
		) in
		begin match xs with
		| lazy (`cons (id_p, (#FirstSet.identifier as id_e), xs)) ->
			loop (id_p, id_e) xs
		| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
			let l_paren = lp_p, lp_e in
			let attrs, xs = parse_attribute_list_option error typedefs xs in
			let d, xs = parse_declarator_or_error ~use_string error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let ps, () = l_paren &^ attrs &^ d &^ r_paren in
			loop (ps, `paren (l_paren, attrs, d, r_paren)) xs
		| lazy (`cons (id_p, (`chars_literal ident), xs)) ->
			if not use_string then failwith "parse_direct_declarator" else
			loop (id_p, `ident ident) xs
		end
	) and parse_direct_declarator_or_error
		?(use_string = false)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: direct_declarator e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_direct_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_direct_declarator ~use_string error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`chars_literal _ as it), xr)) when use_string ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_direct_declarator ~use_string error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "direct-declarator was expected.";
			`error, xs
		end
	) and parse_pointer
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_pointer * 'a in_t] lazy_t)
		: pointer p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a_p, (#FirstSet.firstset_of_pointer as a_e), xs)) ->
			let pointer_mark = a_p, a_e in
			let q, xs = parse_type_qualifier_list_option error typedefs xs in
			begin match xs with
			| lazy (`cons (a, (#FirstSet.firstset_of_pointer as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				let next, xs = parse_pointer error typedefs xs in
				let ps, () = pointer_mark & next in
				(ps, `cons (pointer_mark, q, next)), xs
			| _ ->
				let attrs, xs = parse_attribute_list_option error typedefs xs in
				let ps, () = pointer_mark &^ q &^ attrs in
				(ps, `nil (pointer_mark, q, attrs)), xs
			end
		end
	) and parse_type_qualifier_list_option
		(_: ranged_position -> string -> unit)
		(_: typedef_set)
		(xs: 'a in_t)
		: type_qualifier_list opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (q_p, (#FirstSet.type_qualifier as q_e), xs)) ->
			let rec loop (rs: type_qualifier_list p) (xs: 'a in_t) = (
				begin match xs with
				| lazy (`cons (q_p, (#FirstSet.type_qualifier as q_e), xs)) ->
					let q = q_p, q_e in
					let ps, () = rs & q in
					loop (ps, `cons (rs, q)) xs
				| _ ->
					`none, xs
				end
			) in
			loop (q_p, `nil q_e) xs
		| _ ->
			`none, xs
		end
	) and parse_parameter_type_list
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_declaration_specifiers' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: parameter_type_list p * 'a in_t =
	(
		let params, xs = parse_parameter_list error typedefs xs in
		begin match xs with
		| lazy (`cons (c_p, (`comma as c_e), (lazy (`cons (v_p, (`varargs as v_e), xs))))) ->
			let comma = c_p, c_e in
			let varargs = v_p, v_e in
			let ps, () = params & varargs in
			(ps, `varargs (params, comma, varargs)), xs
		| _ ->
			let ps = fst params in
			(ps, `args (snd params)), xs
		end
	) and parse_parameter_list
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_declaration_specifiers' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: parameter_list p * 'a in_t =
	(
		let rec loop (rs: parameter_list p) (xs: 'a in_t): parameter_list p * 'a in_t = (
			begin match xs with
			| lazy (`cons (_, `comma, lazy (`cons (_, `varargs, _)))) ->
				rs, xs
			| lazy (`cons (c_p, (`comma as c_e), xs)) ->
				let comma = c_p, c_e in
				let param, xs = parse_parameter_declaration_or_error error typedefs xs in
				let ps, () = rs & comma &^ param in
				loop (ps, `cons (rs, comma, param)) xs
			| _ ->
				rs, xs
			end
		) in
		let (rp, r), xs = parse_parameter_declaration error typedefs xs in
		loop (rp, `nil r) xs
	) and parse_parameter_declaration
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_declaration_specifiers' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: parameter_declaration p * 'a in_t =
	(
		let has_error = ref false in
		let dummy_error _ _ = (has_error := true) in
		let spec, xs = parse_declaration_specifiers error typedefs xs in
		let decl, d_xs = parse_declarator_or_error dummy_error typedefs xs in
		begin match decl with
		| `some decl when not !has_error ->
			let ps, () = spec & decl in
			(ps, (`with_name (spec, decl))), d_xs
		| _ ->
			let decl, a_xs = parse_abstract_declarator_option error typedefs xs in
			let ps, () = spec &^ decl in
			(ps, (`no_name (spec, decl))), a_xs
		end
	) and parse_parameter_declaration_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: parameter_declaration e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_parameter_declaration error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_parameter_declaration error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "parameter-declaration was expected.";
			`error, xs
		end
	) and parse_identifier_list
		(error: ranged_position -> string -> unit)
		(_: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.identifier * 'a in_t] lazy_t)
		: identifier_list p * 'a in_t =
	(
		let rec loop (rs: identifier_list p) (xs: 'a in_t) = (
			begin match xs with
			| lazy (`cons (comma_p, (`comma as comma_e), xs)) ->
				let comma = (comma_p, comma_e) in
				begin match xs with
				| lazy (`cons (ident_p, (#FirstSet.identifier as ident_e), xs)) ->
					let ident = ident_p, ident_e in
					let ps, () = rs & ident in
					loop (ps, `cons (rs, comma, `some ident)) xs
				| _ ->
					error (LazyList.hd_a xs) "identifier was expected.";
					let ps, () = rs & comma in
					(ps, `cons (rs, comma, `error)), xs
				end
			| _ ->
				rs, xs
			end
		) in
		let lazy (`cons (ident_p, (#FirstSet.identifier as ident_e), xs)) = xs in
		loop (ident_p, `nil ident_e) xs
	) and parse_identifier_list_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: identifier_list opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.identifier as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let list, xs = parse_identifier_list error typedefs xs in
			`some list, xs
		| _ ->
			`none, xs
		end
	) and parse_type_name
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_type_name' | FirstSet.typedef_name] * 'a in_t] lazy_t)
		: type_name p * 'a in_t =
	(
		let spec, xs = parse_specifier_qualifier_list error typedefs xs in
		let decl, xs = parse_abstract_declarator_option error typedefs xs in
		let ps, () = spec &^ decl in
		(ps, (spec, decl)), xs
	) and parse_type_name_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: type_name e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_type_name' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_type_name error typedefs xs in
			`some r, xs
		| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_type_name error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "type-name was expected.";
			`error, xs
		end
	) and parse_abstract_declarator
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_abstract_declarator * 'a in_t] lazy_t)
		: abstract_declarator p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_direct_abstract_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let dd, xs = parse_direct_abstract_declarator error typedefs xs in
			let ps = fst dd in
			(ps, `declarator (`none, dd)), xs
		| lazy (`cons (a, (#FirstSet.firstset_of_pointer as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let pointer, xs = parse_pointer error typedefs xs in
			begin match xs with
			| lazy (`cons (a, (#FirstSet.firstset_of_direct_abstract_declarator as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				let dd, xs = parse_direct_abstract_declarator error typedefs xs in
				let ps, () = pointer & dd in
				(ps, `declarator (`some pointer, dd)), xs
			| _ ->
				let ps, pointer = pointer in
				(ps, `pointer pointer), xs
			end
		end
	) and parse_abstract_declarator_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: abstract_declarator opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_abstract_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_abstract_declarator error typedefs xs in
			`some r, xs
		| _ ->
			`none, xs
		end
	) and parse_abstract_declarator_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: abstract_declarator e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_abstract_declarator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_abstract_declarator error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "abstract-declarator was expected.";
			`error, xs
		end
	) and parse_direct_abstract_declarator
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_direct_abstract_declarator * 'a in_t] lazy_t)
		: direct_abstract_declarator p * 'a in_t =
	(
		let rec loop
			(item: direct_abstract_declarator opt)
			(xs: [`cons of ranged_position * FirstSet.firstset_of_direct_abstract_declarator * 'a in_t] lazy_t)
			: direct_abstract_declarator p * 'a in_t =
		(
			let result, xs =
				begin match xs with
				| lazy (`cons (lb_p, (`l_bracket as lb_e), xs)) ->
					let l_bracket = lb_p, lb_e in
					let tql, xs = parse_type_qualifier_list_option error typedefs xs in
					let ass, xs = parse_assignment_expression_option error typedefs xs in
					let r_bracket, xs = parse_r_bracket_or_error error xs in
					let ps, () = item ^& l_bracket &^ tql &^ ass &^ r_bracket in
					(ps, `array (item, l_bracket, tql, ass, r_bracket)), xs
				| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
					let l_paren = lp_p, lp_e in
					let handle_function_type xs = (
						let params, xs = parse_parameter_type_list error typedefs xs in
						let r_paren, xs = parse_r_paren_or_error error xs in
						let ps, () = item ^& l_paren & params &^ r_paren in
						(ps, `function_type (item, l_paren, `some params, r_paren)), xs
					) in
					begin match xs with
					| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' as it), xr)) ->
						let xs = lazy (`cons (a, it, xr)) in
						handle_function_type xs
					| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
						let xs = lazy (`cons (a, it, xr)) in
						handle_function_type xs
					| _ ->
						let r_paren, xs =
							begin match xs with
							| lazy (`cons (id_p, `ident _, xs)) ->
								error id_p "old style function type could not be used in direct-abstract-declarator.";
								parse_r_paren_or_error error xs
							| _ ->
								parse_r_paren_or_error error xs
							end
						in
						let ps, () = item ^& l_paren &^ r_paren in
						(ps, `function_type (item, l_paren, `none, r_paren)), xs
					end
				end
			in
			begin match xs with
			| lazy (`cons (a, (#FirstSet.firstset_of_direct_abstract_declarator as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				loop (`some result) xs
			| _ ->
				result, xs
			end
		) in
		begin match xs with
		| lazy (`cons (_, `l_paren, lazy (`cons (_, #FirstSet.firstset_of_type_specifier', _)))) ->
			loop `none xs
		| lazy (`cons (_, `l_paren, lazy (`cons (_, `ident s, _))))
			when StringSet.mem s typedefs ->
			loop `none xs
		| lazy (`cons (lp_p, (`l_paren as lp_e), xs)) ->
			let l_paren = lp_p, lp_e in
			let attrs, xs = parse_attribute_list_option error typedefs xs in
			let d, xs = parse_abstract_declarator_or_error error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let ps, () = l_paren &^ d &^ r_paren in
			let result = (ps, `paren (l_paren, attrs, d, r_paren)) in
			begin match xs with
			| lazy (`cons (a, (#FirstSet.firstset_of_direct_abstract_declarator as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				loop (`some result) xs
			| _ ->
				result, xs
			end
		| lazy (`cons (_, `l_bracket, _)) ->
			loop `none xs
		end
	) and parse_initializer
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_initializer * 'a in_t] lazy_t)
		: initializer_t p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_assignment_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let (expr_p, expr_e), xs = parse_assignment_expression error typedefs xs in
			(expr_p, `expression expr_e), xs
		| lazy (`cons (lc_p, (`l_curly as lc_e), xs)) ->
			let l_curly = lc_p, lc_e in
			let ilist, xs = parse_initializer_list_or_error error typedefs xs in
			let r_curly, xs = parse_r_curly_or_error error xs in
			let ps, () = l_curly &^ ilist &^ r_curly in
			(ps, `list (l_curly, ilist, r_curly)), xs
		end
	) and parse_initializer_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: initializer_t e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_initializer as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_initializer error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "initializer was expected.";
			`error, xs
		end
	) and parse_initializer_list_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: initializer_list e * 'a in_t =
	(
		let rec loop rs xs = (
			begin match xs with
			| lazy (`cons (comma_p, (`comma as comma_e), xs)) ->
				let comma = comma_p, comma_e in
				let designation, xs = parse_designation_option error typedefs xs in
				let init, xs = parse_initializer_or_error error typedefs xs in
				let ps, () = rs & comma &^ designation &^ init in
				loop (ps, `cons (rs, comma, designation, init)) xs
			| _ ->
				`some rs, xs
			end
		) in
		let designation, xs = parse_designation_option error typedefs xs in
		begin match designation with
		| `some d_body ->
			let init, xs = parse_initializer_or_error error typedefs xs in
			let ps, () = d_body &^ init in
			loop (ps, `nil (designation, init)) xs
		| `none ->
			begin match parse_initializer_or_error error typedefs xs with
			| `some i_body as init, xs ->
				loop (fst i_body, `nil (`none, init)) xs
			| `error, _ as result ->
				(* an error has been already reported by parse_initializer_or_error *)
				result
			end
		end
	) and parse_designator
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_designator * 'a in_t] lazy_t)
		: designator p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (lb_p, (`l_bracket as lb_e), xs)) ->
			let l_bracket = lb_p, lb_e in
			let expr, xs = parse_constant_expression_or_error error typedefs xs in
			let r_bracket, xs = parse_r_bracket_or_error error xs in
			let ps, () = l_bracket &^ expr &^ r_bracket in
			(ps, `index (l_bracket, expr, r_bracket)), xs
		| lazy (`cons (pe_p, (`period as pe_e), xs)) ->
			let period = pe_p, pe_e in
			let id, xs = parse_identifier_or_error error xs in
			let ps, () = period &^ id in
			(ps, `element (period, id)), xs
		end
	) and parse_designator_list
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_designator * 'a in_t] lazy_t)
		: designator_list p * 'a in_t =
	(
		let rec loop (rs: designator_list p) xs = (
			begin match xs with
			| lazy (`cons (a, (#FirstSet.firstset_of_designator as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				let second, xs = parse_designator error typedefs xs in
				let ps, () = rs & second in
				loop (ps, `cons (rs, second)) xs
			| _ ->
				rs, xs
			end
		) in
		let (first_p, first_e), xs = parse_designator error typedefs xs in
		loop (first_p, `nil first_e) xs
	) and parse_designation_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: designation opt * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_designator as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let dlist, xs = parse_designator_list error typedefs xs in
			begin match xs with
			| lazy (`cons (ass_p, (`assign as ass_e), xs)) ->
				let ass = ass_p, ass_e in
				let ps, () = dlist & ass in
				`some (ps, (dlist, `some ass)), xs
			| _ ->
				error (LazyList.hd_a xs) "\"=\" was expected.";
				`some (fst dlist, (dlist, `error)), xs
			end
		| _ ->
			`none, xs
		end
	)
	(* A.2.3 Statements *)
	and parse_statement
		?(semicolon_need: bool = true) (* for macro *)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_statement * 'a in_t] lazy_t)
		: statement p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.asm as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let (asm_p, asm_e), xs = parse_inline_assembler ~semicolon_need error typedefs xs in
			(asm_p, `asm asm_e), xs
		| lazy (`cons (id_p, (#FirstSet.identifier as id_e),
			lazy (`cons (colon_p, (`colon as colon_e), xs)))) ->
			let id = id_p, id_e in
			let colon = colon_p, colon_e in
			let stmt, xs = parse_statement_or_error ~semicolon_need error typedefs xs in
			let ps, () = id & colon &^ stmt in
			(ps, `label (id, colon, stmt)), xs
		| lazy (`cons (case_p, (`CASE as case_e), xs)) ->
			let case = case_p, case_e in
			let expr, xs = parse_constant_expression_or_error error typedefs xs in
			let colon, xs = parse_colon_or_error error xs in
			let stmt, xs = parse_statement_or_error ~semicolon_need error typedefs xs in
			let ps, () = case &^ expr &^ colon &^ stmt in
			(ps, `case (case, expr, colon, stmt)), xs
		| lazy (`cons (default_p, (`DEFAULT as default_e), xs)) ->
			let default = default_p, default_e in
			let colon, xs = parse_colon_or_error error xs in
			let stmt, xs = parse_statement_or_error ~semicolon_need error typedefs xs in
			let ps, () = default &^ colon &^ stmt in
			(ps, `default (default, colon, stmt)), xs
		| lazy (`cons (a, (`l_curly as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let (block_p, block_e), xs = parse_compound_statement error typedefs xs in
			(block_p, `compound block_e), xs
		| lazy (`cons (sc_p, (`semicolon as sc_e), xs)) ->
			let semicolon = sc_p, sc_e in
			(sc_p, `expression (`none, `some semicolon)), xs
		| lazy (`cons (a, (#FirstSet.firstset_of_expression as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let expr, xs = parse_expression error typedefs xs in
			let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
			let ps, () = expr &^ semicolon in
			(ps, `expression (`some expr, semicolon)), xs
		| lazy (`cons (b_p, (`__builtin_va_start as b_e), xs)) ->
			let builtin = b_p, b_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let arg, xs = parse_assignment_expression_or_error error typedefs xs in
			begin match xs with
			| lazy (`cons (cm_p, (`comma as cm_e), xs)) ->
				let comma = cm_p, cm_e in
				let format, xs = parse_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
				let ps, () = builtin & comma &^ format &^ r_paren &^ semicolon in
				(ps, `__builtin_va_start (builtin, l_paren, arg, `some comma, format, r_paren, semicolon)), xs
			| _ ->
				error (LazyList.hd_a xs) "arguments mismatch for __builtin_va_start(va_list, format).";
				let r_paren, xs = parse_r_paren_or_error error xs in
				let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
				let ps, () = builtin &^ l_paren &^ arg &^ r_paren &^ semicolon in
				(ps, `__builtin_va_start (builtin, l_paren, arg, `error, `error, r_paren, semicolon)), xs
			end
		| lazy (`cons (b_p, (`__builtin_va_end as b_e), xs)) ->
			let builtin = b_p, b_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let arg, xs = parse_assignment_expression_or_error error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
			let ps, () = builtin &^ l_paren &^ arg &^ r_paren &^ semicolon in
			(ps, `__builtin_va_end (builtin, l_paren, arg, r_paren, semicolon)), xs
		| lazy (`cons (b_p, (`__builtin_va_copy as b_e), xs)) ->
			let builtin = b_p, b_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let dest, xs = parse_assignment_expression_or_error error typedefs xs in
			begin match xs with
			| lazy (`cons (cm_p, (`comma as cm_e), xs)) ->
				let comma = cm_p, cm_e in
				let source, xs = parse_expression_or_error error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
				let ps, () = builtin & comma &^ source &^ r_paren &^ semicolon in
				(ps, `__builtin_va_copy (builtin, l_paren, dest, `some comma, source, r_paren, semicolon)), xs
			| _ ->
				error (LazyList.hd_a xs) "arguments mismatch for __builtin_va_copy(va_list, va_list).";
				let r_paren, xs = parse_r_paren_or_error error xs in
				let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
				let ps, () = builtin &^ l_paren &^ dest &^ r_paren &^ semicolon in
				(ps, `__builtin_va_copy (builtin, l_paren, dest, `error, `error, r_paren, semicolon)), xs
			end
		| lazy (`cons (if_p, (`IF as if_e), xs)) ->
			let ifc = if_p, if_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let cond, xs = parse_expression_or_error error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let true_case, xs = parse_statement_or_error error typedefs xs in
			begin match xs with
			| lazy (`cons (el_p, (`ELSE as el_e), xs)) ->
				let elsec = el_p, el_e in
				let false_case, xs = parse_statement_or_error ~semicolon_need error typedefs xs in
				let ps, () = ifc & elsec &^ false_case in
				(ps, `if_then_else (ifc, l_paren, cond, r_paren, true_case, elsec, false_case)), xs
			| _ ->
				let ps, () = ifc &^ l_paren &^ cond &^ r_paren &^ true_case in
				(ps, `if_then (ifc, l_paren, cond, r_paren, true_case)), xs
			end
		| lazy (`cons (switch_p, (`SWITCH as switch_e), xs)) ->
			let switch = switch_p, switch_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let cond, xs = parse_expression_or_error error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let stmt, xs = parse_statement_or_error ~semicolon_need error typedefs xs in
			let ps, () = switch &^ l_paren &^ cond &^ r_paren &^ stmt in
			(ps, `switch (switch, l_paren, cond, r_paren, stmt)), xs
		| lazy (`cons (while_p, (`WHILE as while_e), xs)) ->
			let while_t = while_p, while_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let cond, xs = parse_expression_or_error error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let stmt, xs = parse_statement_or_error ~semicolon_need error typedefs xs in
			let ps, () = while_t &^ l_paren &^ cond &^ r_paren &^ stmt in
			(ps, `while_loop (while_t, l_paren, cond, r_paren, stmt)), xs
		| lazy (`cons (do_p, (`DO as do_e), xs)) ->
			let do_t = do_p, do_e in
			let stmt, xs = parse_statement_or_error error typedefs xs in
			let while_t, xs = parse_while_or_error error xs in
			let l_paren, xs = parse_l_paren_or_error error xs in
			let cond, xs = parse_expression_or_error error typedefs xs in
			let r_paren, xs = parse_r_paren_or_error error xs in
			let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
			let ps, () = do_t &^ stmt &^ while_t &^ l_paren &^ cond &^ r_paren &^ semicolon in
			(ps, `do_loop (do_t, stmt, while_t, l_paren, cond, r_paren, semicolon)), xs
		| lazy (`cons (for_p, (`FOR as for_e), xs)) ->
			let handle_with_declaration for_t l_paren xs = (
				let decl, typedefs, xs = parse_declaration error typedefs xs in
				let cond_expr, xs = parse_expression_option error typedefs xs in
				let semicolon2, xs = parse_semicolon_or_error error xs in
				let next_expr, xs = parse_expression_option error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let stmt, xs = parse_statement_or_error ~semicolon_need error typedefs xs in
				let ps, () = for_t & decl &^ cond_expr &^ semicolon2 &^ next_expr &^ r_paren &^ stmt in
				(ps, `for_with_declaration (for_t, l_paren, decl, cond_expr, semicolon2, next_expr, r_paren, stmt)), xs
			) in
			let for_t = for_p, for_e in
			let l_paren, xs = parse_l_paren_or_error error xs in
			begin match xs with
			| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				handle_with_declaration for_t l_paren xs
			| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
				let xs = lazy (`cons (a, it, xr)) in
				handle_with_declaration for_t l_paren xs
			| _ ->
				let init_expr, xs = parse_expression_option error typedefs xs in
				let semicolon1, xs = parse_semicolon_or_error error xs in
				let cond_expr, xs = parse_expression_option error typedefs xs in
				let semicolon2, xs = parse_semicolon_or_error error xs in
				let next_expr, xs = parse_expression_option error typedefs xs in
				let r_paren, xs = parse_r_paren_or_error error xs in
				let stmt, xs = parse_statement_or_error ~semicolon_need error typedefs xs in
				let ps, () = for_t &^ init_expr &^ semicolon1 &^ cond_expr &^ semicolon2 &^ next_expr &^ r_paren &^ stmt in
				(ps, `for_loop (for_t, l_paren, init_expr, semicolon1, cond_expr, semicolon2, next_expr, r_paren, stmt)), xs
			end
		| lazy (`cons (goto_p, (`GOTO as goto_e), xs)) ->
			let goto = goto_p, goto_e in
			let id, xs = parse_identifier_or_error error xs in
			let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
			let ps, () = goto &^ id &^ semicolon in
			(ps, `goto (goto, id, semicolon)), xs
		| lazy (`cons (continue_p, (`CONTINUE as continue_e), xs)) ->
			let continue = continue_p, continue_e in
			let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
			let ps, () = continue &^ semicolon in
			(ps, `continue (continue, semicolon)), xs
		| lazy (`cons (break_p, (`BREAK as break_e), xs)) ->
			let break = break_p, break_e in
			let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
			let ps, () = break &^ semicolon in
			(ps, `break (break, semicolon)), xs
		| lazy (`cons (ret_p, (`RETURN as ret_e), xs)) ->
			let return = ret_p, ret_e in
			let expr, xs = parse_expression_option error typedefs xs in
			let semicolon, xs = parse_semicolon_or_error ~semicolon_need error xs in
			let ps, () = return &^ expr &^ semicolon in
			(ps, `return (return, expr, semicolon)), xs
		end
	) and parse_statement_or_error
		?(semicolon_need: bool = true) (* for macro *)
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: statement e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_statement as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_statement ~semicolon_need error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "statement was expected.";
			`error, xs
		end
	) and parse_compound_statement
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [`l_curly] * 'a in_t] lazy_t)
		: compound_statement p * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (lc_p, (`l_curly as lc_e), xs)) ->
			let l_curly = lc_p, lc_e in
			let stmts, _, xs = parse_block_item_list_option error typedefs xs in
			let r_curly, xs = parse_r_curly_or_error error xs in
			let ps, () = l_curly &^ stmts &^ r_curly in
			(ps, (l_curly, stmts, r_curly)), xs
		end
	) and parse_compound_statement_or_error
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: compound_statement e * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (`l_curly as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let r, xs = parse_compound_statement error typedefs xs in
			`some r, xs
		| _ ->
			error (LazyList.hd_a xs) "compound-statement was expected.";
			`error, xs
		end
	) and parse_block_item_list_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: block_item_list opt * typedef_set * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_block_item as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let rec loop rs typedefs xs = (
				begin match xs with
				| lazy (`cons (a, (#FirstSet.firstset_of_block_item as it), xr)) ->
					let xs = lazy (`cons (a, it, xr)) in
					let second, typedefs, xs = parse_block_item error typedefs xs in
					let ps, () = rs & second in
					loop (ps, (`cons (rs, second))) typedefs xs
				| _ ->
					`some rs, typedefs, xs
				end
			) in
			let (first_p, first_e), typedefs, xs = parse_block_item error typedefs xs in
			loop (first_p, (`nil first_e)) typedefs xs
		| _ ->
			`none, typedefs, xs
		end
	) and parse_block_item
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * FirstSet.firstset_of_block_item * 'a in_t] lazy_t)
		: block_item p * typedef_set * 'a in_t =
	(
		let handle_declaration xs = (
			let (ps, decl), typedefs, xs = parse_declaration error typedefs xs in
			(ps, (`declaration decl)), typedefs, xs
		) in
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_declaration xs
		| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
			let xs = lazy (`cons (a, it, xr)) in
			handle_declaration xs
		| lazy (`cons (a, (#FirstSet.firstset_of_statement as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let (ps, stmt), xs = parse_statement error typedefs xs in
			(ps, (`statement stmt)), typedefs, xs
		end
	);;
	
	(* A.2.4 External definitions *)
	let rec parse_translation_unit
		(error: ranged_position -> string -> unit)
		(xs: 'a in_t)
		: translation_unit * typedef_set * 'a nil =
	(
		let rec loop rs typedefs xs = (
			let handle_external_declaration xs = (
				let def, typedefs, xs = parse_external_declaration error typedefs xs in
				loop (`cons (rs, def)) typedefs xs
			) in
			begin match xs with
			| lazy (`nil a) ->
				rs, typedefs, (lazy (`nil a))
			| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' | `sharp_PRAGMA as it), xr)) ->
				let xs = lazy (`cons (a, it, xr)) in
				handle_external_declaration xs
			| lazy (`cons (a, (`ident name as it), xr)) when StringSet.mem name typedefs ->
				let xs = lazy (`cons (a, it, xr)) in
				handle_external_declaration xs
			| lazy (`cons (_, `semicolon, xs)) ->
				loop rs typedefs xs (* ignore repeated ";" *)
			| lazy (`cons (_, _, xr)) ->
				error (LazyList.hd_a xs) "syntax error.";
				let _, xs = skip_until_semicolon xr in (* skip to ";" *)
				loop rs typedefs xs
			end
		) in
		let typedefs = StringSet.empty in
		loop `nil typedefs xs
	) and parse_external_declaration
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: [`cons of ranged_position * [FirstSet.firstset_of_declaration_specifiers' | FirstSet.typedef_name | `sharp_PRAGMA] * 'a in_t] lazy_t)
		: external_declaration p * typedef_set * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (`sharp_PRAGMA as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let (pragma_p, pragma_e), xs = parse_pragma error typedefs xs in
			(pragma_p, `pragma pragma_e), typedefs, xs
		| lazy (`cons (a, (#FirstSet.firstset_of_declaration_specifiers' | #FirstSet.typedef_name as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let rec is_function_declarator (declarator: declarator): bool = (
				let rec is_function_dd (direct_declarator: direct_declarator): bool = (
					begin match direct_declarator with
					| `ident _ ->
						false
					| `paren (_, _, d, _) ->
						begin match d with
						| `some (_, d) -> is_function_declarator d
						| `error -> false
						end
					| `array ((_, dd), _, _, _, _)
					| `static_array1 ((_, dd), _, _, _, _, _) | `static_array2 ((_, dd), _, _, _, _, _)
					| `dynamic_array ((_, dd), _, _, _, _) ->
						is_function_dd dd
					| `function_type ((_, dd), _, _, _) | `old_function_type ((_, dd), _, _, _) ->
						is_function_ident_dd dd
					end
				) and is_function_ident_dd (direct_declarator: direct_declarator): bool = (
					begin match direct_declarator with
					| `ident _ ->
						true
					| `paren (_, _, `some (_, (`none, `some (_, dd), _)), _) ->
						is_function_ident_dd dd
					| _ ->
						is_function_dd direct_declarator
					end
				) in
				let _, direct_declarator, _ = declarator in
				begin match direct_declarator with
				| `some (_, direct_declarator) ->
					is_function_dd direct_declarator
				| `error ->
					false
				end
			) in
			let specifiers, xs = parse_declaration_specifiers error typedefs xs in
			let declarators, xs = parse_init_declarator_list_option error typedefs xs in
			begin match declarators with
			| `some (decl_p, `nil (`no_init declarator)) (* single declarator with ... *)
				when (
					match xs with (* ... some following body *)
					| lazy (`cons (_, `l_curly, _))
					| lazy (`cons (_, #FirstSet.asm, _)) ->
						true
					| lazy (`cons (_, (#FirstSet.firstset_of_type_specifier' | #FirstSet.typedef_name), _)) (* declaration-list *)
						when is_function_declarator declarator ->
						true
					| _ ->
						false) ->
				let declarator = decl_p, declarator in
				begin match xs with
				| lazy (`cons (asm_p, (#FirstSet.asm as asm_e), xs)) ->
					let asm = asm_p, asm_e in
					let l_paren, xs = parse_l_paren_or_error error xs in
					let alias_name, xs = parse_chars_literal_or_error error typedefs xs in
					let r_paren, xs = parse_r_paren_or_error error xs in
					let attr, xs = parse_attribute_list_option error typedefs xs in
					let semicolon, xs = parse_semicolon_or_error error xs in
					let ps, () = specifiers & asm &^ l_paren &^ alias_name &^ r_paren &^ attr &^ semicolon in
					let result = ps, `aliased_declaration (
						specifiers,
						`some declarator,
						asm,
						l_paren,
						alias_name,
						r_paren,
						attr,
						semicolon)
					in
					result, typedefs, xs
				| _ ->
					let arg_bodies, typedefs, xs = parse_declaration_list_option error typedefs xs in
					let func_body, xs = parse_compound_statement_or_error error typedefs xs in
					let ps, () = specifiers & declarator &^ arg_bodies &^ func_body in
					let result = ps, `function_definition (
						specifiers,
						`some declarator,
						arg_bodies,
						func_body)
					in
					result, typedefs, xs
				end
			| _ ->
				let semicolon, xs =
					begin match parse_semicolon_or_error error xs with
					| `some _, _ as r -> r
					| `error, xs -> skip_until_semicolon xs (* error recovery *)
					end
				in
				let ps, () = specifiers &^ declarators &^ semicolon in
				let result = ps, `declaration (specifiers, declarators, semicolon) in
				let typedefs =
					if has_typedef specifiers then add_declarators_to_typedef_set declarators typedefs else
					typedefs
				in
				result, typedefs, xs
			end
		end
	) and parse_declaration_list_option
		(error: ranged_position -> string -> unit)
		(typedefs: typedef_set)
		(xs: 'a in_t)
		: declaration_list opt * typedef_set * 'a in_t =
	(
		begin match xs with
		| lazy (`cons (a, (#FirstSet.firstset_of_type_specifier' | #FirstSet.typedef_name as it), xr)) ->
			let xs = lazy (`cons (a, it, xr)) in
			let rec loop rs typedefs xs = (
				begin match xs with
				| lazy (`cons (a, (#FirstSet.firstset_of_type_specifier' | #FirstSet.typedef_name as it), xr)) ->
					let xs = lazy (`cons (a, it, xr)) in
					let second, typedefs, xs = parse_declaration error typedefs xs in
					let ps, () = rs & second in
					loop (ps, (`cons (rs, second))) typedefs xs
				| _ ->
					`some rs, typedefs, xs
				end
			) in
			let (first_p, first_e), typedefs, xs = parse_declaration error typedefs xs in
			loop (first_p, (`nil first_e)) typedefs xs
		| _ ->
			`none, typedefs, xs
		end
	);;
	
end;;
