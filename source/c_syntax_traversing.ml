open C_literals;;
open C_syntax;;

module type TraversingType = sig
	module Literals: LiteralsType;;
	module Syntax: SyntaxType
		with module Literals := Literals;;
	
	type 'a p = 'a Syntax.p;;
	type 'a e = 'a Syntax.e;;
	type 'a opt = 'a Syntax.opt;;
	
	(* make opt version of fold_xxx *)
	val opt: (('a -> 'b p -> 'a) -> 'a -> 'c p -> 'a) ->
		('a -> 'b p -> 'a) -> 'a -> 'c opt -> 'a;;
	
	(* poison_identifier_list *)
	val fold_pil: ('a -> Syntax.identifier p -> 'a) -> 'a -> Syntax.poison_identifier_list p -> 'a;;
	
	(* attribute_list *)
	val fold_al: ('a -> Syntax.attribute p -> 'a) -> 'a -> (Syntax.attribute_list p) -> 'a;;
	
	(* attribute_item_list *)
	val fold_ail: ('a -> Syntax.attribute_item p -> 'a) -> 'a -> Syntax.attribute_item_list p -> 'a;;
	
	(* ia_argument_list *)
	val fold_iaal: ('a -> Syntax.ia_argument p -> 'a) -> 'a -> Syntax.ia_argument_list p -> 'a;;
	
	(* ia_register_list *)
	val fold_iarl: ('a -> [`chars_literal of string] p -> 'a) -> 'a -> Syntax.ia_register_list p -> 'a;;
	val fold_right_iarl: ([`chars_literal of string] p -> 'a -> 'a) -> Syntax.ia_register_list p -> 'a -> 'a;;
	
	(* argument_expression_list *)
	val fold_ael: ('a -> Syntax.assignment_expression p -> 'a) -> 'a -> Syntax.argument_expression_list p -> 'a;;
	val fold_right_ael: (Syntax.assignment_expression p -> 'a -> 'a) -> Syntax.argument_expression_list p -> 'a -> 'a;;
	
	(* init_declarator_list *)
	val fold_idrl: ('a -> Syntax.init_declarator p -> 'a) -> 'a -> Syntax.init_declarator_list p -> 'a;;
	
	(* struct_declaration_list *)
	val fold_sdnl: ('a -> Syntax.struct_declaration p -> 'a) -> 'a -> (Syntax.struct_declaration_list p) -> 'a;;
	
	(* specifier_qualifier_list *)
	val fold_sql: ('a -> Syntax.type_specifier p -> 'a) -> ('a -> Syntax.type_qualifier p -> 'a) ->
		'a -> Syntax.specifier_qualifier_list p -> 'a;;
	
	(* struct_declarator_list *)
	val fold_sdrl: ('a -> Syntax.struct_declarator p -> 'a) -> 'a -> (Syntax.struct_declarator_list p) -> 'a;;
	
	(* pointer *)
	val fold_p: ('a -> [`asterisk | `caret] p * Syntax.type_qualifier_list opt * Syntax.attribute_list opt -> 'a) ->
		'a -> Syntax.pointer p -> 'a;;
	
	(* enumerator_list *)
	val fold_el: ('a -> Syntax.enumerator p -> 'a) -> 'a -> Syntax.enumerator_list p -> 'a;;
	
	(* type_qualifier_list *)
	val fold_tql: ('a -> Syntax.type_qualifier p -> 'a) -> 'a -> Syntax.type_qualifier_list p -> 'a;;
	
	(* parameter_list *)
	val fold_pl: ('a -> Syntax.parameter_declaration p -> 'a) -> 'a -> Syntax.parameter_list p -> 'a;;
	
	(* identifier_list *)
	val fold_idl: ('a -> Syntax.identifier p -> 'a) -> 'a -> Syntax.identifier_list p -> 'a;;
	
	(* initializer_list *)
	val fold_il: ('a -> Syntax.designation opt * Syntax.initializer_t e -> 'a) -> 'a -> Syntax.initializer_list p -> 'a;;
	
	(* designator_list *)
	val fold_drl: ('a -> Syntax.designator p -> 'a) -> 'a -> Syntax.designator_list p -> 'a;;
	
	(* block_item_list *)
	val fold_bil: ('a -> Syntax.block_item p -> 'a) -> 'a -> Syntax.block_item_list p -> 'a;;
	val fold_right_bil: (Syntax.block_item p -> 'a -> 'a) -> Syntax.block_item_list p -> 'a -> 'a;;
	
	(* translation_unit *)
	val fold_tu: ('b -> Syntax.external_declaration p -> 'b) ->
		(string list -> string -> 'a -> 'b) -> (* in *)
		(string -> 'b -> 'a) -> (* out *)
		'a -> Syntax.translation_unit -> 'a;;
	
	(* declaration_list *)
	val fold_dnl: ('a -> Syntax.declaration p -> 'a) -> 'a -> Syntax.declaration_list p -> 'a;;
	
end;;

module Traversing
	(Literals: LiteralsType)
	(Syntax: SyntaxType
		with module Literals := Literals)
	: TraversingType
		with module Literals := Literals
		with module Syntax := Syntax =
struct
	open Syntax;;
	
	type 'a p = 'a Syntax.p;;
	type 'a e = 'a Syntax.e;;
	type 'a opt = 'a Syntax.opt;;
	
	let opt (fold: ('a -> 'b p -> 'a) -> 'a -> 'c p -> 'a)
		(f: ('a -> 'b p -> 'a)) (a: 'a) (xs: 'c opt): 'a =
	(
		begin match xs with
		| `some xs ->
			fold f a xs
		| `none ->
			a
		end
	);;
	
	let rec fold_pil (f: 'a -> identifier p -> 'a) (a: 'a) (xs: poison_identifier_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, x) ->
			f (fold_pil f a xr) x
		end
	);;
	
	let rec fold_al (f: 'a -> attribute p -> 'a) (a: 'a) (xs: attribute_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, x) ->
			f (fold_al f a xr) x
		end
	);;
	
	let rec fold_ail (f: 'a -> attribute_item p -> 'a) (a: 'a) (xs: attribute_item_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_ail f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_iaal (f: 'a -> ia_argument p -> 'a) (a: 'a) (xs: ia_argument_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_iaal f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_iarl (f: 'a -> [`chars_literal of string] p -> 'a) (a: 'a) (xs: ia_register_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_iarl f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_right_iarl (f: [`chars_literal of string] p -> 'a -> 'a) (xs: ia_register_list p) (a: 'a): 'a = (
		begin match snd xs with
		| `nil x ->
			f (fst xs, x) a
		| `cons (xr, _, x) ->
			let v =
				begin match x with
				| `some x ->
					f x a
				| `error ->
					a
				end
			in
			fold_right_iarl f xr v
		end
	);;
	
	let rec fold_ael (f: 'a -> assignment_expression p -> 'a) (a: 'a) (xs: argument_expression_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_ael f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_right_ael (f: assignment_expression p -> 'a -> 'a) (xs: argument_expression_list p) (a: 'a): 'a = (
		begin match snd xs with
		| `nil x ->
			f (fst xs, x) a
		| `cons (xr, _, x) ->
			let v =
				begin match x with
				| `some x ->
					f x a
				| `error ->
					a
				end
			in
			fold_right_ael f xr v
		end
	);;
	
	let rec fold_idrl (f: 'a -> init_declarator p -> 'a) (a: 'a) (xs: init_declarator_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_idrl f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_sdnl (f: 'a -> struct_declaration p -> 'a) (a: 'a) (xs: struct_declaration_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, x) ->
			f (fold_sdnl f a xr) x
		end
	);;
	
	let rec fold_sql (s_f: 'a -> type_specifier p -> 'a) (q_f: 'a -> type_qualifier p -> 'a)
		(a: 'a) (xs: specifier_qualifier_list p): 'a =
	(
		begin match snd xs with
		| `type_specifier (s, xr) ->
			let v = s_f a s in
			begin match xr with
			| `some xr ->
				fold_sql s_f q_f v xr
			| `none ->
				v
			end
		| `type_qualifier (q, xr) ->
			let v = q_f a q in
			begin match xr with
			| `some xr ->
				fold_sql s_f q_f v xr
			| `none ->
				v
			end
		end
	);;
	
	let rec fold_sdrl (f: 'a -> struct_declarator p -> 'a) (a: 'a) (xs: struct_declarator_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_sdrl f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_el (f: 'a -> enumerator p -> 'a) (a: 'a) (xs: enumerator_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_el f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_p (f: 'a -> [`asterisk | `caret] p * type_qualifier_list opt * attribute_list opt -> 'a) (a: 'a) (xs: pointer p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a x
		| `cons (mark, tql, xr) ->
			let v = f a (mark, tql, `none) in
			fold_p f v xr
		end
	);;
	
	let rec fold_tql (f: 'a -> type_qualifier p -> 'a) (a: 'a) (xs: type_qualifier_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, x) ->
			f (fold_tql f a xr) x
		end
	);;
	
	let rec fold_pl (f: 'a -> parameter_declaration p -> 'a) (a: 'a) (xs: parameter_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_pl f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_idl (f: 'a -> identifier p -> 'a) (a: 'a) (xs: identifier_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, _, x) ->
			let v = fold_idl f a xr in
			begin match x with
			| `some x ->
				f v x
			| `error ->
				v
			end
		end
	);;
	
	let rec fold_il (f: 'a -> designation opt * initializer_t e -> 'a) (a: 'a) (xs: initializer_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a x
		| `cons (xr, _, x1, x2) ->
			f (fold_il f a xr) (x1, x2)
		end
	);;
	
	let rec fold_drl (f: 'a -> designator p -> 'a) (a: 'a) (xs: designator_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, x) ->
			f (fold_drl f a xr) x
		end
	);;
	
	let rec fold_bil (f: 'a -> block_item p -> 'a) (a: 'a) (xs: block_item_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, x) ->
			f (fold_bil f a xr) x
		end
	);;
	
	let rec fold_right_bil (f: block_item p -> 'a -> 'a) (xs: block_item_list p) (a: 'a): 'a = (
		begin match snd xs with
		| `nil x ->
			f (fst xs, x) a
		| `cons (xr, x) ->
			fold_right_bil f xr (f x a)
		end
	);;
	
	let fold_tu
		(f: 'b -> external_declaration p -> 'b)
		(in_f: string list -> string -> 'a -> 'b)
		(out_f: string -> 'b -> 'a)
		(a: 'a)
		(xs: translation_unit)
		: 'a =
	(
		let rec make_list xs rs: external_declaration p list = (
			begin match xs with
			| `nil ->
				rs
			| `cons (xr, x) ->
				make_list xr (x :: rs)
			end
		) in
		let included (filename: string) (ys: external_declaration p list): string list = (
			let rec loop ys result = (
				begin match ys with
				| y :: yr ->
					let ((previous_filename, _, _, _), _), _ = y in
					if previous_filename = filename then result else
					let result =
						if List.mem previous_filename result then result else
						previous_filename :: result
					in
					loop yr result
				| [] ->
					[]
				end
			) in
			loop ys []
		) in
		let rec folding f in_f out_f v xs ys = (
			begin match xs with
			| x :: xr ->
				let ((current_filename, _, _, _), _), _ = x in
				let v = in_f [] current_filename v in
				folding_in current_filename f in_f out_f (f v x) xr (x :: ys)
			| [] ->
				v
			end
		) and folding_in previous_filename f in_f out_f v xs ys = (
			begin match xs with
			| x :: xr ->
				let ((current_filename, _, _, _), _), _ = x in
				let v =
					if current_filename = previous_filename then v else
					let included = included current_filename ys in
					in_f included current_filename (out_f previous_filename v)
				in
				folding_in current_filename f in_f out_f (f v x) xr (x :: ys)
			| [] ->
				out_f previous_filename v
			end
		) in
		folding f in_f out_f a (make_list xs []) []
	);;
	
	let rec fold_dnl (f: 'a -> declaration p -> 'a) (a: 'a) (xs: declaration_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, x) ->
			f (fold_dnl f a xr) x
		end
	);;
	
end;;
