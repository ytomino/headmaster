open C_literals;;
open C_syntax;;

module Traversing
	(Literals: LiteralsType)
	(Syntax: SyntaxType (Literals).S) =
struct
	open Syntax;;
	
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
	
	let rec fold_idl (f: 'a -> init_declarator p -> 'a) (a: 'a) (xs: init_declarator_list p): 'a = (
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
	
	let rec fold_sdnl (f: 'a -> struct_declaration p -> 'a) (a: 'a) (xs: struct_declaration_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a (fst xs, x)
		| `cons (xr, x) ->
			f (fold_sdnl f a xr) x
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
	
	let rec fold_il (f: 'a -> designation opt * initializer_t pe -> 'a) (a: 'a) (xs: initializer_list p): 'a = (
		begin match snd xs with
		| `nil x ->
			f a x
		| `cons (xr, _, x1, x2) ->
			f (fold_il f a xr) (x1, x2)
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
	
	let fold_tu
		(f: 'b -> external_declaration p -> 'b)
		(in_f: string -> 'a -> 'b)
		(out_f: string -> 'b -> 'a)
		(a: 'a)
		(xs: translation_unit)
		: 'a =
	(
		let rec make_list xs rs = (
			begin match xs with
			| `nil ->
				rs
			| `cons (xr, x) ->
				make_list xr (x :: rs)
			end
		) in
		let rec folding f in_f out_f v (xs: external_declaration p list) = (
			begin match xs with
			| x :: xr ->
				let ((current_filename, _, _, _), _), _ = x in
				let v = in_f current_filename v in
				folding_in current_filename f in_f out_f (f v x) xr
			| [] ->
				v
			end
		) and folding_in previous_filename f in_f out_f v (xs: external_declaration p list) = (
			begin match xs with
			| x :: xr ->
				let ((current_filename, _, _, _), _), _ = x in
				if current_filename = previous_filename then (
					folding_in current_filename f in_f out_f (f v x) xr
				) else (
					let v = out_f previous_filename v in
					let v = in_f current_filename v in
					folding_in current_filename f in_f out_f (f v x) xr
				)
			| [] ->
				out_f previous_filename v
			end
		) in
		folding f in_f out_f a (make_list xs [])
	);;
	
end;;
