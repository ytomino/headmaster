open C_semantics;;
open Position;;
open Value;;

module Machine
	(Literals: LiteralsType)
	(Semantics: SemanticsType (Literals).S) =
struct
	open Literals;;
	open Semantics;;
	
	let rec sizeof_predefined_type
		(e: [< predefined_type])
		(predefined_types: predefined_types): int =
	(
		snd (List.find (fun (x, _) -> x = (e :> predefined_type)) (fst predefined_types))
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
		| `pointer _ ->
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
								let total = if bits > 0 then total + 1 else total in
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
		| `pointer _ ->
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
		| `array (Some n, base) ->
			begin match alignof (base :> all_type) predefined_types with
			| Some item_align ->
				Some (item_align * Integer.to_int n)
			| None ->
				None
			end
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
	
	(* for analyzer *)
	
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
		(error: ranged_position -> string -> unit)
		(predefined_types: predefined_types)
		(ps: ranged_position)
		(xs: struct_item list)
		: struct_item list =
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
							x :: loop xr total_bytes bits
						) else (
							let x = item_name, item_type, Some (total_bytes * 8 + bits, item_bits, true), item_attrs in
							let bits = bits + item_bits in
							x :: loop xr total_bytes bits
						)
					| None ->
						error ps (item_name ^ " is not able to be aligned.");
						[]
					end
				| item_name, item_type, None, item_attrs ->
					begin match sizeof item_type predefined_types with
					| Some item_size ->
						begin match alignof item_type predefined_types with
						| Some item_align ->
							let total_bytes = if bits > 0 then total_bytes + 1 else total_bytes in
							let total_bytes = (total_bytes + item_align - 1) / item_align * item_align in
							let x = item_name, item_type, Some (total_bytes * 8, item_size * 8, false), item_attrs in
							let total_bytes = total_bytes + item_size in
							x :: loop xr total_bytes 0
						| None ->
							error ps (item_name ^ " is not able to be aligned.");
							[]
						end
					| None ->
						error ps (item_name ^ " is not able to be sized.");
						[]
					end
				end
			| [] ->
				[]
			end
		) in
		loop xs 0 0
	);;
	
end;;
