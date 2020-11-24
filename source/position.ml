type position = (string * int * int * int);;
type ranged_position = position * position;;

let make_string_of_position (output: string -> unit) (filename_f: string -> string) (p: position): unit = (
	let f, _, l, c = p in
	output (filename_f f);
	output ":";
	output (string_of_int l);
	output ":";
	output (string_of_int c)
);;

let string_of_position (filename_f: string -> string) (p: position): string = (
	let b = Buffer.create 128 in
	make_string_of_position (Buffer.add_string b) filename_f p;
	Buffer.contents b
);;

let output_position (f: out_channel) (filename_f: string -> string) (p: position): unit = (
	make_string_of_position (output_string f) filename_f p
);;

module PositionOperators = struct
	
	let ( & ) (a: ranged_position * 'a) (b: ranged_position * 'b)
		: ranged_position * unit =
	(
		let (first, _), _ = a in
		let (_, last), _ = b in
		(first, last), ()
	);;
	
	let ( &^ ) (a: ranged_position * 'a) (b: [> `some of ranged_position * 'b])
		: ranged_position * unit =
	(
		let (first, _), _ = a in
		let last =
			begin match b with
			| `some b ->
				let ((_, last), _) = b in last
			| _ ->
				let ((_, last), _) = a in last
			end
		in
		(first, last), ()
	);;
	
	(* right to left *)
	let ( ^& ) (a: [> `some of ranged_position * 'a]) (b: ranged_position * 'b)
		: ranged_position * unit =
	(
		let first =
			begin match a with
			| `some a ->
				let ((first, _), _) = a in first
			| _ ->
				let ((first, _), _) = b in first
			end
		in
		let (_, last), _ = b in
		(first, last), ()
	);;
	
end;;
