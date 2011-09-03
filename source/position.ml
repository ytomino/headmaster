type position = (string * int * int * int);;
type ranged_position = position * position;;

let predefined_name = "<predefined>";;

let make_string_of_position (output: string -> unit) (p: position): unit = (
	let f, _, l, c = p in
	output f;
	output ":";
	output (string_of_int l);
	output ":";
	output (string_of_int c)
);;

let string_of_position (p: position): string = (
	let b = Buffer.create 128 in
	make_string_of_position (Buffer.add_string b) p;
	Buffer.contents b
);;

let output_position (f: out_channel) (p: position): unit = (
	make_string_of_position (output_string f) p
);;
