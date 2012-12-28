(* special filename *)

let predefined_name = "<predefined>";;
let builtin_name = "<builtin>";;

let is_special_filename (filename: string): bool = (
	String.length filename > 0 && filename.[0] = '<'
);;
