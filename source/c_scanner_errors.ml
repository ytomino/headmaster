open Known_errors;;

let known_hexadecimal_literal_errors = make_set [
	"mmsystem.h"];; (* mingw32 / 0xL *)

let is_known_hexadecimal_literal_error = set_mem ~set:known_hexadecimal_literal_errors;;
