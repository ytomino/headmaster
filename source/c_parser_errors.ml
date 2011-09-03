open Known_errors;;

let known_struct_declarator_list_errors = make_set [
	"oaidl.h"];; (* mingw32 / anonymous struct without __extension__ *)

let is_known_struct_declarator_list_error = set_mem ~set:known_struct_declarator_list_errors;;
