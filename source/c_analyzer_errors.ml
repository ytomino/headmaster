open Known_errors;;
open Position;;

let known_uninterpretable_macros = make_setmap [
	"dirent.h", [
		"dirfd"; (* darwin9 / accessing element of untyped parameter *)
		"_GENERIC_DIRSIZ"]; (* freebsd7 / accessing element of untyped parameter *)
	"fenv.h", [
		"__get_mxcsr"; (* freebsd7 / accessing element of untyped parameter *)
		"__set_mxcsr"]; (* freebsd7 / accessing element of untyped parameter *)
	"gmp.h", [
		"mpf_sgn"; (* GMP / accessing element of untyped parameter *)
		"mpq_cmp_si"; (* GMP / accessing element of untyped parameter *)
		"mpq_cmp_ui"; (* GMP / accessing element of untyped parameter *)
		"mpq_denref"; (* GMP / accessing element of untyped parameter *)
		"mpq_numref"; (* GMP / accessing element of untyped parameter *)
		"mpq_sgn"; (* GMP / accessing element of untyped parameter *)
		"mpz_cmp_si"; (* GMP / accessing element of untyped parameter *)
		"mpz_cmp_ui"; (* GMP / accessing element of untyped parameter *)
		"mpz_even_p"; (* GMP / accessing element of untyped parameter *)
		"mpz_odd_p"; (* GMP / accessing element of untyped parameter *)
		"mpz_sgn"]; (* GMP / accessing element of untyped parameter *)
	"mpc.h", [
		"mpc_imagref"; (* MPC / accessing element of untyped parameter *)
		"mpc_realref"; (* MPC / accessing element of untyped parameter *)
		"mpfr_set_fr"]; (* MPFR / accessing element of untyped parameter *)
	"mpfr.h", [
		"mpfr_add_one_ulp"; (* MPFR / accessing element of untyped parameter *)
		"mpfr_custom_get_mantissa"; (* MPFR / accessing element of untyped parameter *)
		"mpfr_custom_get_significand"; (* MPFR / accessing element of untyped parameter *)
		"mpfr_init_set"; (* MPFR / accessing element of untyped parameter *)
		"mpfr_set"; (* MPFR / accessing element of untyped parameter *)
		"mpfr_sub_one_ulp"; (* MPFR / accessing element of untyped parameter *)
		"MPFR_SIGN"]; (* MPFR / accessing element of untyped parameter *)
	"objbase.h", [
		"LISet32"; (* mingw32 / accessing element of untyped parameter *)
		"ULISet32"]; (* mingw32 / accessing element of untyped parameter *)
	"oleauto.h", [
		"V_ARRAY"; (* mingw32 / accessing element of untyped parameter *)
		"V_ARRAYREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_BLOB"; (* mingw32 / accessing element of untyped parameter *)
		"V_BOOL"; (* mingw32 / accessing element of untyped parameter *)
		"V_BOOLREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_BSTR"; (* mingw32 / accessing element of untyped parameter *)
		"V_BSTRREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_BYREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_CLSID"; (* mingw32 / accessing element of untyped parameter *)
		"V_CY"; (* mingw32 / accessing element of untyped parameter *)
		"V_CYREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_DATE"; (* mingw32 / accessing element of untyped parameter *)
		"V_DATEREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_DECIMAL"; (* mingw32 / accessing element of untyped parameter *)
		"V_DECIMALREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_DISPATCH"; (* mingw32 / accessing element of untyped parameter *)
		"V_DISPATCHREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_ERROR"; (* mingw32 / accessing element of untyped parameter *)
		"V_ERRORREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_FILETIME"; (* mingw32 / accessing element of untyped parameter *)
		"V_FILETIMEREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_I1"; (* mingw32 / accessing element of untyped parameter *)
		"V_I2"; (* mingw32 / accessing element of untyped parameter *)
		"V_I2REF"; (* mingw32 / accessing element of untyped parameter *)
		"V_I4"; (* mingw32 / accessing element of untyped parameter *)
		"V_I4REF"; (* mingw32 / accessing element of untyped parameter *)
		"V_I8"; (* mingw32 / accessing element of untyped parameter *)
		"V_I8REF"; (* mingw32 / accessing element of untyped parameter *)
		"V_INT_PTR"; (* mingw32 / accessing element of untyped parameter *)
		"V_INT_PTRREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_ISARRAY"; (* mingw32 / accessing element of untyped parameter *)
		"V_ISBYREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_ISVECTOR"; (* mingw32 / accessing element of untyped parameter *)
		"V_LPSTR"; (* mingw32 / accessing element of untyped parameter *)
		"V_LPSTRREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_LPWSTR"; (* mingw32 / accessing element of untyped parameter *)
		"V_LPWSTRREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_NONE"; (* mingw32 / accessing element of untyped parameter *)
		"V_R4"; (* mingw32 / accessing element of untyped parameter *)
		"V_R4REF"; (* mingw32 / accessing element of untyped parameter *)
		"V_R8"; (* mingw32 / accessing element of untyped parameter *)
		"V_R8REF"; (* mingw32 / accessing element of untyped parameter *)
		"V_UI1"; (* mingw32 / accessing element of untyped parameter *)
		"V_UI1REF"; (* mingw32 / accessing element of untyped parameter *)
		"V_UI2"; (* mingw32 / accessing element of untyped parameter *)
		"V_UI4"; (* mingw32 / accessing element of untyped parameter *)
		"V_UI4REF"; (* mingw32 / accessing element of untyped parameter *)
		"V_UI8"; (* mingw32 / accessing element of untyped parameter *)
		"V_UI8REF"; (* mingw32 / accessing element of untyped parameter *)
		"V_UINT_PTR"; (* mingw32 / accessing element of untyped parameter *)
		"V_UINT_PTRREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_UNION"; (* mingw32 / accessing element of untyped parameter *)
		"V_UNKNOWN"; (* mingw32 / accessing element of untyped parameter *)
		"V_UNKNOWNREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_UUID"; (* mingw32 / accessing element of untyped parameter *)
		"V_VARIANTREF"; (* mingw32 / accessing element of untyped parameter *)
		"V_VT"]; (* mingw32 / accessing element of untyped parameter *)
	"rpcndr.h", [
		"boolean_array_from_ndr"; (* mingw32 / accessing element of untyped parameter *)
		"boolean_from_ndr"; (* mingw32 / accessing element of untyped parameter *)
		"byte_array_from_ndr"; (* mingw32 / accessing element of untyped parameter *)
		"byte_from_ndr"; (* mingw32 / accessing element of untyped parameter *)
		"NDRSContextValue"; (* mingw32 / accessing element of untyped parameter *)
		"small_array_from_ndr"; (* mingw32 / accessing element of untyped parameter *)
		"small_from_ndr"]; (* mingw32 / accessing element of untyped parameter *)
	"select.h", [
		"FD_CLR"; (* darwin9 / accessing element of untyped parameter *)
		"FD_ISSET"; (* freebsd7 / accessing element of untyped parameter *)
		"FD_SET"]; (* darwin9 / accessing element of untyped parameter *)
	"stdio.h", [
		"clearerr_unlocked"; (* darwin9 / accessing element of untyped parameter *)
		"ferror_unlocked"; (* darwin9 / accessing element of untyped parameter *)
		"feof_unlocked";  (* darwin9 / accessing element of untyped parameter *)
		"fileno_unlocked"; (* darwin9 / accessing element of untyped parameter *)
		"__sclearerr"; (* darwin9 / accessing element of untyped parameter *)
		"__sfeof"; (* darwin9 / accessing element of untyped parameter *)
		"__sferror"; (* darwin9 / accessing element of untyped parameter *)
		"__sfileno"; (* darwin9 / accessing element of untyped parameter *)
		"__sgetc"]; (* darwin9 / accessing element of untyped parameter *)
	"_structs.h", [
		"__DARWIN_FD_CLR"; (* darwin9 / accessing element of untyped parameter *)
		"__DARWIN_FD_SET"]; (* darwin9 / accessing element of untyped parameter *)
	"time.h", [
		"FD_CLR"; (* darwin9 / accessing element of untyped parameter *)
		"FD_SET"; (* darwin9 / accessing element of untyped parameter *)
		"timeradd"; (* darwin9 / accessing element of untyped parameter *)
		"timerclear"; (* darwin9 / accessing element of untyped parameter *)
		"timerisset"; (* darwin9 / accessing element of untyped parameter *)
		"timersub"; (* darwin9 / accessing element of untyped parameter *)
		"TIMESPEC_TO_TIMEVAL"; (* darwin9 / accessing element of untyped parameter *)
		"TIMEVAL_TO_TIMESPEC"]; (* darwin9 / accessing element of untyped parameter *)
	"timespec.h", [
		"TIMEVAL_TO_TIMESPEC"; (* freebsd7 / accessing element of untyped parameter *)
		"TIMESPEC_TO_TIMEVAL"]; (* freebsd7 / accessing element of untyped parameter *)
	"tree.h", [
		"XML_GET_CONTENT"]; (* libxml2 / accessing element of untyped parameter *)
	"types.h", [
		"FD_CLR"; (* darwin9 / accessing element of untyped parameter *)
		"FD_SET"]; (* darwin9 / accessing element of untyped parameter *)
	"winbase.h", [
		"HasOverlappedIoCompleted"]; (* mingw32 / accessing element of untyped parameter *)
	"winsock2.h", [
		"timerclear"; (* mingw32 / accessing element of untyped parameter *)
		"timerisset"]; (* mingw32 / accessing element of untyped parameter *)
	"winuser.h", [
		"POINTSTOPOINT"; (* mingw32 / accessing element of untyped parameter *)
		"POINTTOPOINTS"]; (* mingw32 / accessing element of untyped parameter *)
	"wtypes.h", [
		"CBPCLIPDATA"; (* mingw32 / accessing element of untyped parameter *)
		"DECIMAL_SETZERO"]; (* mingw32 / accessing element of untyped parameter *)
	predefined_name, [
		"__DBL_DENORM_MIN__"]];; (* freebsd7 / smaller than minimum value *)

let is_known_uninterpretable_macro = setmap_mem ~set:known_uninterpretable_macros;;
