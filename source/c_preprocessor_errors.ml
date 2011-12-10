open Known_errors;;

let known_preprocessor_errors = make_setmap [
	"cdefs.h",
		["__FreeBSD_cc_version"]; (* freebsd7 *)
	"complex.h",
		["__WANT_LONG_DOUBLE_FORMAT__"]; (* darwin9 *)
	"float.h",
		["__clang_major__"]; (* mingw-w64 *)
	"in.h", [
		"CONFIG_FORCE_OUT_IFP"]; (* darwin9 *)
	"ioctl.h", [
		"COMPAT_43"; (* darwin9 *)
		"COMPAT_43_TTY"]; (* darwin9 *)
	"_mingw.h",
		["__GNUC_STDC_INLINE__"]; (* mingw32 *)
	"shellapi.h",
		["_WIN32_IE"]; (* mingw32 *)
	"stdint.h",
		["__LP64__"]; (* darwin9 *)
	"stdio.h", [
		"_FORTIFY_SOURCE"; (* darwin9 *)
		"__MINGW_FEATURES__"; (* mingw32 *)
		"__USE_MINGW_ANSI_STDIO"]; (* mingw-w64 *)
	"string.h",
		["_FORTIFY_SOURCE"]; (* darwin9 *)
	"types.h",
		["__LP64__"]; (* darwin9 *)
	"winbase.h", [
		"__USE_NTOSKRNL__"; (* mingw32 *)
		"_WIN32_WINDOWS"]; (* mingw32 *)
	"wingdi.h",
		["_WIN32_WINDOWS"]; (* mingw32 *)
	"winsock2.h",
		["__INSIDE_MSYS__"]; (* mingw32 *)
	"winuser.h",
		["_WIN32_WINDOWS"]];; (* mingw32 *)

let is_known_preprocessor_error = setmap_mem ~set:known_preprocessor_errors;;
