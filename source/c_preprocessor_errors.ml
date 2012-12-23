open Known_errors;;

module StringMap = StringMap;; (* for C_preprocessor *)

let known_undefined_macros = make_setmap [
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
	"kmod.h",
		["PRAGMA_MARK"]; (* darwin10 *)
	"_mingw.h",
		["__GNUC_STDC_INLINE__"]; (* mingw32 *)
	"mount.h",
		["COMPAT_GETFSSTAT"]; (* darwin9 *)
	"sdkddkver.h",
		["_WIN32_WINNT"]; (* mingw-w64 *)
	"shellapi.h",
		["_WIN32_IE"]; (* mingw32 *)
	"specstrings.h",
		["_MSC_VER"]; (* mingw-w64 *)
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
	"vm_region.h",
		["VM32_SUPPORT"]; (* darwin10 *)
	"winbase.h", [
		"__USE_NTOSKRNL__"; (* mingw32 *)
		"_WIN32_WINDOWS"]; (* mingw32 *)
	"wingdi.h",
		["_WIN32_WINDOWS"]; (* mingw32 *)
	"winnt.h",
		["_MSC_VER"]; (* mingw-w64 *)
	"winsock2.h",
		["__INSIDE_MSYS__"]; (* mingw32 *)
	"winuser.h", [
		"_WIN32_WINDOWS"; (* mingw32 *)
		"_WIN32_WCE"]];; (* mingw-w64 *)

let is_known_undefined_macros = setmap_mem ~set:known_undefined_macros;;

let known_redefine_compiler_macros = make_setmap [
	"features.h", [
		"__STDC_ISO_10646__"]; (* linux *)
	"predefs.h", [
		"__STDC_IEC_559__"; (* linux *)
		"__STDC_IEC_559_COMPLEX__"]];; (* linux *)

let is_known_redefine_compiler_macros = setmap_mem ~set:known_redefine_compiler_macros;;

let known_redefine_extended_words = make_setmap [
	"cdefs.h", [
		"__const"; (* darwin10 *)
		"__restrict"]; (* darwin10 *)
	"_mingw.h", [
		"__int64"]];; (* mingw-w64 *)

let is_known_redefine_extended_words = setmap_mem ~set:known_redefine_extended_words;;

let known_defined_push_macros = make_setmap [
	"unknwn.h",
		["interface"]; (* mingw-w64 *)
	"winbase.h",
		["GetEnvironmentStrings"]];; (* mingw-w64 *)

let is_known_defined_push_macros = setmap_mem ~set:known_defined_push_macros;;
