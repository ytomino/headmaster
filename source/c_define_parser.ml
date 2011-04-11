open C_lexical;;
open C_parser;;
open C_preprocessor;;
open C_syntax;;
open Value;;

let rec list_combination
	(f: ('b -> 'b) -> ('a * bool) list -> 'b -> 'b)
	(xs: 'a list)
	(last: 'b -> 'b)
	(start: 'b)
	: 'b =
(
	let rec loop xs (cont: 'b -> 'b) ys r = (
		begin match xs with
		| [] ->
			f cont (List.rev ys) r
		| x :: xr ->
			loop xr (loop xr cont ((x, true) :: ys)) ((x, false) :: ys) r
		end
	) in
	loop xs last [] start
);;

let ignore_macros = make_setmap [
	"basetyps.h", [
		"_COM_interface"; (* mingw32 / reserved word *)
		"DECLARE_ENUMERATOR_"; (* mingw32 / parameterized declaration *)
		"DECLARE_INTERFACE_"; (* mingw32 / parameterized declaration *)
		"DEFINE_GUID"; (* mingw32 *)
		"DEFINE_OLEGUID"; (* mingw32 *)
		"FWD_DECL"; (* mingw32 *)
		"IENUM_THIS"; (* mingw32 *)
		"IENUM_THIS_"; (* mingw32 *)
		"interface"; (* mingw32 / reserved word *)
		"REFCLSID"; (* mingw32 / conflicted with typedef *)
		"REFGUID"; (* mingw32 / conflicted with typedef *)
		"REFFMTID"; (* mingw32 / conflicted with typedef *)
		"REFIID"; (* mingw32 / conflicted with typedef *)
		"STDAPI"; (* mingw32 / storage class and type *)
		"STDAPI_"; (* mingw32 / storage class and parameterized attribute *)
		"STDAPIV"; (* mingw32 / storage class and type *)
		"STDAPIV_"; (* mingw32 / storage class and parameterized attribute *)
		"STDMETHOD"; (* mingw32 *)
		"STDMETHOD_"; (* mingw32 *)
		"STDMETHODIMP_"; (* mingw32 *)
		"STDMETHODIMPV_"; (* mingw32 *)
		"THIS"; (* mingw32 *)
		"THIS_"]; (* mingw32 *)
	"cairo-deprecated.h", [
		"cairo_atsui_font_face_create_for_atsu_font_id"; (* cairo / undefined *)
		"cairo_concat_matrix"; (* cairo / undefined *)
		"cairo_copy"; (* cairo / undefined *)
		"cairo_current_fill_rule"; (* cairo / undefined *)
		"cairo_current_font_extents"; (* cairo / undefined *)
		"cairo_current_line_cap";
		"cairo_current_line_join";
		"cairo_current_line_width";
		"cairo_current_matrix";
		"cairo_current_miter_limit";
		"cairo_current_operator";
		"cairo_current_path";
		"cairo_current_path_flat";
		"cairo_current_point";
		"cairo_current_target_surface";
		"cairo_current_tolerance";
		"cairo_default_matrix";
		"cairo_get_font_extents";
		"cairo_get_path";
		"cairo_get_path_flat";
		"cairo_get_status";
		"cairo_get_status_string";
		"cairo_init_clip";
		"cairo_inverse_transform_distance";
		"cairo_inverse_transform_point";
		"cairo_matrix_copy";
		"cairo_matrix_create";
		"cairo_matrix_destroy";
		"cairo_matrix_get_affine";
		"cairo_matrix_set_affine";
		"cairo_matrix_set_identity";
		"cairo_pattern_add_color_stop";
		"cairo_pdf_surface_set_dpi";
		"cairo_ps_surface_set_dpi";
		"cairo_scale_font";
		"cairo_select_font";
		"cairo_set_alpha";
		"cairo_set_pattern";
		"cairo_set_rgb_color";
		"cairo_set_target_drawable";
		"cairo_set_target_image";
		"cairo_set_target_pdf";
		"cairo_set_target_png";
		"cairo_set_target_ps";
		"cairo_set_target_quartz";
		"cairo_set_target_surface";
		"cairo_set_target_win32";
		"cairo_set_target_xcb"; (* cairo / undefined *)
		"cairo_show_surface"; (* cairo / undefined *)
		"cairo_status_string"; (* cairo / undefined *)
		"cairo_surface_create_for_image"; (* cairo / undefined *)
		"cairo_surface_get_filter"; (* cairo / undefined *)
		"cairo_surface_get_matrix"; (* cairo / undefined *)
		"cairo_surface_set_filter"; (* cairo / undefined *)
		"cairo_surface_set_matrix"; (* cairo / undefined *)
		"cairo_surface_set_repeat"; (* cairo / undefined *)
		"cairo_svg_surface_set_dpi"; (* cairo / undefined *)
		"cairo_transform_distance"; (* cairo / undefined *)
		"cairo_transform_font"; (* cairo / undefined *)
		"cairo_transform_point"; (* cairo / undefined *)
		"cairo_xcb_surface_create_for_pixmap_with_visual"; (* cairo / undefined *)
		"cairo_xcb_surface_create_for_window_with_visual"; (* cairo / undefined *)
		"cairo_xlib_surface_create_for_pixmap_with_visual"; (* cairo / undefined *)
		"cairo_xlib_surface_create_for_window_with_visual"]; (* cairo / undefined *)
	"cdefs.h", [
		"__aligned"; (* freebsd7 / parameterized attribute *)
		"__CONCAT"; (* freebsd7 / ## *)
		"__COPYRIGHT"; (* darwin9 / generic declaration *)
		"__DARWIN_1050"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_1050ALIAS"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_1050ALIAS_C"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_1050ALIAS_I"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_1050INODE64"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_ALIAS"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_ALIAS_C"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_ALIAS_I"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_EXTSN"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_EXTSN_C"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_INODE64"; (* darwin9 / parameterized inline assembler *)
		"__DARWIN_NO_LONG_LONG"; (* darwin9 / defined *)
		"__DECONST"; (* freebsd7 / generic declaration-specifiers *)
		"__DEVOLATILE"; (* freebsd7 / generic declaration-specifiers *)
		"__DEQUALIFY"; (* freebsd7 / generic declaration-specifiers *)
		"__FBSDID"; (* freebsd7 / generic declaration *)
		"__format_arg"; (* freebsd7 / parameterized attribute *)
		"__IDSTRING"; (* darwin9 / generic declaration *)
		"__nonnull"; (* freebsd7 / parameterized attribute *)
		"__offsetof"; (* freebsd7 / generic, field *)
		"__P"; (* darwin9 / parameter list *)
		"__printflike"; (* darwin9 / parameterized attribute *)
		"__PROJECT_VERSION"; (* darwin9 / generic declaration *)
		"__RCSID"; (* darwin9 / generic declaration *)
		"__RCSID_SOURCE"; (* freebsd7 / generic declaration *)
		"__rangeof"; (* freebsd7 / generic, field *)
		"__scanflike"; (* darwin9 / parameterized attribute *)
		"__SCCSID"; (* darwin9 / generic declaration *)
		"__section"; (* freebsd7 / parameterized attribute *)
		"__strong_reference"; (* freebsd7 / parameterized attribute *)
		"__warn_references"; (* freebsd7 / parameterized attribute *)
		"__weak_reference"]; (* freebsd7 / parameterized attribute *)
	"complex.h", [
		"complex"]; (* darwin9 / alias of _Complex *)
	"dirent.h", [
		"__DARWIN_STRUCT_DIRENTRY"; (* darwin9 / partial declaration *)
		"d_ino"; (* freebsd7 / renaming field of struct in the other file *)
		"dirfd"; (* darwin9 / hard to type inference...for future *)
		"_GENERIC_DIRSIZ"]; (* freebsd7 / hard to type inference...for future *)
	"e_os2.h", [
		"OPENSSL_UNISTD_IO"]; (* OpenSSL / header *)
	"fenv.h", [
		"__get_mxcsr"; (* freebsd7 / hard to type inference...for future *)
		"__set_mxcsr"]; (* freebsd7 / hard to type inference...for future *)
	"gc.h", [
		"GC_EXTRAS"; (* Boehm-GC / parameter list *)
		"GC_EXTRA_PARAMS"]; (* Boehm-GC / formal parameter list *)
	"gc_typed.h", [
		"GC_WORD_OFFSET"]; (* Boehm-GC / generic, field *)
	"gmp.h", [
		"__GMP_CAST"; (* GMP / unclear cast or function call *)
		"__GMP_DECLSPEC_EXPORT"; (* GMP / __declspec in no Windows *)
		"__GMP_DECLSPEC_IMPORT"; (* GMP / __declspec in no Windows *)
		"gmp_fprintf"; (* GMP / #include <stdio.h> *)
		"gmp_fscanf"; (* GMP / #include <stdio.h> *)
		"gmp_obstack_printf"; (* GMP / #include <stdio.h> *)
		"gmp_obstack_vprintf"; (* GMP / #include <stdio.h> *)
		"gmp_vasprintf"; (* GMP / #include <stdio.h> *)
		"gmp_vfprintf"; (* GMP / #include <stdio.h> *)
		"gmp_vfscanf"; (* GMP / #include <stdio.h> *)
		"gmp_vprintf"; (* GMP / #include <stdio.h> *)
		"gmp_vscanf"; (* GMP / #include <stdio.h> *)
		"gmp_vsnprintf"; (* GMP / #include <stdio.h> *)
		"gmp_vsprintf"; (* GMP / #include <stdio.h> *)
		"gmp_vsscanf"; (* GMP / #include <stdio.h> *)
		"__GMPN_ADD"; (* GMP / partial function *)
		"__GMPN_ADD_1"; (* GMP / multi-statements *)
		"__GMPN_AORS"; (* GMP / partial function *)
		"__GMPN_AORS_1"; (* GMP / multi-statements *)
		"__GMPN_SUB"; (* GMP / partial function *)
		"__GMPN_SUB_1"; (* GMP / multi-statements *)
		"__GMPZ_FITS_UTYPE_P"; (* GMP / multi-statements *)
		"mpf_inp_str"; (* GMP / #include <stdio.h> *)
		"mpf_out_str"; (* GMP / #include <stdio.h> *)
		"mpf_sgn"; (* GMP / hard to type inference...for future *)
		"mpq_cmp_si"; (* GMP / hard to type inference...for future *)
		"mpq_cmp_ui"; (* GMP / hard to type inference...for future *)
		"mpq_denref"; (* GMP / hard to type inference...for future *)
		"mpq_inp_str"; (* GMP / #include <stdio.h> *)
		"mpq_numref"; (* GMP / hard to type inference...for future *)
		"mpq_out_str"; (* GMP / #include <stdio.h> *)
		"mpq_sgn"; (* GMP / hard to type inference...for future *)
		"mpz_cmp_si"; (* GMP / hard to type inference...for future *)
		"mpz_cmp_ui"; (* GMP / hard to type inference...for future *)
		"mpz_even_p"; (* GMP / hard to type inference...for future *)
		"mpz_inp_raw"; (* GMP / #include <stdio.h> *)
		"mpz_inp_str"; (* GMP / #include <stdio.h> *)
		"mpz_odd_p"; (* GMP / hard to type inference...for future *)
		"mpz_out_raw"; (* GMP / #include <stdio.h> *)
		"mpz_out_str"; (* GMP / #include <stdio.h> *)
		"mpz_sgn"]; (* GMP / hard to type inference...for future *)
	"_mingw.h", [
		"__MINGW_ATTRIB_NONNULL"; (* mingw32 / parameterized attribute *)
		"__UNUSED_PARAM"; (* mingw32 / parameterized attribute *)
		"__USE_MINGW_ANSI_STDIO"]; (* mingw32 / condition-compiling macro *)
	"mmsystem.h", [
		"MCI_ANIM_WINDOW_DEFAULT"; (* mingw32 / bug? 0xL *)
		"MCI_OVLY_WINDOW_DEFAULT"]; (* mingw32 / bug? 0xL *)
	"mpc.h", [
		"mpc_imagref"; (* MPC / hard to type inference...for future *)
		"__MPC_PROTO"; (* MPC / parameter list *)
		"mpc_realref"]; (* MPC / hard to type inference...for future *)
	"mpfr.h", [
		"mpfr_add_one_ulp"; (* MPFR / hard to type inference...for future *)
		"mpfr_custom_get_significand"; (* MPFR / hard to type inference...for future *)
		"__mpfr_default_fp_bit_precision"; (* MPFR / bug? mpfr_get_default_fp_bit_precision was undefined *)
		"mpfr_init_set"; (* MPFR / hard to type inference...for future *)
		"mpfr_set"; (* MPFR / hard to type inference...for future *)
		"mpfr_sub_one_ulp"; (* MPFR / hard to type inference...for future *)
		"_MPFR_PROTO"; (* MPFR / parameter list *)
		"MPFR_SIGN"]; (* MPFR / hard to type inference...for future *)
	"objbase.h", [
		"LISet32"; (* mingw32 / hard to type inference...for future *)
		"ULISet32"; (* mingw32 / hard to type inference...for future *)
		"WINOLEAPI_"]; (* mingw32 *)
	"oleauto.h", [
		"V_ARRAY"; (* mingw32 / hard to type inference...for future *)
		"V_ARRAYREF"; (* mingw32 / hard to type inference...for future *)
		"V_BLOB"; (* mingw32 / hard to type inference...for future *)
		"V_BOOL"; (* mingw32 / hard to type inference...for future *)
		"V_BOOLREF"; (* mingw32 / hard to type inference...for future *)
		"V_BSTR"; (* mingw32 / hard to type inference...for future *)
		"V_BSTRREF"; (* mingw32 / hard to type inference...for future *)
		"V_BYREF"; (* mingw32 / hard to type inference...for future *)
		"V_CLSID"; (* mingw32 / hard to type inference...for future *)
		"V_CY"; (* mingw32 / hard to type inference...for future *)
		"V_CYREF"; (* mingw32 / hard to type inference...for future *)
		"V_DATE"; (* mingw32 / hard to type inference...for future *)
		"V_DATEREF"; (* mingw32 / hard to type inference...for future *)
		"V_DECIMAL"; (* mingw32 / hard to type inference...for future *)
		"V_DECIMALREF"; (* mingw32 / hard to type inference...for future *)
		"V_DISPATCH"; (* mingw32 / hard to type inference...for future *)
		"V_DISPATCHREF"; (* mingw32 / hard to type inference...for future *)
		"V_ERROR"; (* mingw32 / hard to type inference...for future *)
		"V_ERRORREF"; (* mingw32 / hard to type inference...for future *)
		"V_FILETIME"; (* mingw32 / hard to type inference...for future *)
		"V_FILETIMEREF"; (* mingw32 / hard to type inference...for future *)
		"V_I1"; (* mingw32 / hard to type inference...for future *)
		"V_I2"; (* mingw32 / hard to type inference...for future *)
		"V_I2REF"; (* mingw32 / hard to type inference...for future *)
		"V_I4"; (* mingw32 / hard to type inference...for future *)
		"V_I4REF"; (* mingw32 / hard to type inference...for future *)
		"V_I8"; (* mingw32 / hard to type inference...for future *)
		"V_I8REF"; (* mingw32 / hard to type inference...for future *)
		"V_INT_PTR"; (* mingw32 / hard to type inference...for future *)
		"V_INT_PTRREF"; (* mingw32 / hard to type inference...for future *)
		"V_ISARRAY"; (* mingw32 / hard to type inference...for future *)
		"V_ISBYREF"; (* mingw32 / hard to type inference...for future *)
		"V_ISVECTOR"; (* mingw32 / hard to type inference...for future *)
		"V_LPSTR"; (* mingw32 / hard to type inference...for future *)
		"V_LPSTRREF"; (* mingw32 / hard to type inference...for future *)
		"V_LPWSTR"; (* mingw32 / hard to type inference...for future *)
		"V_LPWSTRREF"; (* mingw32 / hard to type inference...for future *)
		"V_NONE"; (* mingw32 / hard to type inference...for future *)
		"V_R4"; (* mingw32 / hard to type inference...for future *)
		"V_R4REF"; (* mingw32 / hard to type inference...for future *)
		"V_R8"; (* mingw32 / hard to type inference...for future *)
		"V_R8REF"; (* mingw32 / hard to type inference...for future *)
		"V_UI1"; (* mingw32 / hard to type inference...for future *)
		"V_UI1REF"; (* mingw32 / hard to type inference...for future *)
		"V_UI2"; (* mingw32 / hard to type inference...for future *)
		"V_UI4"; (* mingw32 / hard to type inference...for future *)
		"V_UI4REF"; (* mingw32 / hard to type inference...for future *)
		"V_UI8"; (* mingw32 / hard to type inference...for future *)
		"V_UI8REF"; (* mingw32 / hard to type inference...for future *)
		"V_UINT_PTR"; (* mingw32 / hard to type inference...for future *)
		"V_UINT_PTRREF"; (* mingw32 / hard to type inference...for future *)
		"V_UNION"; (* mingw32 / hard to type inference...for future *)
		"V_UNKNOWN"; (* mingw32 / hard to type inference...for future *)
		"V_UNKNOWNREF"; (* mingw32 / hard to type inference...for future *)
		"V_UUID"; (* mingw32 / hard to type inference...for future *)
		"V_VARIANTREF"; (* mingw32 / hard to type inference...for future *)
		"V_VT"; (* mingw32 / hard to type inference...for future *)
		"WINOLEAUTAPI_"]; (* mingw32 *)
	"opensslconf.h", [
		"OPENSSL_UNISTD"]; (* OpenSSL / header *)
	"param.h", [
		"CBSIZE"; (* freebsd7 / cblock was undefined... be #include <sys/clist.h> *)
		"MID_MACHINE"; (* freebsd7 / MID_I386 was undefined... be #include <sys/imgact_aout.h> *)
		"NPDEPG"; (* freebsd7 / pd_entry_t was undefined... be #include <machine/pmap.h> *)
		"NPDEPTD"; (* freebsd7 / pd_entry_t was undefined... be #include <machine/pmap.h> *)
		"NPTEPG"]; (* freebsd7 / pd_entry_t was undefined... be #include <machine/pmap.h> *)
	"pngconf.h", [
		"PNG_ABORT"; (* libpng / abort was undefined... be #include <stdlib.h> *)
		"PNG_EXPORT"; (* libpng / parameterized declaration *)
		"PNG_EXPORT_VAR"; (* libpng / parameterized declaration *)
		"png_jmpbuf"]; (* libpng / hard to type inference...for future *)
	"queue.h", [
		"LIST_EMPTY"; (* freebsd7 / hard to type inference...for future *)
		"LIST_ENTRY"; (* freebsd7 / parameterized declaration *)
		"LIST_FIRST"; (* freebsd7 / hard to type inference...for future *)
		"LIST_FOREACH"; (* freebsd7 / partial statement *)
		"LIST_FOREACH_SAFE"; (* freebsd7 / partial statement *)
		"LIST_HEAD"; (* freebsd7 / parameterized declaration *)
		"LIST_HEAD_INITIALIZER"; (* freebas7 / parameterized initializer *)
		"LIST_INIT"; (* freebsd7 / hard to type inference...for future *)
		"LIST_INSERT_AFTER"; (* freebsd7 / hard to type inference...for future *)
		"LIST_INSERT_BEFORE"; (* freebsd7 / hard to type inference...for future *)
		"LIST_INSERT_HEAD"; (* freebsd7 / hard to type inference...for future *)
		"LIST_NEXT"; (* freebsd7 / hard to type inference...for future *)
		"LIST_REMOVE"; (* freebsd7 / hard to type inference...for future *)
		"SLIST_EMPTY"; (* freebsd7 / hard to type inference...for future *)
		"SLIST_ENTRY"; (* freebsd7 / parameterized declaration *)
		"SLIST_FIRST"; (* freebsd7 / hard to type inference...for future *)
		"SLIST_FOREACH"; (* freebsd7 / partial statement *)
		"SLIST_FOREACH_SAFE"; (* freebsd7 / partial statement *)
		"SLIST_FOREACH_PREVPTR"; (* freebsd7 / partial statement *)
		"SLIST_HEAD"; (* freebsd7 / parameterized declaration *)
		"SLIST_HEAD_INITIALIZER"; (* freebas7 / parameterized initializer *)
		"SLIST_INIT"; (* freebsd7 / hard to type inference...for future *)
		"SLIST_INSERT_AFTER"; (* freebsd7 / hard to type inference...for future *)
		"SLIST_INSERT_HEAD"; (* freebsd7 / hard to type inference...for future *)
		"SLIST_NEXT"; (* freebsd7 / hard to type inference...for future *)
		"SLIST_REMOVE"; (* freebsd7 / hard to type inference...for future *)
		"SLIST_REMOVE_HEAD"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_CONCAT"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_EMPTY"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_ENTRY"; (* freebsd7 / parameterized declaration *)
		"STAILQ_FIRST"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_FOREACH"; (* freebsd7 / partial statement *)
		"STAILQ_FOREACH_SAFE"; (* freebsd7 / partial statement *)
		"STAILQ_HEAD"; (* freebsd7 / parameterized declaration *)
		"STAILQ_HEAD_INITIALIZER"; (* freebas7 / parameterized initializer *)
		"STAILQ_INIT"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_INSERT_AFTER"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_INSERT_HEAD"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_INSERT_TAIL"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_LAST"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_NEXT"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_REMOVE"; (* freebsd7 / hard to type inference...for future *)
		"STAILQ_REMOVE_HEAD"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_CONCAT"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_EMPTY"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_ENTRY"; (* freebsd7 / parameterized declaration *)
		"TAILQ_FIRST"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_FOREACH"; (* freebsd7 / partial statement *)
		"TAILQ_FOREACH_REVERSE"; (* freebsd7 / partial statement *)
		"TAILQ_FOREACH_REVERSE_SAFE"; (* freebsd7 / partial statement *)
		"TAILQ_FOREACH_SAFE"; (* freebsd7 / partial statement *)
		"TAILQ_HEAD"; (* freebsd7 / parameterized declaration *)
		"TAILQ_HEAD_INITIALIZER"; (* freebas7 / parameterized initializer *)
		"TAILQ_INIT"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_INSERT_AFTER"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_INSERT_BEFORE"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_INSERT_HEAD"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_INSERT_TAIL"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_LAST"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_NEXT"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_PREV"; (* freebsd7 / hard to type inference...for future *)
		"TAILQ_REMOVE"]; (* freebsd7 / hard to type inference...for future *)
	"rpcdce.h", [
		"RPC_C_SECURITY_QOS_VERSION"; (* mingw32 / L ?? *)
		"_SEC_WINNT_AUTH_IDENTITY"]; (* mingw32 / struct tag *)
	"rpcndr.h", [
		"boolean_array_from_ndr"; (* mingw32 / hard to type inference...for future *)
		"boolean_from_ndr"; (* mingw32 / hard to type inference...for future *)
		"byte_array_from_ndr"; (* mingw32 / hard to type inference...for future *)
		"byte_from_ndr"; (* mingw32 / hard to type inference...for future *)
		"MIDL_INTERFACE"; (* mingw32 / reserved word *)
		"NdrFieldPad"; (* mingw32 / generic, field *)
		"NdrFieldOffset"; (* mingw32 / generic, field *)
		"NdrMarshSCtxtHdl"; (* mingw32 / bug? not enough "" *)
		"NDRSContextValue"; (* mingw32 / hard to type inference...for future *)
		"NdrUnMarshSCtxtHdl"; (* mingw32 / bug? misspell of N*DR*SContextUnMarshall *)
		"small_array_from_ndr"; (* mingw32 / hard to type inference...for future *)
		"small_from_ndr"]; (* mingw32 / hard to type inference...for future *)
	"select.h", [
		"FD_COPY"; (* darwin9 / bcopy was undefined... be #include <string.h> *)
		"FD_CLR"; (* darwin9 / hard to type inference...for future *)
		"FD_ISSET"; (* freebsd7 / hard to type inference...for future *)
		"FD_SET"; (* darwin9 / hard to type inference...for future *)
		"FD_ZERO"]; (* darwin9 / sizeof(macro argument) *)
	"stat.h", [
		"__DARWIN_STRUCT_STAT64"; (* darwin9 / partial declaration *)
		"__DARWIN_STRUCT_STAT64_TIMES"]; (* darwin9 / partial declaration *)
	"stddef.h", [
		"offsetof"]; (* darwin9 / generic, field *)
	"stdint.h", [
		"INTMAX_C"; (* mingw32 / ## *)
		"UINTMAX_C"]; (* mingw32 / ## *)
	"stdio.h", [
		"clearerr_unlocked"; (* darwin9 / hard to type inference...for future *)
		"ferror_unlocked"; (* darwin9 / hard to type inference...for future *)
		"feof_unlocked";  (* darwin9 / hard to type inference...for future *)
		"fileno_unlocked"; (* darwin9 / hard to type inference...for future *)
		"__sclearerr"; (* darwin9 / hard to type inference...for future *)
		"__sfeof"; (* darwin9 / hard to type inference...for future *)
		"__sferror"; (* darwin9 / hard to type inference...for future *)
		"__sfileno"; (* darwin9 / hard to type inference...for future *)
		"__sgetc"]; (* darwin9 / hard to type inference...for future *)
	"_structs.h", [
		"__DARWIN_FD_COPY"; (* darwin9 / bcopy was undefined... be #include <string.h> *)
		"__DARWIN_FD_CLR"; (* darwin9 / hard to type inference...for future *)
		"__DARWIN_FD_SET"; (* darwin9 / hard to type inference...for future *)
		"__DARWIN_FD_ZERO"; (* darwin9 / sizeof(macro argument) *)
		"I386_MCONTEXT_SIZE"]; (* darwin9 / undefined struct *)
	"tgmath.h", [
		"acos"; (* gcc *)
		"acosh"; (* gcc *)
		"asin"; (* gcc *)
		"asinh"; (* gcc *)
		"atan"; (* gcc *)
		"atan2"; (* gcc *)
		"atanh"; (* gcc *)
		"carg"; (* gcc *)
		"cbrt"; (* gcc *)
		"ceil"; (* gcc *)
		"cimag"; (* gcc *)
		"conj"; (* gcc *)
		"copysign"; (* gcc *)
		"cos"; (* gcc *)
		"cosh"; (* gcc *)
		"cproj"; (* gcc *)
		"creal"; (* gcc *)
		"erf"; (* gcc *)
		"erfc"; (* gcc *)
		"exp"; (* gcc *)
		"exp2"; (* gcc *)
		"expm1"; (* gcc *)
		"fabs"; (* gcc *)
		"fdim"; (* gcc *)
		"floor"; (* gcc *)
		"fma"; (* gcc *)
		"fmax"; (* gcc *)
		"fmin"; (* gcc *)
		"fmod"; (* gcc *)
		"frexp"; (* gcc *)
		"hypot"; (* gcc *)
		"ilogb"; (* gcc *)
		"ldexp"; (* gcc *)
		"lgamma"; (* gcc *)
		"llrint"; (* gcc *)
		"llround"; (* gcc *)
		"log"; (* gcc *)
		"log10"; (* gcc *)
		"log1p"; (* gcc *)
		"log2"; (* gcc *)
		"logb"; (* gcc *)
		"lrint"; (* gcc *)
		"lround"; (* gcc *)
		"nearbyint"; (* gcc *)
		"nextafter"; (* gcc *)
		"nexttoward"; (* gcc *)
		"pow"; (* gcc *)
		"remainder"; (* gcc *)
		"remquo"; (* gcc *)
		"rint"; (* gcc *)
		"round"; (* gcc *)
		"scalbln"; (* gcc *)
		"scalbn"; (* gcc *)
		"sin"; (* gcc *)
		"sinh"; (* gcc *)
		"sqrt"; (* gcc *)
		"tan"; (* gcc *)
		"tanh"; (* gcc *)
		"__tg_cplx"; (* gcc *)
		"__tg_dbl"; (* gcc *)
		"__tg_choose"; (* gcc *)
		"__tg_choose_2"; (* gcc *)
		"__tg_choose_3"; (* gcc *)
		"__tg_ldbl"; (* gcc *)
		"tgamma"; (* gcc *)
		"trunc"]; (* gcc *)
	"time.h", [
		"timeradd"; (* darwin9 / hard to type inference...for future *)
		"timerclear"; (* darwin9 / hard to type inference...for future *)
		"timercmp"; (* darwin9 / operator *)
		"timerisset"; (* darwin9 / hard to type inference...for future *)
		"timersub"; (* darwin9 / hard to type inference...for future *)
		"TIMESPEC_TO_TIMEVAL"; (* darwin9 / hard to type inference...for future *)
		"TIMEVAL_TO_TIMESPEC"; (* darwin9 / hard to type inference...for future *)
		"timevalcmp"]; (* darwin9 / operator *)
	"timespec.h", [
		"TIMEVAL_TO_TIMESPEC"; (* freebsd7 / hard to type inference...for future *)
		"TIMESPEC_TO_TIMEVAL"]; (* freebsd7 / hard to type inference...for future *)
	"trap.h", [
		"ILL_RESAD_FAULT"; (* freebsd7 / bug? T_RESADFLT was undefined anywhere *)
		"ILL_RESOP_FAULT"]; (* freebsd7 / bug? T_RESOPFLT was undefined anywhere *)
	"types.h", [
		"FD_COPY"; (* darwin9 / bcopy was undefined... be #include <string.h> *)
		"FD_CLR"; (* darwin9 / hard to type inference...for future *)
		"FD_SET"; (* darwin9 / hard to type inference...for future *)
		"FD_ZERO"; (* darwin9 / sizeof(macro argument) *)
		"__offsetof"]; (* darwin9 / generic, field *)
	"winbase.h", [
		"GetEnvironmentStrings"; (* mingw32 / duplicated by macro and normal function *)
		"GetVolumeNameForVolumeMountPoint"; (* mingw32 / #ifdef *)
		"GetVolumePathName"; (* mingw32 / #ifdef *)
		"GetVolumePathNamesForVolumeName"; (* mingw32 / #ifdef *)
		"DefineHandleTable"; (* mingw32 / parameter list *)
		"HasOverlappedIoCompleted"; (* mingw32 / hard to type inference...for future *)
		"SetVolumeMountPoint"]; (* mingw32 / #ifdef *)
	"windef.h", [
		"DECLARE_STDCALL_P"; (* mingw32 / generic declaration-specifiers *)
		"_declspec"]; (* mingw32 / parameterized attribute *)
	"winnt.h", [
		"C_ASSERT"; (* mingw32 / static assert *)
		"CONTAINING_RECORD"; (* mingw32 / generic, field *)
		"DECLSPEC_ALIGN"; (* mingw32 / parameterized attribute *)
		"FIELD_OFFSET"; (* mingw32 / generic, field *)
		"_SLIST_ENTRY"; (* mingw32 / struct tag *)
		"TEXT"]; (* mingw32 / ## *)
	"winsock2.h", [
		"FD_SET"; (* mingw32 / conflicted with typedef *)
		"h_addr"; (* mingw32 / element alias and dereferencing *)
		"timerclear"; (* mingw32 / hard to type inference...for future *)
		"timercmp"; (* mingw32 / operator *)
		"timerisset"]; (* mingw32 / hard to type inference...for future *)
	"winspool.h", [
		"DeletePrintProcessor"; (* mingw32 / bug? misspell of DeletePrint*er*ProcessorW *)
		"DeletePrintProvidor"; (* mingw32 / bug? misspell of DeletePrint*er*ProviderW *)
		"GetDefaultPrinter"]; (* mingw32 / #ifdef *)
	"winuser.h", [
		"BroadcastSystemMessage"; (* mingw32 / bug? declarated as normal function *)
		"BroadcastSystemMessageEx"; (* mingw32 / #ifdef *)
		"DC_CAPTION"; (* mingw32 / #ifdef *)
		"DC_NC"; (* mingw32 / #ifdef *)
		"DEKSTOPENUMPROC"; (* mingw32 / bug? misspell of DE*SK*TOPENUMPROCW *)
		"EnumDisplaySettingsEx"; (* mingw32 / #ifdef *)
		"GetRawInputDeviceInfo"; (* mingw32 / #ifdef *)
		"POINTSTOPOINT"; (* mingw32 / hard to type inference...for future *)
		"POINTTOPOINTS"; (* mingw32 / hard to type inference...for future *)
		"RegisterDeviceNotification"; (* mingw32 / #ifdef *)
		"WHEEL_PAGESCROLL"]; (* mingw32 / #include <limits.h> *)
	"wtypes.h", [
		"CBPCLIPDATA"; (* mingw32 / hard to type inference...for future *)
		"DECIMAL_SETZERO"]; (* mingw32 / hard to type inference...for future *)
	predefined_name, [
		"__DBL_DENORM_MIN__"; (* freebsd7 / underflow *)
		"__declspec"; (* mingw32 / parameterized attribute *)
		"__USER_LABEL_PREFIX__"]];; (* darwin9 / single "_" ...? *)

module DefineParser
	(Literals: LiteralsType)
	(LexicalElement: LexicalElementType (Literals).S)
	(Preprocessor: PreprocessorType (Literals) (LexicalElement).S)
	(Syntax: SyntaxType (Literals).S) =
struct
	module Parser = Parser (Literals) (LexicalElement) (Syntax);;
	
	type 'a p = 'a Syntax.p;;
	
	let rec has_sharps (xs: 'a Parser.in_t): bool = (
		begin match xs with
		| lazy (`cons (_, it, xs)) ->
			begin match it with
			| `sharp | `d_sharp ->
				true
			| _ ->
				has_sharps xs
			end
		| lazy (`nil _) ->
			false
		end
	);;
	
	let parse_operator_option
		(xs: 'a Parser.in_t)
		: operator p option * 'a Parser.in_t =
	(
		begin match xs with
		| lazy (`cons (op_p, (#operator as op_e), xs)) ->
			Some (op_p, op_e), xs
		| _ ->
			None, xs
		end
	);;
	
	type define = [
		| `operator of operator
		| `declaration_specifiers of Syntax.declaration_specifiers
		| `initializer_t of Syntax.initializer_t
		| `function_expr of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.expression
		| `function_stmt of (string p * [`typedef | `value]) list * [`varargs | `none] * Syntax.statement
		| `any of string];;
	
	let is_alias_of_other_macro
		(macros: Preprocessor.define_map)
		(macro: Preprocessor.define_item)
		: Preprocessor.define_item option =
	(
		if macro.Preprocessor.df_has_arguments then None else
		begin match macro.Preprocessor.df_contents with
		| lazy (`cons (_, `ident target_name, lazy (`nil _))) ->
			begin try
				Some (StringMap.find target_name macros)
			with Not_found ->
				None
			end
		| _ ->
			None
		end
	);;
	
	let rec parse_define
		?(name: string option)
		(error: ranged_position -> string -> unit)
		(lang: language)
		(typedefs: Parser.typedef_set)
		(macros: Preprocessor.define_map)
		(macro: Preprocessor.define_item)
		: define p =
	(
		let ps = macro.Preprocessor.df_position in
		if setmap_mem ps macro.Preprocessor.df_name ignore_macros then ps, (`any "suppressed") else
		let name =
			begin match name with
			| Some name ->
				name
			| None ->
				macro.Preprocessor.df_name
			end
		in
		begin match is_alias_of_other_macro macros macro with
		| Some target ->
			let _, result = parse_define ~name error lang typedefs macros target in
			ps, result
		| None ->
			let xs = macro.Preprocessor.df_contents in
			let has_arguments = macro.Preprocessor.df_has_arguments in
			if has_arguments && has_sharps xs then ps, (`any "has # or ##") else (* exclude macros having # or ## *)
			let has_error = ref false in
			let dummy_error _ _ = has_error := true in
			let xs = lazy (Preprocessor.preprocess
				dummy_error
				lang
				(fun ~current ?next _ _ ->
					ignore current;
					ignore next;
					has_error := true;
					(LazyList.find_nil xs :> Preprocessor.in_prim))
				false
				macros
				StringMap.empty
				xs)
			in
			let (_: [`nil of ranged_position * Preprocessor.define_map]) = LazyList.find_nil xs in (* error check *)
			begin try
				if !has_error then (
					error ps ("macro " ^ name ^ " could not be interpreted. (preprocessor error)");
					ps, (`any "preprocessor error")
				) else if LazyList.is_empty xs then (
					ps, (`any "empty")
				) else if has_arguments then (
					let varargs = if macro.Preprocessor.df_varargs then `varargs else `none in
					let last_f result =
						error ps ("function-macro " ^ name ^ " could not be interpreted.");
						result
					in
					list_combination (fun cont args dummy_result ->
						let args =
							List.map (fun (v, is_type) ->
								v, (if is_type then `typedef else `value)
							) args 
						in
						let typedefs =
							List.fold_right (fun ((_, k), is_type) v ->
								if is_type = `typedef then StringSet.add k v else v
							) args typedefs
						in
						let expr, xr = has_error := false; Parser.parse_expression_or_error dummy_error lang typedefs xs in
						if not !has_error && LazyList.is_empty xr && expr <> `error then (
							begin match expr with
							| `some expr -> ps, `function_expr (args, varargs, snd expr)
							| `error -> assert false
							end
						) else
						let stmt, xr = has_error := false; Parser.parse_statement_or_error ~semicolon_need:false dummy_error lang typedefs xs in
						if not !has_error && LazyList.is_empty xr && stmt <> `error then (
							begin match stmt with
							| `some stmt -> ps, `function_stmt (args, varargs, snd stmt)
							| `error -> assert false
							end
						) else (
							cont dummy_result
						)
					) macro.Preprocessor.df_args last_f (ps, `any "parser error")
				) else (
					let op, xr = has_error := false; parse_operator_option xs in
					if not !has_error && LazyList.is_empty xr && op <> None then (
						begin match op with
						| Some op -> ps, `operator (snd op)
						| None -> assert false
						end
					) else
					let spec, xr = has_error := false; Parser.parse_declaration_specifiers_option dummy_error lang typedefs xs in
					if not !has_error && LazyList.is_empty xr && spec <> `none then (
						begin match spec with
						| `some spec -> ps, `declaration_specifiers (snd spec)
						| `none -> assert false
						end
					) else
					let expr, xr = has_error := false; Parser.parse_initializer_or_error dummy_error lang typedefs xs in
					if not !has_error && LazyList.is_empty xr && expr <> `error then (
						begin match expr with
						| `some expr -> ps, `initializer_t (snd expr)
						| `error -> assert false
						end
					) else (
						error ps ("macro " ^ name ^ " could not be interpreted.");
						ps, `any "parser error"
					)
				)
			with Assert_failure _ as e ->
				prerr_string (Printexc.to_string e);
				prerr_newline ();
				Printexc.print_backtrace stderr;
				flush stderr;
				error ps ("macro " ^ name ^ " could not be interpreted. (assert failure)");
				raise e
			end
		end
	);;
	
	let map
		(error: ranged_position -> string -> unit)
		(lang: language)
		(typedefs: Parser.typedef_set)
		(items: Preprocessor.define_map)
		: define p StringMap.t =
	(
		StringMap.map (parse_define error lang typedefs items) items
	);;
	
end;;
