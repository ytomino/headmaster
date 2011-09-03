open Known_errors;;
open Position;;

let known_define_parser_errors = make_setmap [
	"basetyps.h", [
		"_COM_interface"; (* mingw32 / alias of reserved word *)
		"DECLARE_ENUMERATOR_"; (* mingw32 / parameterized declaration *)
		"DECLARE_INTERFACE_"; (* mingw32 / parameterized declaration *)
		"DEFINE_GUID"; (* mingw32 / parameterized declaration *)
		"DEFINE_OLEGUID"; (* mingw32 / parameterized declaration *)
		"FWD_DECL"; (* mingw32 / parameterized declaration *)
		"IENUM_THIS"; (* mingw32 / storage class and type *)
		"IENUM_THIS_"; (* mingw32 / storage class and parameterized type *)
		"interface"; (* mingw32 / alias of reserved word *)
		"REFCLSID"; (* mingw32 / conflicted with typedef *)
		"REFGUID"; (* mingw32 / conflicted with typedef *)
		"REFFMTID"; (* mingw32 / conflicted with typedef *)
		"REFIID"; (* mingw32 / conflicted with typedef *)
		"STDAPI"; (* mingw32 / storage class and type *)
		"STDAPI_"; (* mingw32 / storage class and parameterized type *)
		"STDAPIV"; (* mingw32 / storage class and type *)
		"STDAPIV_"; (* mingw32 / storage class and parameterized type *)
		"STDMETHOD"; (* mingw32 / storage class and type *)
		"STDMETHOD_"; (* mingw32 / storage class and parameterized type *)
		"STDMETHODIMP_"; (* mingw32 / storage class and type *)
		"STDMETHODIMPV_"; (* mingw32 / storage class and parameterized type *)
		"THIS"; (* mingw32 / only used in interface declaration *)
		"THIS_"]; (* mingw32 / only used in interface declaration *)
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
		"__DARWIN_NO_LONG_LONG"; (* darwin9 / "defined" used out of preprocessor *)
		"__DECONST"; (* freebsd7 / generic declaration *)
		"__DEQUALIFY"; (* freebsd7 / generic declaration *)
		"__DEVOLATILE"; (* freebsd7 / generic declaration *)
		"__FBSDID"; (* freebsd7 / generic declaration *)
		"__format_arg"; (* freebsd7 / parameterized attribute *)
		"__IDSTRING"; (* darwin9 / generic declaration *)
		"__nonnull"; (* freebsd7 / parameterized attribute *)
		"__offsetof"; (* freebsd7 / generic, field *)
		"__printflike"; (* darwin9 / parameterized attribute *)
		"__PROJECT_VERSION"; (* darwin9 / generic declaration *)
		"__rangeof"; (* freebsd7 / generic, field *)
		"__RCSID"; (* darwin9 / generic declaration *)
		"__RCSID_SOURCE"; (* freebsd7 / generic declaration *)
		"__scanflike"; (* darwin9 / parameterized attribute *)
		"__SCCSID"; (* darwin9 / generic declaration *)
		"__section"]; (* freebsd7 / parameterized attribute *)
	"complex.h", [
		"complex"]; (* darwin9 / alias of _Complex *)
	"dirent.h", [
		"__DARWIN_STRUCT_DIRENTRY"; (* darwin9 / partial declaration *)
		"d_ino"]; (* freebsd7 / renaming field of struct in the other file *)
	"expat.h", [
		"XML_STATUS_ERROR"; (* conflicated with enum element *)
		"XML_STATUS_OK"; (* conflicated with enum element *)
		"XML_STATUS_SUSPENDED"]; (* conflicated with enum element *)
	"expat_external.h", [
		"XMLPARSEAPI"]; (* expat / storage class and parameterized type *)
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
		"__GMPN_ADD"; (* GMP / partial function *)
		"__GMPN_ADD_1"; (* GMP / multi-statements *)
		"__GMPN_AORS"; (* GMP / partial function *)
		"__GMPN_AORS_1"; (* GMP / multi-statements *)
		"__GMPN_SUB"; (* GMP / partial function *)
		"__GMPN_SUB_1"; (* GMP / multi-statements *)
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
		"__GMPZ_FITS_UTYPE_P"; (* GMP / multi-statements *)
		"mpf_inp_str"; (* GMP / #include <stdio.h> *)
		"mpf_out_str"; (* GMP / #include <stdio.h> *)
		"mpq_inp_str"; (* GMP / #include <stdio.h> *)
		"mpq_out_str"; (* GMP / #include <stdio.h> *)
		"mpz_inp_raw"; (* GMP / #include <stdio.h> *)
		"mpz_inp_str"; (* GMP / #include <stdio.h> *)
		"mpz_out_raw"; (* GMP / #include <stdio.h> *)
		"mpz_out_str"]; (* GMP / #include <stdio.h> *)
	"_mingw.h", [
		"__MINGW_ATTRIB_NONNULL"; (* mingw32 / parameterized attribute *)
		"__UNUSED_PARAM"; (* mingw32 / parameterized attribute *)
		"__USE_MINGW_ANSI_STDIO"]; (* mingw32 / __MINGW_FEATURES__ was undefined *)
	"mpc.h", [
		"mpfr_exp_t"; (* MPC / conflicted with typedef *)
		"__MPC_PROTO"]; (* MPC / parameter list *)
	"mpfr.h", [
		"__mpfr_default_fp_bit_precision"; (* MPFR / bug? mpfr_get_default_fp_bit_precision was undefined *)
		"_MPFR_PROTO"]; (* MPFR / parameter list *)
	"objbase.h", [
		"WINOLEAPI"; (* mingw32 / storage class and type *)
		"WINOLEAPI_"]; (* mingw32 / storage class and parameterized type *)
	"oleauto.h", [
		"WINOLEAUTAPI"; (* mingw32 / storage class and type *)
		"WINOLEAUTAPI_"]; (* mingw32 / storage class and parameterized type *)
	"opensslconf.h", [
		"OPENSSL_UNISTD"]; (* OpenSSL / header *)
	"param.h", [
		"CBSIZE"; (* freebsd7 / cblock was undefined, #include <sys/clist.h> *)
		"MID_MACHINE"; (* freebsd7 / MID_I386 was undefined, #include <sys/imgact_aout.h> *)
		"NPDEPG"; (* freebsd7 / pd_entry_t was undefined, #include <machine/pmap.h> *)
		"NPDEPTD"; (* freebsd7 / pd_entry_t was undefined, #include <machine/pmap.h> *)
		"NPTEPG"]; (* freebsd7 / pd_entry_t was undefined, #include <machine/pmap.h> *)
	"pngconf.h", [
		"PNG_ABORT"; (* libpng / abort was undefined, #include <stdlib.h> *)
		"png_benign_error"; (* libpng / circular dependency *)
		"png_chunk_benign_error"; (* libpng / circular dependency *)
		"PNG_EXPORT"]; (* libpng / parameterized declaration *)
	"pthread.h", [
		"pthread_cleanup_pop"; (* darwin9 / partial statement *)
		"pthread_cleanup_push"]; (* darwin9 / partial statement *)
	"rpcdce.h", [
		"RPC_C_SECURITY_QOS_VERSION"; (* mingw32 / L ?? *)
		"_SEC_WINNT_AUTH_IDENTITY"]; (* mingw32 / alias of struct tag was unsupported *)
	"rpcndr.h", [
		"MIDL_INTERFACE"; (* mingw32 / reserved word *)
		"NdrFieldPad"; (* mingw32 / generic, field *)
		"NdrFieldOffset"; (* mingw32 / generic, field *)
		"NdrMarshSCtxtHdl"; (* mingw32 / bug? not enough "" *)
		"NdrUnMarshSCtxtHdl"]; (* mingw32 / bug? misspell of N*DR*SContextUnMarshall *)
	"select.h", [
		"FD_COPY"]; (* darwin9 / bcopy was undefined, #include <string.h> *)
	"stat.h", [
		"__DARWIN_STRUCT_STAT64"; (* darwin9 / partial declaration *)
		"__DARWIN_STRUCT_STAT64_TIMES"]; (* darwin9 / partial declaration *)
	"stddef.h", [
		"offsetof"]; (* darwin9 / generic, field *)
	"stdint.h", [
		"INTMAX_C"; (* mingw32 / ## *)
		"UINTMAX_C"]; (* mingw32 / ## *)
	"_structs.h", [
		"__DARWIN_FD_COPY"; (* darwin9 / bcopy was undefined, #include <string.h> *)
		"I386_MCONTEXT_SIZE"]; (* darwin9 / struct mcontext was undefined *)
	"time.h", [
		"FD_COPY"; (* darwin9 / bcopy was undefined, #include <string.h> *)
		"timercmp"; (* darwin9 / parameterized operator *)
		"timevalcmp"]; (* darwin9 / parameterized operator *)
	"tgmath.h", [
		"acos"; (* gcc / magic macro *)
		"acosh"; (* gcc / magic macro *)
		"asin"; (* gcc / magic macro *)
		"asinh"; (* gcc / magic macro *)
		"atan"; (* gcc / magic macro *)
		"atan2"; (* gcc / magic macro *)
		"atanh"; (* gcc / magic macro *)
		"carg"; (* gcc / magic macro *)
		"cbrt"; (* gcc / magic macro *)
		"ceil"; (* gcc / magic macro *)
		"cimag"; (* gcc / magic macro *)
		"conj"; (* gcc / magic macro *)
		"copysign"; (* gcc / magic macro *)
		"cos"; (* gcc / magic macro *)
		"cosh"; (* gcc / magic macro *)
		"cproj"; (* gcc / magic macro *)
		"creal"; (* gcc / magic macro *)
		"erf"; (* gcc / magic macro *)
		"erfc"; (* gcc / magic macro *)
		"exp"; (* gcc / magic macro *)
		"exp2"; (* gcc / magic macro *)
		"expm1"; (* gcc / magic macro *)
		"fabs"; (* gcc / magic macro *)
		"fdim"; (* gcc / magic macro *)
		"floor"; (* gcc / magic macro *)
		"fma"; (* gcc / magic macro *)
		"fmax"; (* gcc / magic macro *)
		"fmin"; (* gcc / magic macro *)
		"fmod"; (* gcc / magic macro *)
		"frexp"; (* gcc / magic macro *)
		"hypot"; (* gcc / magic macro *)
		"ilogb"; (* gcc / magic macro *)
		"ldexp"; (* gcc / magic macro *)
		"lgamma"; (* gcc / magic macro *)
		"llrint"; (* gcc / magic macro *)
		"llround"; (* gcc / magic macro *)
		"log"; (* gcc / magic macro *)
		"log10"; (* gcc / magic macro *)
		"log1p"; (* gcc / magic macro *)
		"log2"; (* gcc / magic macro *)
		"logb"; (* gcc / magic macro *)
		"lrint"; (* gcc / magic macro *)
		"lround"; (* gcc / magic macro *)
		"nearbyint"; (* gcc / magic macro *)
		"nextafter"; (* gcc / magic macro *)
		"nexttoward"; (* gcc / magic macro *)
		"pow"; (* gcc / magic macro *)
		"remainder"; (* gcc / magic macro *)
		"remquo"; (* gcc / magic macro *)
		"rint"; (* gcc / magic macro *)
		"round"; (* gcc / magic macro *)
		"scalbln"; (* gcc / magic macro *)
		"scalbn"; (* gcc / magic macro *)
		"sin"; (* gcc / magic macro *)
		"sinh"; (* gcc / magic macro *)
		"sqrt"; (* gcc / magic macro *)
		"tan"; (* gcc / magic macro *)
		"tanh"; (* gcc / magic macro *)
		"__tg_choose"; (* gcc / magic macro *)
		"__tg_choose_2"; (* gcc / magic macro *)
		"__tg_choose_3"; (* gcc / magic macro *)
		"__tg_cplx"; (* gcc / magic macro *)
		"__tg_dbl"; (* gcc / magic macro *)
		"__tg_ldbl"; (* gcc / magic macro *)
		"tgamma"; (* gcc / magic macro *)
		"trunc"]; (* gcc / magic macro *)
	"trap.h", [
		"ILL_RESAD_FAULT"; (* freebsd7 / T_RESADFLT was undefined *)
		"ILL_RESOP_FAULT"]; (* freebsd7 / T_RESOPFLT was undefined *)
	"types.h", [
		"FD_COPY"; (* darwin9 / bcopy was undefined, #include <string.h> *)
		"__offsetof"]; (* darwin9 / generic, field *)
	"winbase.h", [
		"GetEnvironmentStrings"; (* mingw32 / duplicated by macro and normal function *)
		"GetVolumeNameForVolumeMountPoint"; (* mingw32 / out of #ifdef *)
		"GetVolumePathName"; (* mingw32 / out of #ifdef *)
		"GetVolumePathNamesForVolumeName"; (* mingw32 / out of #ifdef *)
		"DefineHandleTable"; (* mingw32 / parameter list *)
		"SetVolumeMountPoint"]; (* mingw32 / out of #ifdef *)
	"windef.h", [
		"DECLARE_STDCALL_P"; (* mingw32 / generic declaration-specifiers *)
		"_declspec"]; (* mingw32 / parameterized attribute *)
	"winnt.h", [
		"C_ASSERT"; (* mingw32 / static assert *)
		"CONTAINING_RECORD"; (* mingw32 / generic, field *)
		"DECLSPEC_ALIGN"; (* mingw32 / parameterized attribute *)
		"FIELD_OFFSET"; (* mingw32 / generic, field *)
		"_SLIST_ENTRY"; (* mingw32 / alias of struct tag was unsupported *)
		"TEXT"]; (* mingw32 / ## *)
	"winsock2.h", [
		"FD_SET"; (* mingw32 / conflicted with typedef *)
		"h_addr"; (* mingw32 / alias of element and dereferencing was unsupported *)
		"timercmp"]; (* mingw32 / parameterized operator *)
	"winspool.h", [
		"DeletePrintProcessor"; (* mingw32 / bug? misspell of DeletePrint*er*ProcessorW *)
		"DeletePrintProvidor"; (* mingw32 / bug? misspell of DeletePrint*er*ProviderW *)
		"GetDefaultPrinter"]; (* mingw32 / out of #ifdef *)
	"winuser.h", [
		"BroadcastSystemMessage"; (* mingw32 / bug? declarated as normal function *)
		"BroadcastSystemMessageEx"; (* mingw32 / out of #ifdef *)
		"DC_CAPTION"; (* mingw32 / out of #ifdef *)
		"DC_NC"; (* mingw32 / out of #ifdef *)
		"DEKSTOPENUMPROC"; (* mingw32 / bug? misspell of DE*SK*TOPENUMPROCW *)
		"EnumDisplaySettingsEx"; (* mingw32 / out of #ifdef *)
		"GetRawInputDeviceInfo"; (* mingw32 / out of #ifdef *)
		"RegisterDeviceNotification"; (* mingw32 / out of #ifdef *)
		"WHEEL_PAGESCROLL"]; (* mingw32 / UINT_MAX was undefined, #include <limits.h> *)
	predefined_name, [
		"__declspec"; (* mingw32 / parameterized attribute *)
		"__USER_LABEL_PREFIX__"]];; (* darwin9 / single "_" ...? *)

let is_known_define_parser_error = setmap_mem ~set:known_define_parser_errors;;
