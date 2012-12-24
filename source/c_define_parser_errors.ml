open Known_errors;;
open Position;;

module StringSet = StringSet;; (* for C_define_parser *)
module StringMap = StringMap;; (* for C_define_parser *)

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
		"cairo_set_target_glitz";
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
		"__ASMNAME"; (* linux/ # *)
		"__ASMNAME2"; (* linux / # *)
		"__attribute_format_arg__"; (* linux / parameterized attribute *)
		"__attribute_format_strfmon__"; (* linux / parameterized attribute *)
		"__CAST_AWAY_QUALIFIER"; (* darwin10 / parameterized type qualifier *)
		"__CONCAT"; (* freebsd7 / ## *)
		"__COPYRIGHT"; (* darwin9 / generic declaration *)
		"__DARWIN_NO_LONG_LONG"; (* darwin9 / "defined" used out of preprocessor *)
		"__DECONST"; (* freebsd7 / generic declaration *)
		"__DEQUALIFY"; (* freebsd7 / generic declaration *)
		"__DEVOLATILE"; (* freebsd7 / generic declaration *)
		"__errordecl"; (* linux / parameterized attribute *)
		"__extern_always_inline"; (* linux / __inline and attributes *)
		"__FBSDID"; (* freebsd7 / generic declaration *)
		"__flexarr"; (* linux / [] *)
		"__format_arg"; (* freebsd7 / parameterized attribute *)
		"__IDSTRING"; (* darwin9 / generic declaration *)
		"__LDBL_REDIR"; (* linux / parameterized partial declaration *)
		"__LDBL_REDIR_NTH"; (* linux / parameterized partial declaration *)
		"__LDBL_REDIR1"; (* linux / parameterized partial declaration *)
		"__LDBL_REDIR1_NTH"; (* linux / parameterized partial declaration *)
		"__NTH"; (* linux / parameterized attribute *)
		"__nonnull"; (* freebsd7 / parameterized attribute *)
		"__offsetof"; (* freebsd7 / parameterized field *)
		"__ptr_t"; (* linux / declaration specifier and pointer *)
		"__printflike"; (* darwin9 / parameterized attribute *)
		"__PROJECT_VERSION"; (* darwin9 / generic declaration *)
		"__rangeof"; (* freebsd7 / parameterized field *)
		"__RCSID"; (* darwin9 / generic declaration *)
		"__RCSID_SOURCE"; (* freebsd7 / generic declaration *)
		"__REDIRECT_LDBL"; (* linux / parameterized partial declaration *)
		"__REDIRECT_NTH_LDBL"; (* linux / parameterized partial declaration *)
		"__scanflike"; (* darwin9 / parameterized attribute *)
		"__SCCSID"; (* darwin9 / generic declaration *)
		"__section"; (* freebsd7 / parameterized attribute *)
		"__warnattr"; (* linux / parameterized declaration *)
		"__warndecl"]; (* linux / parameterized declaration *)
	"commdlg.h", [
		"CDSIZEOF_STRUCT"]; (* mingw-w64 / parameterized field *)
	"complex.h", [
		"complex"]; (* darwin9 / alias of _Complex *)
	"ctype.h", [
		"__chvalidchk"; (* mingw-w64 / _pctype is defined as expression *)
		"_chvalidchk_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_isalnum_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_isalpha_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_ischartype_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_iscntrl_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_iscsym_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_iscsymf_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_isdigit_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_isgraph_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_islower_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_isprint_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_ispunct_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_isspace_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_isupper_l"; (* mingw-w64 / _pctype is defined as expression *)
		"_isxdigit_l"; (* mingw-w64 / _pctype is defined as expression *)
		"__PCTYPE_FUNC"; (* mingw-w64 / _pctype is defined as expression *)
		"__pctype_func"; (* mingw-w64 / _pctype is defined as expression *)
		"__pwctype_func"]; (* mingw-w64 / _pctype is defined as expression *)
	"dirent.h", [
		"__DARWIN_STRUCT_DIRENTRY"; (* darwin9 / partial declaration *)
		"d_ino"]; (* freebsd7 / renaming field of struct in the other file *)
	"excpt.h", [
		"__except1"; (* mingw-w64 / extra semicolon *)
		"exception_info"; (* mingw-w64 / alias of function and cast *)
		"GetExceptionInformation"]; (* mingw-w64 / alias of function and cast *)
	"expat.h", [
		"XML_STATUS_ERROR"; (* conflicated with enum element *)
		"XML_STATUS_OK"; (* conflicated with enum element *)
		"XML_STATUS_SUSPENDED"]; (* conflicated with enum element *)
	"expat_external.h", [
		"XMLPARSEAPI"]; (* expat / storage class and parameterized type *)
	"gc.h", [
		"GC_EXTRAS"; (* Boehm-GC / parameter list *)
		"GC_EXTRA_PARAMS"]; (* Boehm-GC / formal parameter list *)
	"gc_config_macros.h", [
		"GC_ATTR_ALLOC_SIZE"]; (* Boehm-GC / parameterized attribute *)
	"gc_typed.h", [
		"GC_WORD_OFFSET"]; (* Boehm-GC / parameterized field *)
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
	"guiddef.h", [
		"CLSID_NULL"; (* mingw-w64 / GUID_NULL is undefined on WIN32_LEAN_AND_MEAN mode *)
		"FMTID_NULL"; (* mingw-w64 / GUID_NULL is undefined on WIN32_LEAN_AND_MEAN mode *)
		"IID_NULL"; (* mingw-w64 / GUID_NULL is undefined on WIN32_LEAN_AND_MEAN mode *)
		"REFCLSID"; (* mingw-w64 / declaration specifier and pointer *)
		"REFFMTID"; (* mingw-w64 / declaration specifier and pointer *)
		"REFGUID"; (* mingw-w64 / declaration specifier and pointer *)
		"REFIID"; (* mingw-w64 / declaration specifier and pointer *)
		"DEFINE_GUID"; (* mingw-w64 / parameterized declaration *)
		"DEFINE_OLEGUID"]; (* mingw-w64 / parameterized declaration *)
	"host_special_ports.h", [
		"host_get_amfid_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_audit_control_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_automountd_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_chud_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_dynamic_pager_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_host_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_host_priv_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_io_master_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_kextd_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_lockd_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_unfreed_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_get_user_notification_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_amfid_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_audit_control_port"; (* darwin9 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_automountd_port";  (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_chud_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_dynamic_pager_port"; (* darwin9 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_kextd_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_lockd_port"; (* darwin9 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_unfreed_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"host_set_user_notification_port"]; (* darwin9 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
	"_ip_types.h", [
		"h_addr"]; (* mingw-w64 / alias of element and dereferencing *)
	"in.h", [
		"CTL_IPPROTO_NAMES"; (* freebsd7 / partial initializer *)
		"IPCTL_NAMES"]; (* freebsd7 / partial initializer *)
	"in6.h", [
		"IN6_ARE_ADDR_EQUAL"; (* darwin9 / memcmp is undefined, #include <string.h> *)
		"M_AUTHIPDGM"; (* freebsd7 / M_PROTO5 was undefined *)
		"M_AUTHIPHDR"; (* freebsd7 / M_PROTO2 was undefined *)
		"M_DECRYPTED"; (* freebsd7 / M_PROTO3 was undefined *)
		"M_LOOP"]; (* freebsd7 / M_PROTO4 was undefined *)
	"malloc.h", [
		"_STATIC_ASSERT"]; (* mingw-w64 / parameterized declaration *)
	"memory_object_types.h", [
		"invalid_memory_object_flavor"]; (* darwin9 / OLD_MEMORY_OBJECT_BEHAVIOR_INFO and OLD_MEMORY_OBJECT_ATTRIBUTE_INFO are undefined *)
	"_mingw.h", [
		"_CRT_ALIGN"; (* mingw-w64 / parameterized attribute *)
		"_CRT_DEPRECATE_TEXT"; (* mingw-w64 / parameterized attribute *)
		"_CRT_glob"; (* mingw-w64 / _dowildcard is undefined *)
		"_CRT_WIDE"; (* mingw-w64 / ## *)
		"__MINGW_ATTRIB_NONNULL"; (* mingw32 / parameterized attribute *)
		"__MINGW_BROKEN_INTERFACE"; (* mingw-w64 / parameterized pragma *)
		"__restrict_arr"; (* mingw32 / reserved word *)
		"__UNUSED_PARAM"; (* mingw32 / parameterized attribute *)
		"__USE_MINGW_ANSI_STDIO"]; (* mingw32 / __MINGW_FEATURES__ was undefined *)
	"_mingw_mac.h", [
		"__CRT_SECURE_CPP_OVERLOAD_SECURE_NAMES_0_2_"; (* mingw-w64 / parameterized declaration *)
		"__CRT_SECURE_CPP_OVERLOAD_SECURE_NAMES_MEMORY_0_3_"; (* mingw-w64 / parameterized declaration *)
		"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_0_2_"; (* mingw-w64 / parameterized declaration *)
		"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT_0_3_"; (* mingw-w64 / parameterized declaration *)
		"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT_1_4_"; (* mingw-w64 / parameterized declaration *)
		"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_MEMORY_0_3_"; (* mingw-w64 / parameterized declaration *)
		"__MINGW_CRT_NAME_CONCAT1"]; (* mingw-w64 / ??? *)
	"_mingw_secapi.h", [
		"__CRT_SECURE_CPP_OVERLOAD_SECURE_NAMES_0_2_"; (* mingw-w64 / parameterized declaraton *)
		"__CRT_SECURE_CPP_OVERLOAD_SECURE_NAMES_MEMORY_0_3_"; (* mingw-w64 / parameterized declaraton *)
		"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_0_2_"; (* mingw-w64 / parameterized declaraton *)
		"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT_0_3_"; (* mingw-w64 / parameterized declaraton *)
		"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT_1_4_"; (* mingw-w64 / parameterized declaraton *)
		"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_MEMORY_0_3_"; (* mingw-w64 / parameterized declaraton *)
		"__MINGW_CRT_NAME_CONCAT1"]; (* mingw-w64 / :: *)
	"_mingw_unicode.h", [
		"__MINGW_TYPEDEF_AW"; (* mingw-w64 / parameterized declaration *)
		"__MINGW_TYPEDEF_UAW"]; (* mingw-w64 / parameterized declaration *)
	"mount.h", [
		"__DARWIN_STRUCT_STATFS64"]; (* darwin10 / partial declaration *)
	"mpc.h", [
		"mpfr_exp_t"; (* MPC / conflicted with typedef *)
		"__MPC_PROTO"]; (* MPC / parameter list *)
	"mpfr.h", [
		"__mpfr_default_fp_bit_precision"; (* MPFR / bug? mpfr_get_default_fp_bit_precision was undefined *)
		"_MPFR_PROTO"]; (* MPFR / parameter list *)
	"netdb.h", [
		"h_addr"]; (* darwin9 / alias of element and dereferencing *)
	"objbase.h", [
		"DECLARE_INTERFACE_"; (* mingw-w64 / parameterized type *)
		"DECLARE_INTERFACE_IID_"; (* mingw-w64 / parameterized type *)
		"IFACEMETHOD"; (* mingw-w64 / parameterized declaration *)
		"IFACEMETHOD_"; (* mingw-w64 / parameterized declaration *)
		"IFACEMETHODV"; (* mingw-w64 / parameterized declaration *)
		"IFACEMETHODV_"; (* mingw-w64 / parameterized declaration *)
		"STDMETHOD"; (* mingw-w64 / parameterized declaration *)
		"STDMETHOD_"; (* mingw-w64 / parameterized declaration *)
		"STDMETHODV"; (* mingw-w64 / parameterized declaration *)
		"STDMETHODV_"; (* mingw-w64 / parameterized declaration *)
		"THIS"; (* mingw-w64 / parameter list *)
		"THIS_"; (* mingw-w64 / parameter list *)
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
	"png.h", [
		"PNG_GAMMA_THRESHOLD"; (* libpng / PNG_GAMMA_THRESHOLD_FIXED is undefined *)
		"PNG_READ_16_TO_8"]; (* libpng / SUPPORTED is undefined *)
	"pngconf.h", [
		"png_benign_error"; (* libpng / circular dependency *)
		"PNG_CALLBACK"; (* libpng / parameterized declaration *)
		"png_chunk_benign_error"; (* libpng / circular dependency *)
		"PNG_EXPORT"; (* libpng / parameterized declaration *)
		"PNG_EXPORTA"; (* libpng / parameterized declaration *)
		"PNG_FIXED_EXPORT"; (* libpng / parameterized declaration *)
		"PNG_FP_EXPORT"; (* libpng / parameterized declaration *)
		"PNG_FUNCTION"; (* libpng / parameterized declaration *)
		"png_sprintf"; (* libpng / sprintf is undefined, #include <stdio.h> *)
		"png_snprintf"; (* libpng / snprintf is undefined, #include <stdio.h> *)
		"png_snprintf2"; (* libpng / snprintf is undefined, #include <stdio.h> *)
		"png_snprintf6"]; (* libpng / snprintf is undefined, #include <stdio.h> *)
	"propidl.h", [
		"REFPROPVARIANT"];  (* mingw-w64 / declaration specifier and pointer *)
	"prsht.h", [
		"CCSIZEOF_STRUCT"; (* mingw-w64 / parameterized field *)
		"PROPSHEETPAGEA_V1_FIELDS"; (* mingw-w64 / partial declaration *)
		"PROPSHEETPAGEW_V1_FIELDS"; (* mingw-w64 / partial declaration *)
		"PROPSHEETHEADER_V2_SIZE"; (* mingw-w64 / does not have DUMMYUNION5_MEMBER *)
		"PROPSHEETHEADERA_V2_SIZE"; (* mingw-w64 / does not have DUMMYUNION5_MEMBER *)
		"PROPSHEETHEADERW_V2_SIZE"]; (* mingw-w64 / does not have DUMMYUNION5_MEMBER *)
	"pthread.h", [
		"pthread_cleanup_pop"; (* darwin9 / partial statement *)
		"pthread_cleanup_push"]; (* darwin9 / partial statement *)
	"queue.h", [
		"CIRCLEQ_EMPTY"; (* darwin10 / many bad macros... *)
		"CIRCLEQ_ENTRY";
		"CIRCLEQ_FIRST";
		"CIRCLEQ_FOREACH";
		"CIRCLEQ_HEAD";
		"CIRCLEQ_INIT";
		"CIRCLEQ_INSERT_AFTER";
		"CIRCLEQ_INSERT_BEFORE";
		"CIRCLEQ_INSERT_HEAD";
		"CIRCLEQ_INSERT_TAIL";
		"CIRCLEQ_LAST";
		"CIRCLEQ_NEXT";
		"CIRCLEQ_PREV";
		"CIRCLEQ_REMOVE";
		"LIST_EMPTY";
		"LIST_ENTRY";
		"LIST_FIRST";
		"LIST_FOREACH";
		"LIST_FOREACH_SAFE";
		"LIST_HEAD";
		"LIST_HEAD_INITIALIZER";
		"LIST_INIT";
		"LIST_INSERT_AFTER";
		"LIST_INSERT_BEFORE";
		"LIST_INSERT_HEAD";
		"LIST_NEXT";
		"LIST_REMOVE";
		"SLIST_EMPTY";
		"SLIST_ENTRY";
		"SLIST_FIRST";
		"SLIST_FOREACH";
		"SLIST_FOREACH_PREVPTR";
		"SLIST_FOREACH_SAFE";
		"SLIST_HEAD";
		"SLIST_HEAD_INITIALIZER";
		"SLIST_INIT";
		"SLIST_INSERT_AFTER";
		"SLIST_INSERT_HEAD";
		"SLIST_NEXT";
		"SLIST_REMOVE";
		"SLIST_REMOVE_HEAD";
		"STAILQ_CONCAT";
		"STAILQ_EMPTY";
		"STAILQ_ENTRY";
		"STAILQ_FIRST";
		"STAILQ_FOREACH";
		"STAILQ_FOREACH_SAFE";
		"STAILQ_HEAD";
		"STAILQ_HEAD_INITIALIZER";
		"STAILQ_INIT";
		"STAILQ_INSERT_AFTER";
		"STAILQ_INSERT_HEAD";
		"STAILQ_INSERT_TAIL";
		"STAILQ_LAST";
		"STAILQ_NEXT";
		"STAILQ_REMOVE";
		"STAILQ_REMOVE_HEAD";
		"STAILQ_REMOVE_HEAD_UNTIL";
		"TAILQ_CONCAT";
		"TAILQ_EMPTY";
		"TAILQ_ENTRY";
		"TAILQ_FIRST";
		"TAILQ_FOREACH";
		"TAILQ_FOREACH_REVERSE";
		"TAILQ_FOREACH_REVERSE_SAFE";
		"TAILQ_FOREACH_SAFE";
		"TAILQ_HEAD";
		"TAILQ_HEAD_INITIALIZER";
		"TAILQ_INIT";
		"TAILQ_INSERT_AFTER";
		"TAILQ_INSERT_BEFORE";
		"TAILQ_INSERT_HEAD";
		"TAILQ_INSERT_TAIL";
		"TAILQ_LAST";
		"TAILQ_NEXT";
		"TAILQ_PREV";
		"TAILQ_REMOVE"];
	"rpc.h", [
		"interface"; (* mingw-w64 / reserved word *)
		"RpcEndFinally"; (* mingw-w64 / partial statement *)
		"RpcEndExcept"; (* mingw-w64 / partial statement *)
		"RpcExcept"; (* mingw-w64 / partial statement *)
		"RpcFinally"; (* mingw-w64 / partial statement *)
		"RpcTryExcept"; (* mingw-w64 / partial statement *)
		"RpcTryFinally"]; (* mingw-w64 / partial statement *)
	"rpcdce.h", [
		"RPC_C_SECURITY_QOS_VERSION"]; (* mingw32 / L ?? *)
	"rpcndr.h", [
		"EXTERN_GUID"; (* mingw-w64 / parameterized declaration *)
		"MIDL_INTERFACE"; (* mingw32 / reserved word *)
		"NdrFieldPad"; (* mingw32 / parameterized field *)
		"NdrFieldOffset"; (* mingw32 / parameterized field *)
		"NdrMarshSCtxtHdl"; (* mingw32 / bug? not enough "" *)
		"NdrUnMarshSCtxtHdl"]; (* mingw32 / bug? misspell of N*DR*SContextUnMarshall *)
	"sdkddkver.h", [
		"NTDDI_VERSION_FROM_WIN32_WINNT"]; (* mingw-w64 / ## *)
	"select.h", [
		"FD_COPY"]; (* darwin9 / bcopy was undefined, #include <string.h> *)
	"shellapi.h", [
		"SHDOCAPI"; (* mingw-w64 / storage class and type *)
		"SHDOCAPI_"; (* mingw-w64 / storage class and parameterized type *)
		"SHSTDAPI"; (* mingw-w64 / storage class and type *)
		"SHSTDAPI_"]; (* mingw-w64 / storage class and parameterized type *)
	"socket.h", [
		"CTL_NET_NAMES"; (* freebsd7 / partial initializer *)
		"CTL_NET_RT_NAMES"]; (* freebsd7 / partial initializer *)
	"stat.h", [
		"__DARWIN_STRUCT_STAT64"; (* darwin9 / partial declaration *)
		"__DARWIN_STRUCT_STAT64_TIMES"]; (* darwin9 / partial declaration *)
	"stddef.h", [
		"offsetof"]; (* darwin9 / parameterized field *)
	"stdint.h", [
		"INTMAX_C"; (* mingw32 / ## *)
		"UINTMAX_C"]; (* mingw32 / ## *)
	"stdio.h", [
		"__MINGW_PRINTF_FORMAT"; (* mingw-w64 / ms_printf is undefined *)
		"__MINGW_SCANF_FORMAT"]; (* mingw-w64 / ms_scanf is undefined *)
	"_stdio.h", [
		"snprintf"; (* darwin10 / varargs macro *)
		"sprintf"]; (* darwin10 / varargs macro *)
	"stralign.h", [
		"__UA_STACKCOPY"]; (* mingw-w64 / _alloca is undefined *)
	"_structs.h", [
		"__DARWIN_FD_COPY"; (* darwin9 / bcopy was undefined, #include <string.h> *)
		"I386_MCONTEXT_SIZE"]; (* darwin9 / struct mcontext was undefined *)
	"task_special_ports.h", [
		"task_get_automountd_port"; (* darwin9 / task_get_special_port is undefined, #include <mach/*/task.h> *)
		"task_get_bootstrap_port"; (* darwin9 / task_get_special_port is undefined, #include <mach/*/task.h> *)
		"task_get_gssd_port"; (* darwin9 / task_get_special_port is undefined, #include <mach/*/task.h> *)
		"task_get_host_port"; (* darwin9 / task_get_special_port is undefined, #include <mach/*/task.h> *)
		"task_get_kernel_port"; (* darwin9 / task_get_special_port is undefined, #include <mach/*/task.h> *)
		"task_get_paged_ledger_port"; (* darwin9 / task_get_special_port is undefined, #include <mach/*/task.h> *)
		"task_get_task_access_port"; (* darwin9 / task_get_special_port is undefined, #include <mach/*/task.h> *)
		"task_get_wired_ledger_port"; (* darwin9 / task_get_special_port is undefined, #include <mach/*/task.h> *)
		"task_set_automountd_port"; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
		"task_set_bootstrap_port"; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
		"task_set_gssd_port"; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
		"task_set_host_port"; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
		"task_set_kernel_port"; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
		"task_set_paged_ledger_port"; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
		"task_set_task_access_port"; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
		"task_set_wired_ledger_port"]; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
	"thread_special_ports.h", [
		"thread_get_kernel_port"; (* darwin9 / thread_get_special_port is undefined, #include <mach/*/thread_act.h> *)
		"thread_set_kernel_port"]; (* darwin9 / thread_set_special_port is undefined, #include <mach/*/thread_act.h> *)
	"time.h", [
		"FD_COPY"; (* darwin9 / bcopy was undefined, #include <string.h> *)
		"timercmp"; (* darwin9 / parameterized operator *)
		"timevalcmp"]; (* darwin9 / parameterized operator *)
	"_timeval.h", [
		"timercmp"]; (* mingw-w64 / parameterized operator *)
	"time_value.h", [
		"time_value_add"; (* darwin9 / bad form, it shold use do ... while(0) *)
		"time_value_add_usec"]; (* darwin9 / bad form, it shold use do ... while(0) *)
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
		"__offsetof"]; (* darwin9 / parameterized field *)
	"_types.h", [
		"__strfmonlike"; (* darwin10 / parameterized attribute *)
		"__strftimelike"]; (* darwin10 / parameterized attribute *)
	"ucred.h", [
		"cr_gid"]; (* darwin10 / alias of element and dereferencing *)
	"urlmon.h", [
		"LPOINETPROTOCOLSINKSTACKABLE"]; (* mingw-w64 / bug? misspell of LPIINTERNETPROTOCOLSINKStackable *)
	"vm_param.h", [
		"i386_btop"; (* darwin9 / pmap_paddr_t is undefined *)
		"i386_ptob"; (* darwin9 / pmap_paddr_t is undefined *)
		"i386_round_page"; (* darwin9 / pmap_paddr_t is undefined *)
		"i386_trunc_page"; (* darwin9 / pmap_paddr_t is undefined *)
		"machine_btop"]; (* darwin9 / pmap_paddr_t is undefined *)
	"vm_types.h", [
		"MACH_MSG_TYPE_INTEGER_T"]; (* darwin9 / MACH_MSG_TYPE_INTEGER_32 is undefined *)
	"winbase.h", [
		"EXCEPTION_POSSIBLE_DEADLOCK"; (* mingw-w64 / STATUS_POSSIBLE_DEADLOCK is undefined *)
		"GetVolumeNameForVolumeMountPoint"; (* mingw32 / out of #ifdef *)
		"GetVolumePathName"; (* mingw32 / out of #ifdef *)
		"GetVolumePathNamesForVolumeName"; (* mingw32 / out of #ifdef *)
		"DefineHandleTable"; (* mingw32 / parameter list *)
		"SetVolumeMountPoint"]; (* mingw32 / out of #ifdef *)
	"windef.h", [
		"DECLARE_STDCALL_P"; (* mingw32 / generic declaration-specifiers *)
		"_declspec"]; (* mingw32 / parameterized attribute *)
	"winerror.h", [
		"GetScode"; (* mingw-w64 / SCODE is undefined on WIN32_LEAN_AND_MEAN mode *)
		"MAKE_SCODE"]; (* mingw-w64 / SCODE is undefined on WIN32_LEAN_AND_MEAN mode *)
	"winnt.h", [
		"BitScanForward"; (* mingw-w64 / _BitScanForward is undefined on 32 bit mode *)
		"BitScanReverse"; (* mingw-w64 / _BitScanReverse is undefined on 32 bit mode *)
		"BitTest"; (* mingw-w64 / _bittest is undefined on 32 bit mode *)
		"BitTestAndComplement"; (* mingw-w64 / _bittestandcomplement is undefined on 32 bit mode *)
		"BitTestAndReset"; (* mingw-w64 / _bittestandreset is undefined on 32 bit mode *)
		"BitTestAndSet"; (* mingw-w64 / _bittestandset is undefined on 32 bit mode *)
		"C_ASSERT"; (* mingw32 / static assert *)
		"CONTAINING_RECORD"; (* mingw32 / parameterized field *)
		"DECLSPEC_ALIGN"; (* mingw32 / parameterized attribute *)
		"FIELD_OFFSET"; (* mingw32 / parameterized field *)
		"IFACEMETHODIMP_"; (* mingw-w64 / storage class and parameterized type *)
		"IFACEMETHODIMPV_"; (* mingw-w64 / storage class and parameterized type *)
		"InterlockedCompareExchange16"; (* mingw-w64 / _InterlockedCompareExchange16 is undefined on 32 bit mode *)
		"InterlockedDecrement16"; (* mingw-w64 / _InterlockedDecrement16 is undefined on 32 bit mode *)
		"InterlockedIncrement16"; (* mingw-w64 / _InterlockedIncrement16 is undefined on 32 bit mode *)
		"NOP_FUNCTION"; (* mingw-w64 / (void)0 ??? *)
		"PROBE_ALIGNMENT"; (* mingw-w64 / new struct in macro *)
		"REPARSE_GUID_DATA_BUFFER_HEADER_SIZE"; (* mingw-w64 / accessing element of null *)
		"RTL_BITS_OF_FIELD"; (* mingw-w64 / parameterized field *)
		"RTL_CONTAINS_FIELD"; (* mingw-w64 / parameterized field *)
		"RTL_FIELD_SIZE"; (* mingw-w64 / parameterized field *)
		"RTL_FIELD_TYPE"; (* mingw-w64 / parameterized field *)
		"RTL_NUMBER_OF_FIELD"; (* mingw-w64 / parameterized field *)
		"RTL_PADDING_BETWEEN_FIELDS"; (* mingw-w64 / parameterized field *)
		"RTL_SIZEOF_THROUGH_FIELD"; (* mingw-w64 / parameterized field *)
		"STDAPI"; (* mingw-w64 / storage class and type *)
		"STDAPI_"; (* mingw-w64 / storage class and parameterized type *)
		"STDAPIV"; (* mingw-w64 / storage class and type *)
		"STDAPIV_"; (* mingw-w64 / storage class and parameterized type *)
		"STDMETHODIMP_"; (* mingw-w64 / storage class and parameterized type *)
		"STDMETHODIMPV_"; (* mingw-w64 / storage class and parameterized type *)
		"TEXT"; (* mingw32 / ## *)
		"TYPE_ALIGNMENT"]; (* mingw-w64 / new struct in macro *)
	"winsock2.h", [
		"FD_SET"; (* mingw32 / conflicted with typedef *)
		"h_addr"; (* mingw32 / alias of element and dereferencing *)
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
	"xmlstring.h", [
		"BAD_CAST"]; (* libxml2 / typename with paren *)
	"xmlversion.h", [
		"ATTRIBUTE_ALLOC_SIZE"; (* libxml2 / parameterized attribute *)
		"ATTRIBUTE_PRINTF"; (* libxml2 / parameterized attribute *)
		"LIBXML_ATTR_ALLOC_SIZE"; (* libxml2 / parameterized attribute *)
		"LIBXML_ATTR_FORMAT"; (* libxml2 / parameterized attribute *)
		"LIBXML_TEST_VERSION"]; (* libxml2 / extra semicolon *)
	predefined_name, [
		"__declspec"; (* mingw32 / parameterized attribute *)
		"__USER_LABEL_PREFIX__"]];; (* darwin9 / single "_" ...? *)

let is_known_define_parser_error = setmap_mem ~set:known_define_parser_errors;;
