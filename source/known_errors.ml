open C_filename;;
open Position;;

let match_target ~(pattern: string) (target: string): bool = (
	let pattern_length = String.length pattern in
	let target_length = String.length target in
	let rec loop pattern_index target_index = (
		if pattern_index >= pattern_length then target_index >= target_length else
		begin match pattern.[pattern_index] with
		| '*' ->
			let next_pattern_index = pattern_index + 1 in
			if next_pattern_index = pattern_length then true else
			let next_c = pattern.[next_pattern_index] in
			assert (next_c <> '*');
			let rec find_loop target_index = (
				if target_index >= target_length then false else
				if target.[target_index] = next_c
					&& loop (next_pattern_index + 1) (target_index + 1)
				then (
					true
				) else (
					find_loop (target_index + 1)
				)
			) in
			find_loop target_index
		| _ as c ->
			if target_index >= target_length || target.[target_index] <> c then false else
			loop (pattern_index + 1) (target_index + 1)
		end
	) in
	loop 0 0
);;

type known_error = [
	(* for preprocessor *)
	| `undefined_macro
	| `redefine_macro
	| `redefine_compiler_macro
	| `redefine_extended_word
	| `redefine_preprocessor_word
	| `push_defined_macro
	(* for define parser *)
	| `unparsible_macro
	(* for define analyzer *)
	| `uninterpretable_macro];;

let known_error_table: (string * (string * (known_error * string list) list) list) list = [
	"*-apple-darwin*", [
		"AvailabilityMacros.h", [
			`undefined_macro, [
				"__AVAILABILITY_MACROS_USES_AVAILABILITY"; (* darwin13 *)
				"__i386__"; (* darwin10 *)
				"__ppc64__"]]; (* darwin10 *)
		"complex.h", [
			`undefined_macro, [
				"__WANT_LONG_DOUBLE_FORMAT__"]]; (* darwin10 *)
		"dirent.h", [
			`uninterpretable_macro, [
				"dirfd"]]; (* darwin9 / accessing element of untyped parameter *)
		"i386/_mcontext.h", [
			`uninterpretable_macro, [
				"I386_MCONTEXT_SIZE"]]; (* darwin13 / struct mcontext is undefined *)
		"i386/_structs.h", [
			`unparsible_macro, [
				"__DARWIN_FD_COPY"; (* darwin9 / bcopy was undefined, #include <string.h> *)
				"I386_MCONTEXT_SIZE"]]; (* darwin9 / struct mcontext was undefined *)
		"i386/types.h", [
			`undefined_macro, [
				"__LP64__"]; (* darwin9 *)
			`unparsible_macro, [
				"__offsetof"]]; (* darwin10 / parameterized field *)
		"mach/clock_types.h", [
			`uninterpretable_macro, [
				"ADD_MACH_TIMESPEC"; (* darwin9 / accessing element of untyped parameter *)
				"BAD_MACH_TIMESPEC"; (* darwin9 / accessing element of untyped parameter *)
				"CMP_MACH_TIMESPEC"; (* darwin9 / accessing element of untyped parameter *)
				"SUB_MACH_TIMESPEC"]]; (* darwin9 / accessing element of untyped parameter *)
		"mach/i386/vm_types.h", [
			`unparsible_macro, [
				"MACH_MSG_TYPE_INTEGER_T"]]; (* darwin9 / MACH_MSG_TYPE_INTEGER_32 is undefined *)
		"mach/i386/vm_param.h", [
			`unparsible_macro, [
				"i386_btop"; (* darwin9 / pmap_paddr_t is undefined *)
				"i386_ptob"; (* darwin9 / pmap_paddr_t is undefined *)
				"i386_round_page"; (* darwin9 / pmap_paddr_t is undefined *)
				"i386_trunc_page"; (* darwin9 / pmap_paddr_t is undefined *)
				"machine_btop"; (* darwin9 / pmap_paddr_t is undefined *)
				"machine_ptob"]]; (* darwin12 / pmap_paddr_t is undefined *)
		"mach/host_special_ports.h", [
			`unparsible_macro, [
				"host_get_amfid_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_audit_control_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_automountd_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_chud_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_dynamic_pager_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_gssd_port"; (* darwin12 / host_get_special_port is undefined *)
				"host_get_host_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_host_priv_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_io_master_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_kextd_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_lockd_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_telemetry_port"; (* darwin13 / host_get_special_port is undefined *)
				"host_get_unfreed_port"; (* darwin10 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_get_user_notification_port"; (* darwin9 / host_get_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_amfid_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_audit_control_port"; (* darwin9 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_automountd_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_chud_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_dynamic_pager_port"; (* darwin9 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_gssd_port"; (* darwin12 / host_set_special_port is undefined *)
				"host_set_kextd_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_lockd_port"; (* darwin9 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_telemetry_port"; (* darwin13 / host_set_special_port is undefined *)
				"host_set_unfreed_port"; (* darwin10 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
				"host_set_user_notification_port"]]; (* darwin9 / host_set_special_port is undefined, #include <mach/host_priv.h> *)
		"mach/kmod.h", [
			`undefined_macro, [
				"PRAGMA_MARK"]]; (* darwin10 *)
		"mach/memory_object_types.h", [
			`unparsible_macro, [
				"invalid_memory_object_flavor"]]; (* darwin9 / OLD_MEMORY_OBJECT_BEHAVIOR_INFO and OLD_MEMORY_OBJECT_ATTRIBUTE_INFO are undefined *)
		"mach/task_special_ports.h", [
			`unparsible_macro, [
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
				"task_set_wired_ledger_port"]]; (* darwin9 / task_set_special_port is undefined, #include <mach/*/task.h> *)
		"mach/thread_special_ports.h", [
			`unparsible_macro, [
				"thread_get_kernel_port"; (* darwin10 / thread_get_special_port is undefined, #include <mach/*/thread_act.h> *)
				"thread_set_kernel_port"]]; (* darwin10 / thread_set_special_port is undefined, #include <mach/*/thread_act.h> *)
		"mach/time_value.h", [
			`unparsible_macro, [
				"time_value_add"; (* darwin10 / bad form, it shold use do ... while(0) *)
				"time_value_add_usec"]]; (* darwin10 / bad form, it shold use do ... while(0) *)
		"mach/vm_region.h", [
			`undefined_macro, [
				"VM32_SUPPORT"]]; (* darwin10 *)
		"mach-o/dyld.h", [
			`undefined_macro, [
				"__cplusplus"]]; (* darwin10 *)
		"netdb.h", [
			`unparsible_macro, [
				"h_addr"]]; (* darwin9 / alias of element and dereferencing *)
		"netinet/in.h", [
			`undefined_macro, [
				"CONFIG_FORCE_OUT_IFP"]]; (* darwin9 *)
		"netinet6/in6.h", [
			`unparsible_macro, [
				"IN6_ARE_ADDR_EQUAL"]; (* darwin9 / memcmp is undefined, #include <string.h> *)
			`uninterpretable_macro, [
				"IN6_IS_ADDR_6TO4"; (* darwin11 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_LINKLOCAL"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_LOOPBACK"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_GLOBAL"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_LINKLOCAL"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_NODELOCAL"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_ORGLOCAL"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_SITELOCAL"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MULTICAST"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_SITELOCAL"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_UNIQUE_LOCAL"; (* darwin11 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_UNSPECIFIED"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_V4COMPAT"; (* darwin9 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_V4MAPPED"; (* darwin9 / accessing element of untyped parameter *)
				"__IPV6_ADDR_MC_SCOPE"]]; (* darwin9 / accessing element of untyped parameter *)
		"pthread.h", [
			`undefined_macro, [
				"__IPHONE_OS_VERSION_MIN_REQUIRED"]; (* darwin13 *)
			`unparsible_macro, [
				"pthread_cleanup_pop"; (* darwin10 / partial statement *)
				"pthread_cleanup_push"]]; (* darwin10 / partial statement *)
		"secure/_string.h", [
			`undefined_macro, [
				"__IPHONE_OS_VERSION_MIN_REQUIRED"]]; (* darwin13 *)
		"stdint.h", [
			`undefined_macro, [
				"__LP64__"]]; (* darwin9 *)
		"stdio.h", [
			`undefined_macro, [
				"_FORTIFY_SOURCE"]; (* darwin9 *)
			`uninterpretable_macro, [
				"clearerr_unlocked"; (* darwin9 / accessing element of untyped parameter *)
				"ferror_unlocked"; (* darwin9 / accessing element of untyped parameter *)
				"feof_unlocked"; (* darwin9 / accessing element of untyped parameter *)
				"fileno_unlocked"; (* darwin9 / accessing element of untyped parameter *)
				"__sclearerr"; (* darwin9 / accessing element of untyped parameter *)
				"__sfeof"; (* darwin9 / accessing element of untyped parameter *)
				"__sferror"; (* darwin9 / accessing element of untyped parameter *)
				"__sfileno"; (* darwin9 / accessing element of untyped parameter *)
				"__sgetc"]]; (* darwin9 / accessing element of untyped parameter *)
		"string.h", [
			`undefined_macro, [
				"_FORTIFY_SOURCE"]]; (* darwin9 *)
		"sys/cdefs.h", [
			`redefine_extended_word, [
				"__const"; (* darwin10 *)
				"__signed"; (* darwin10 *)
				"__restrict"]; (* darwin10 *)
			`unparsible_macro, [
				"__CAST_AWAY_QUALIFIER"; (* darwin10 / parameterized type qualifier in expression *)
				"__COPYRIGHT"; (* darwin9 / generic declaration *)
				"__DARWIN_NO_LONG_LONG"; (* darwin9 / "defined" used out of preprocessor *)
				"__IDSTRING"; (* darwin9 / generic declaration *)
				"__PROJECT_VERSION"; (* darwin9 / generic declaration *)
				"__RCSID"; (* darwin9 / generic declaration *)
				"__SCCSID"]]; (* darwin9 / generic declaration *)
		"sys/dirent.h", [
			`unparsible_macro, [
				"__DARWIN_STRUCT_DIRENTRY"]]; (* darwin9 / partial declaration *)
		"sys/event.h", [
			`undefined_macro, [
				"CONFIG_EMBEDDED"]]; (* darwin12 *)
		"sys/ioctl.h", [
			`undefined_macro, [
				"BSD_KERNEL_PRIVATE"; (* darwin10 *)
				"COMPAT_43"; (* darwin9 *)
				"COMPAT_43_TTY"]]; (* darwin9 *)
		"sys/mount.h", [
			`undefined_macro, [
				"COMPAT_GETFSSTAT"]; (* darwin9 *)
			`unparsible_macro, [
				"__DARWIN_STRUCT_STATFS64"]]; (* darwin10 / partial declaration *)
		"sys/param.h", [
			`unparsible_macro, [
				"CBSIZE"]]; (* darwin10 / cblock was undefined, #include <sys/clist.h> *)
		"sys/queue.h", [
			`unparsible_macro, [
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
				"LIST_SWAP"; (* darwin11 *)
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
				"SLIST_REMOVE_AFTER"; (* darwin11 *)
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
				"STAILQ_REMOVE_AFTER"; (* darwin11 *)
				"STAILQ_REMOVE_HEAD";
				"STAILQ_REMOVE_HEAD_UNTIL";
				"STAILQ_SWAP"; (* darwin11 *)
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
				"TAILQ_REMOVE";
				"TAILQ_SWAP"]]; (* darwin11 *)
		"sys/socket.h", [
			`uninterpretable_macro, [
				"CMSG_FIRSTHDR"; (* darwin9 / accessing element of untyped parameter *)
				"CMSG_NXTHDR"]]; (* darwin9 / accessing element of untyped parameter *)
		"sys/stat.h", [
			`unparsible_macro, [
				"__DARWIN_STRUCT_STAT64"; (* darwin10 / partial declaration *)
				"__DARWIN_STRUCT_STAT64_TIMES"]]; (* darwin10 / partial declaration *)
		"sys/_structs.h", [
			`unparsible_macro, [
				"__DARWIN_FD_COPY"]; (* darwin10 / bcopy was undefined, #include <string.h> *)
			`uninterpretable_macro, [
				"__DARWIN_FD_CLR"; (* darwin9 / accessing element of untyped parameter *)
				"__DARWIN_FD_SET"]]; (* darwin9 / accessing element of untyped parameter *)
		"sys/sysctl.h", [
			`undefined_macro, [
				"CONFIG_LCTX"; (* darwin9 *)
				"COUNT_SYSCALLS"]; (* darwin9 *)
			`unparsible_macro, [
				"CTL_NAMES"]]; (* darwin9 / partial initializer *)
		"sys/time.h", [
			`unparsible_macro, [
				"FD_COPY"; (* darwin9 / bcopy was undefined, #include <string.h> *)
				"timercmp"; (* darwin10 / parameterized operator *)
				"timevalcmp"]; (* darwin10 / parameterized operator *)
			`uninterpretable_macro, [
				"FD_CLR"; (* darwin9 / accessing element of untyped parameter *)
				"FD_SET"; (* darwin9 / accessing element of untyped parameter *)
				"timeradd"; (* darwin9 / accessing element of untyped parameter *)
				"timerclear"; (* darwin9 / accessing element of untyped parameter *)
				"timerisset"; (* darwin9 / accessing element of untyped parameter *)
				"timersub"; (* darwin9 / accessing element of untyped parameter *)
				"TIMESPEC_TO_TIMEVAL"; (* darwin9 / accessing element of untyped parameter *)
				"TIMEVAL_TO_TIMESPEC"]]; (* darwin9 / accessing element of untyped parameter *)
		"sys/types.h", [
			`unparsible_macro, [
				"FD_COPY"]; (* darwin9 / bcopy was undefined, #include <string.h> *)
			`uninterpretable_macro, [
				"FD_CLR"; (* darwin9 / accessing element of untyped parameter *)
				"FD_SET"]]; (* darwin9 / accessing element of untyped parameter *)
		"sys/_types/_fd_clr.h", [
			`uninterpretable_macro, [
				"FD_CLR"]]; (* darwin13 / accessing element of untyped parameter *)
		"sys/_types/_fd_def.h", [
			`uninterpretable_macro, [
				"__DARWIN_FD_CLR"; (* darwin13 / accessing element of untyped parameter *)
				"__DARWIN_FD_SET"]]; (* darwin13 / accessing element of untyped parameter *)
		"sys/_types/_fd_set.h", [
			`uninterpretable_macro, [
				"FD_SET"]]; (* darwin13 / accessing element of untyped parameter *)
		"sys/_types/___offsetof.h", [
			`unparsible_macro, [
				"__offsetof"]]; (* darwin13 / parameterized field *)
		"sys/ucred.h", [
			`unparsible_macro, [
				"cr_gid"]]; (* darwin10 / alias of element and dereferencing *)
		"uuid/uuid.h", [
			`unparsible_macro, [
				"UUID_DEFINE"]]; (* darwin12 / parameterized declaration *)
		predefined_name, [
			`unparsible_macro, [
				"__USER_LABEL_PREFIX__"]]]; (* darwin10 *)
	"*-pc-freebsd*", [
		"dirent.h", [
			`unparsible_macro, [
				"d_ino"]; (* freebsd7 / renaming field of struct in the other file *)
			`uninterpretable_macro, [
				"dirfd"]]; (* freebsd7 / accessing element of untyped parameter *)
		"fenv.h", [
			`uninterpretable_macro, [
				"__get_mxcsr"; (* freebsd7 / accessing element of untyped parameter *)
				"__set_mxcsr"]]; (* freebsd7 / accessing element of untyped parameter *)
		"machine/trap.h", [
			`unparsible_macro, [
				"ILL_RESAD_FAULT"; (* freebsd7 / T_RESADFLT was undefined *)
				"ILL_RESOP_FAULT"]]; (* freebsd7 / T_RESOPFLT was undefined *)
		"machine/param.h", [
			`unparsible_macro, [
				"MID_MACHINE"; (* freebsd7 / MID_I386 was undefined, #include <sys/imgact_aout.h> *)
				"NPDEPG"; (* freebsd7 / pd_entry_t was undefined, #include <machine/pmap.h> *)
				"NPDEPTD"; (* freebsd7 / pd_entry_t was undefined, #include <machine/pmap.h> *)
				"NPDPEPG"; (* freebsd9 / pdp_entry_t was undefined *)
				"NPML4EPG"; (* freebsd9 / pml4_entry_t was undefined *)
				"NPTEPG"]]; (* freebsd7 / pd_entry_t was undefined, #include <machine/pmap.h> *)
		"machine/_stdint.h", [
			`uninterpretable_macro, [
				"INTMAX_C"; (* freebsd9 / ## *)
				"UINTMAX_C"]]; (* freebsd9 / ## *)
		"math.h", [
			`undefined_macro, [
				"_DECLARE_C99_LDBL_MATH"]]; (* freebsd9 *)
		"netdb.h", [
			`unparsible_macro, [
				"h_addr"]]; (* freebsd7 / alias of element and dereferencing *)
		"netinet/in.h", [
			`unparsible_macro, [
				"CTL_IPPROTO_NAMES"; (* freebsd7 / partial initializer *)
				"IPCTL_NAMES"]]; (* freebsd7 / partial initializer *)
		"netinet6/in6.h", [
			`unparsible_macro, [
				"IN6_ARE_ADDR_EQUAL"; (* freebsd7 / memcmp is undefined, #include <string.h> *)
				"M_AUTHIPDGM"; (* freebsd7 / M_PROTO5 was undefined *)
				"M_AUTHIPHDR"; (* freebsd7 / M_PROTO2 was undefined *)
				"M_DECRYPTED"; (* freebsd7 / M_PROTO3 was undefined *)
				"M_LOOP"; (* freebsd7 / M_PROTO4 was undefined *)
				"M_RTALERT_MLD"]; (* freebsd9 / M_PROTO6 was undefined *)
			`uninterpretable_macro, [
				"IN6_IS_ADDR_LINKLOCAL"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_LOOPBACK"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_GLOBAL"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_LINKLOCAL"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_NODELOCAL"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_ORGLOCAL"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MC_SITELOCAL"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_MULTICAST"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_SITELOCAL"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_V4COMPAT"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_V4MAPPED"; (* freebsd7 / accessing element of untyped parameter *)
				"IN6_IS_ADDR_UNSPECIFIED"; (* freebsd7 / accessing element of untyped parameter *)
				"__IPV6_ADDR_MC_SCOPE"]]; (* freebsd7 / accessing element of untyped parameter *)
		"pthread.h", [
			`unparsible_macro, [
				"pthread_cleanup_pop"; (* freebsd9 / partial statement *)
				"pthread_cleanup_push"]]; (* freebsd9 / partial statement *)
		"stdio.h", [
			`uninterpretable_macro, [
				"__sclearerr"; (* freebsd7 / accessing element of untyped parameter *)
				"__sfeof"; (* freebsd7 / accessing element of untyped parameter *)
				"__sferror"; (* freebsd7 / accessing element of untyped parameter *)
				"__sfileno"; (* freebsd7 / accessing element of untyped parameter *)
				"__sgetc"]]; (* freebsd7 / accessing element of untyped parameter *)
		"stdlib.h", [
			`undefined_macro, [
				"__cplusplus"]]; (* freebsd9 *)
		"sys/_bitset.h", [
			`unparsible_macro, [
				"BITSET_DEFINE"; (* freebsd9.3 *)
				"BITSET_FSET"; (* freebsd9.3 *)
				"BITSET_T_INITIALIZER"]]; (* freebsd9.3 *)
		"sys/bitset.h", [
			`uninterpretable_macro, [
				"BIT_AND"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_CLR"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_CLR_ATOMIC"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_CMP"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_COPY_STORE_REL"; (* freebsd9.3 / atomic_store_rel_long is undefined *)
				"BIT_EMPTY"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_FFS"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_FILL"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_ISFULLSET"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_ISSET"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_NAND"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_OR"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_OR_ATOMIC"; (* freebsd9.3 / atomic_set_long is undefined *)
				"BIT_OVERLAP"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_SET"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_SETOF"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_SET_ATOMIC"; (* atomic_set_long is undefined *)
				"BIT_SUBSET"; (* freebsd9.3 / accessing element of untyped parameter *)
				"BIT_ZERO"]]; (* freebsd9.3 / accessing element of untyped parameter *)
		"sys/cdefs.h", [
			`undefined_macro, [
				"__FreeBSD_cc_version"]; (* freebsd7 *)
			`redefine_extended_word, [
				"__const"; (* freebsd7 *)
				"__signed"; (* freebsd9 *)
				"__restrict"]; (* freebsd7 *)
			`redefine_preprocessor_word, [
				"__has_include"]; (* freebsd9 *)
			`unparsible_macro, [
				"__CONCAT"; (* freebsd7 / ## *)
				"__containerof"; (* freebsd9 / accessing element of untyped parameter *)
				"__COPYRIGHT"; (* freebsd7 / generic declaration *)
				"__FBSDID"; (* freebsd7 / generic declaration *)
				"__IDSTRING"; (* freebsd7 / generic declaration *)
				"__offsetof"; (* freebsd7 / parameterized field *)
				"__rangeof"; (* freebsd7 / parameterized field *)
				"__RCSID"; (* freebsd7 / generic declaration *)
				"__RCSID_SOURCE"; (* freebsd7 / generic declaration *)
				"__SCCSID"; (* freebsd7 / generic declaration *)
				"_Static_assert"]; (* freebsd9 / generic declaration *)
			`uninterpretable_macro, [
				"_Alignof"; (* freebsd9 / generic expression with parameterized type *)
				"__generic"]]; (* freebsd9 / generic expression with parameterized type *)
		"sys/cpuset.h", [
			`uninterpretable_macro, [
				"CPU_AND"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_CLR"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_CLR_ATOMIC"; (* freebsd9 / atomic_clear_long is undefined *)
				"CPU_CMP"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_COPY_STORE_REL"; (* freebsd9 / atomic_store_rel_long is undefined *)
				"CPU_EMPTY"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_FFS"; (* freebsd9.3 / accessing element of untyped parameter *)
				"CPU_FILL"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_ISFULLSET"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_ISSET"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_NAND"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_OR"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_OR_ATOMIC"; (* freebsd9 / atomic_set_long is undefined *)
				"CPU_OVERLAP"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_SET"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_SET_ATOMIC"; (* freebsd9 / atomic_set_long is undefined *)
				"CPU_SETOF"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_SUBSET"; (* freebsd9 / accessing element of untyped parameter *)
				"CPU_ZERO"]]; (* freebsd9 / accessing element of untyped parameter *)
		"sys/_cpuset.h", [
			`unparsible_macro, [
				"CPUSET_FSET"]]; (* freebsd9.3 *)
		"sys/dirent.h", [
			`uninterpretable_macro, [
				"_GENERIC_DIRSIZ"]]; (* freebsd7 / accessing element of untyped parameter *)
		"sys/elf_common.h", [
			`uninterpretable_macro, [
				"IS_ELF"]]; (* freebsd9 / accessing element of untyped parameter *)
		"sys/elf_generic.h", [
			`unparsible_macro, [
				"__ElfType"; (* freebsd9 / parameterized declaration *)
				"__elfN"; (* freebsd9 / ## *)
				"__ElfN"; (* freebsd9 / ## *)
				"__ELFN"]]; (* freebsd9 / ## *)
		"sys/mount.h", [
			`uninterpretable_macro, [
				"MNT_SHARED_WRITES"]]; (* freebsd9 / accessing element of untyped parameter *)
		"sys/param.h", [
			`unparsible_macro, [
				"CBSIZE"; (* freebsd7 / cblock was undefined, #include <sys/clist.h> *)
				"member2struct"; (* freebsd9 / offsetof *)
				"__PAST_END"]]; (* freebsd9 / __typeof__ *)
		"sys/queue.h", [
			`unparsible_macro, [
				"LIST_EMPTY"; (* freebsd7 / many bad macros... *)
				"LIST_ENTRY";
				"LIST_FIRST";
				"LIST_FOREACH";
				"LIST_FOREACH_FROM"; (* freebsd9 *)
				"LIST_FOREACH_FROM_SAFE"; (* freebsd9 *)
				"LIST_FOREACH_SAFE";
				"LIST_HEAD";
				"LIST_HEAD_INITIALIZER";
				"LIST_INIT";
				"LIST_INSERT_AFTER";
				"LIST_INSERT_BEFORE";
				"LIST_INSERT_HEAD";
				"LIST_NEXT";
				"LIST_PREV"; (* freebsd9 *)
				"LIST_REMOVE";
				"LIST_SWAP"; (* freebsd9 *)
				"SLIST_EMPTY";
				"SLIST_ENTRY";
				"SLIST_FIRST";
				"SLIST_FOREACH";
				"SLIST_FOREACH_FROM"; (* freebsd9 *)
				"SLIST_FOREACH_FROM_SAFE"; (* freebsd9 *)
				"SLIST_FOREACH_PREVPTR";
				"SLIST_FOREACH_SAFE";
				"SLIST_HEAD";
				"SLIST_HEAD_INITIALIZER";
				"SLIST_INIT";
				"SLIST_INSERT_AFTER";
				"SLIST_INSERT_HEAD";
				"SLIST_NEXT";
				"SLIST_REMOVE";
				"SLIST_REMOVE_AFTER"; (* freebsd9 *)
				"SLIST_REMOVE_HEAD";
				"SLIST_SWAP"; (* freebsd9 *)
				"STAILQ_CONCAT";
				"STAILQ_EMPTY";
				"STAILQ_ENTRY";
				"STAILQ_FIRST";
				"STAILQ_FOREACH";
				"STAILQ_FOREACH_FROM"; (* freebsd9 *)
				"STAILQ_FOREACH_FROM_SAFE"; (* freebsd9 *)
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
				"STAILQ_REMOVE_AFTER"; (* freebsd9 *)
				"STAILQ_REMOVE_HEAD";
				"STAILQ_SWAP"; (* freebsd9 *)
				"TAILQ_CONCAT";
				"TAILQ_EMPTY";
				"TAILQ_ENTRY";
				"TAILQ_FIRST";
				"TAILQ_FOREACH";
				"TAILQ_FOREACH_FROM"; (* freebsd9 *)
				"TAILQ_FOREACH_FROM_SAFE"; (* freebsd9 *)
				"TAILQ_FOREACH_REVERSE";
				"TAILQ_FOREACH_REVERSE_FROM"; (* freebsd9 *)
				"TAILQ_FOREACH_REVERSE_FROM_SAFE"; (* freebsd9 *)
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
				"TAILQ_REMOVE";
				"TAILQ_SWAP"]]; (* freebsd9 *)
		"sys/select.h", [
			`uninterpretable_macro, [
				"FD_CLR"; (* freebsd7 / accessing element of untyped parameter *)
				"FD_ISSET"; (* freebsd7 / accessing element of untyped parameter *)
				"FD_SET"]]; (* freebsd7 / accessing element of untyped parameter *)
		"sys/socket.h", [
			`unparsible_macro, [
				"CTL_NET_NAMES"; (* freebsd7 / partial initializer *)
				"CTL_NET_RT_NAMES"]; (* freebsd7 / partial initializer *)
			`uninterpretable_macro, [
				"CMSG_FIRSTHDR"; (* freebsd7 / accessing element of untyped parameter *)
				"CMSG_NXTHDR"]]; (* freebsd7 / accessing element of untyped parameter *)
		"sys/sysctl.h", [
			`unparsible_macro, [
				"CTL_HW_NAMES"; (* freebsd9 / partial initializer *)
				"CTL_KERN_NAMES"; (* freebsd9 / partial initializer *)
				"CTL_NAMES"; (* freebsd9 / partial initializer *)
				"CTL_P1003_1B_NAMES"; (* freebsd9 / partial initializer *)
				"CTL_USER_NAMES"; (* freebsd9 / partial initializer *)
				"CTL_VFS_NAMES"]]; (* freebsd9 / partial initializer *)
		"sys/time.h", [
			`unparsible_macro, [
				"bintime_cmp"; (* freebsd9 / parameterized operator *)
				"timercmp"]; (* freebsd7 / parameterized operator *)
			`uninterpretable_macro, [
				"bintime_clear"; (* freebsd9 / accessing element of untyped parameter *)
				"bintime_isset"; (* freebsd9 / accessing element of untyped parameter *)
				"timeradd"; (* freebsd7 / accessing element of untyped parameter *)
				"timerclear"; (* freebsd7 / accessing element of untyped parameter *)
				"timerisset"; (* freebsd7 / accessing element of untyped parameter *)
				"timersub"]]; (* freebsd7 / accessing element of untyped parameter *)
		"sys/timespec.h", [
			`uninterpretable_macro, [
				"TIMEVAL_TO_TIMESPEC"; (* freebsd7 / accessing element of untyped parameter *)
				"TIMESPEC_TO_TIMEVAL"]]; (* freebsd7 / accessing element of untyped parameter *)
		"sys/ucred.h", [
			`unparsible_macro, [
				"cr_gid"]]; (* freebsd7 / alias of element and dereferencing *)
		"x86/_stdint.h", [
			`uninterpretable_macro, [
				"INTMAX_C"; (* freebsd9.3 / ## *)
				"UINTMAX_C"]]]; (* freebsd9.3 / ## *)
	"*-linux-gnu*", [
		"asm-generic/ioctls.h", [
			`unparsible_macro, [
				"TCGETS2"; (* struct termios2 is undefined *)
				"TCSETS2"; (* struct termios2 is undefined *)
				"TCSETSF2"; (* struct termios2 is undefined *)
				"TCSETSW2"]]; (* struct termios2 is undefined *)
		"assert.h", [
			`unparsible_macro, [
				"__ASSERT_FUNCTION"; (* __PRETTY_FUNCTION__ outside of a function *)
				"__ASSERT_VOID_CAST"]]; (* partial cast *)
		"bits/cmathcalls.h", [
			`unparsible_macro, [
				"_Mdouble_complex_"]]; (* type specifier with an identifier to redefine *)
		"bits/cpu-set.h", [
			`unparsible_macro, [
				"__CPU_CLR_S"; (* accessing element of untyped parameter *)
				"__CPU_ISSET_S"; (* accessing element of untyped parameter *)
				"__CPU_OP_S"; (* accessing element of untyped parameter *)
				"__CPU_SET_S"]]; (* accessing element of untyped parameter *)
		"bits/dlfcn.h", [
			`unparsible_macro, [
				"DL_CALL_FCT"]]; (* parameterized argument expression list *)
		"bits/_G_config.h", [
			`unparsible_macro, [
				"_G_HAVE_ST_BLKSIZE"]]; (* using defined outside of #if *)
		"bits/in.h", [
			`unparsible_macro, [
				"SCM_SRCRT"]]; (* IPV6_RXSRCRT is undefined *)
		"bits/libio.h", [
			`unparsible_macro, [
				"_IO_iconv_t"; (* _G_iconv_t is undefined *)
				"_IO_stderr"; (* _IO_2_1_stderr_ is opaque *)
				"_IO_stdin"; (* _IO_2_1_stdin_ is opaque *)
				"_IO_stdout"; (* _IO_2_1_stdout_ is opaque *)
				"_IO_wint_t"]; (* _G_wint_t is undefined *)
			`uninterpretable_macro, [
				"_IO_PENDING_OUTPUT_COUNT"; (* accessing element of untyped parameter *)
				"_IO_feof_unlocked"; (* accessing element of untyped parameter *)
				"_IO_ferror_unlocked"; (* accessing element of untyped parameter *)
				"_IO_getc_unlocked"; (* accessing element of untyped parameter *)
				"_IO_need_lock"; (* accessing element of untyped parameter *)
				"_IO_peekc"; (* accessing element of untyped parameter *)
				"_IO_peekc_unlocked"; (* accessing element of untyped parameter *)
				"_IO_putc_unlocked"]]; (* accessing element of untyped parameter *)
		"bits/mathdef.h", [
			`undefined_macro, [
				"__FP_FAST_FMA";
				"__FP_FAST_FMAF";
				"__FP_FAST_FMAL"]];
		"bits/predefs.h", [
			`redefine_compiler_macro, [
				"__STDC_IEC_559__";
				"__STDC_IEC_559_COMPLEX__"]];
		"bits/pthreadtypes.h", [
			`unparsible_macro, [
				"__PTHREAD_RWLOCK_ELISION_EXTRA"; (* partial initializer *)
				"__PTHREAD_SPINS"]]; (* partial initializer *)
		"bits/pthreadtypes-arch.h", [
			`unparsible_macro, [
				"__PTHREAD_RWLOCK_ELISION_EXTRA"]]; (* partial initializer *)
		"bits/sched.h", [
			`unparsible_macro, [
				"__CPU_OP_S"]; (* parameterized operator *)
			`uninterpretable_macro, [
				"__CPU_CLR_S"; (* accessing element of untyped parameter *)
				"__CPU_ISSET_S"; (* accessing element of untyped parameter *)
				"__CPU_SET_S"]]; (* accessing element of untyped parameter *)
		"bits/select.h", [
			`uninterpretable_macro, [
				"__FD_CLR"; (* accessing element of untyped parameter *)
				"__FD_SET"; (* accessing element of untyped parameter *)
				"__FD_ISSET"; (* accessing element of untyped parameter *)
				"__FD_ZERO"]]; (* accessing element of untyped parameter *)
		"bits/siginfo.h", [
			`unparsible_macro, [
				"si_int"; (* (siginfo_t)._sifields._rt.si_sigval is not struct *)
				"si_ptr"]]; (* (siginfo_t)._sifields._rt.si_sigval is not struct *)
		"bits/socket.h", [
			`uninterpretable_macro, [
				"CMSG_DATA"; (* accessing element of untyped parameter *)
				"CMSG_FIRSTHDR"]]; (* accessing element of untyped parameter *)
		"bits/stat.h", [
			`uninterpretable_macro, [
				"__S_TYPEISMQ"; (* accessing element of untyped parameter *)
				"__S_TYPEISSEM"; (* accessing element of untyped parameter *)
				"__S_TYPEISSHM"]]; (* accessing element of untyped parameter *)
		"bits/termios.h", [
			`unparsible_macro, [
				"_IOT_termios"]]; (* _IOT is undefined *)
		"bits/thread-shared-types.h", [
			`unparsible_macro, [
				"__PTHREAD_SPINS"; (* partial initializer *)
				"__PTHREAD_SPINS_DATA"]]; (* new struct in macro *)
		"bits/types/siginfo_t.h", [
			`unparsible_macro, [
				"si_int"; (* (siginfo_t)._sifields._rt.si_sigval is not struct *)
				"si_ptr"]]; (* (siginfo_t)._sifields._rt.si_sigval is not struct *)
		"bits/typesizes.h", [
			`unparsible_macro, [
				"__TIMER_T_TYPE"]]; (* declaration specifier and pointer *)
		"ctype.h", [
			`unparsible_macro, [
				"__exctype"; (* parameterized declaration *)
				"__exctype_l"; (* parameterized declaration *)
				"__tobody"]; (* parameterized statement *)
			`uninterpretable_macro, [
				"__isalnum_l"; (* accessing element of untyped parameter *)
				"__isalpha_l"; (* accessing element of untyped parameter *)
				"__isblank_l"; (* accessing element of untyped parameter *)
				"__iscntrl_l"; (* accessing element of untyped parameter *)
				"__isctype_l"; (* accessing element of untyped parameter *)
				"__isdigit_l"; (* accessing element of untyped parameter *)
				"__isgraph_l"; (* accessing element of untyped parameter *)
				"__islower_l"; (* accessing element of untyped parameter *)
				"__isprint_l"; (* accessing element of untyped parameter *)
				"__ispunct_l"; (* accessing element of untyped parameter *)
				"__isspace_l"; (* accessing element of untyped parameter *)
				"__isupper_l"; (* accessing element of untyped parameter *)
				"__isxdigit_l"]]; (* accessing element of untyped parameter *)
		"dirent.h", [
			`uninterpretable_macro, [
				"_D_ALLOC_NAMLEN"; (* accessing element of untyped parameter *)
				"_D_EXACT_NAMLEN"]]; (* accessing element of untyped parameter *)
		"features.h", [
			`redefine_compiler_macro, [
				"__STDC_ISO_10646__"]];
		"fnmatch.h", [
			`redefine_extended_word, [
				"__const"]];
		"_G_config.h", [
			`unparsible_macro, [
				"_G_FSTAT64"; (* __fxstat64 is undefined *)
				"_G_HAVE_ST_BLKSIZE"; (* using defined outside of #if *)
				"_G_LSEEK64"; (* __lseek64 is undefined *)
				"_G_MMAP64"; (* __mmap64 is undefined *)
				"_G_OPEN64"; (* __open64 is undefined *)
				"_G_stat64"; (* stat64 is undefined *)
				"_G_VTABLE_LABEL_PREFIX_ID"; (* __vt__ is undefined *)
				"_G_wint_t"]]; (* wint_t is undefined *)
		"libio.h", [
			`unparsible_macro, [
				"_IO_iconv_t"; (* _G_iconv_t is undefined *)
				"_IO_stderr"; (* _IO_2_1_stderr_ is opaque *)
				"_IO_stdin"; (* _IO_2_1_stdin_ is opaque *)
				"_IO_stdout"; (* _IO_2_1_stdout_ is opaque *)
				"_IO_wint_t"]; (* _G_wint_t is undefined *)
			`uninterpretable_macro, [
				"_IO_feof_unlocked"; (* accessing element of untyped parameter *)
				"_IO_ferror_unlocked"; (* accessing element of untyped parameter *)
				"_IO_getc_unlocked"; (* accessing element of untyped parameter *)
				"_IO_need_lock"; (* accessing element of untyped parameter *)
				"_IO_peekc"; (* accessing element of untyped parameter *)
				"_IO_peekc_unlocked"; (* accessing element of untyped parameter *)
				"_IO_PENDING_OUTPUT_COUNT"; (* accessing element of untyped parameter *)
				"_IO_putc_unlocked"]]; (* accessing element of untyped parameter *)
		"link.h", [
			`unparsible_macro, [
				"ElfW"]]; (* ## *)
		"malloc.h", [
			`unparsible_macro, [
				"__MALLOC_P"; (* parameter list *)
				"__malloc_ptr_t"]]; (* declaration specifier and pointer *)
		"math.h", [
			`unparsible_macro, [
				"__MATHCALL_VEC"; (* parameterized declaration *)
				"__MATHCALLX"; (* parameterized declaration *)
				"__MATHDECL_VEC"; (* parameterized declaration *)
				"__MATHDECLX"]]; (* parameterized declaration *)
		"netdb.h", [
			`unparsible_macro, [
				"h_addr"]]; (* alias of element and dereferencing *)
		"predefs.h", [
			`redefine_compiler_macro, [
				"__STDC_IEC_559__";
				"__STDC_IEC_559_COMPLEX__"]];
		"pthread.h", [
			`unparsible_macro, [
				"pthread_cleanup_pop"; (* partial statement *)
				"pthread_cleanup_pop_restore_np"; (* partial statement *)
				"pthread_cleanup_push"; (* partial statement *)
				"pthread_cleanup_push_defer_np"; (* partial statement *)
				"__PTHREAD_SPINS"]]; (* partial initializer *)
		"sched.h", [
			`unparsible_macro, [
				"sched_priority"]; (* __sched_priority is undefined *)
			`uninterpretable_macro, [
				"CPU_AND"; (* accessing element of untyped parameter *)
				"CPU_AND_S"; (* accessing element of untyped parameter *)
				"CPU_CLR"; (* accessing element of untyped parameter *)
				"CPU_CLR_S"; (* accessing element of untyped parameter *)
				"CPU_ISSET"; (* accessing element of untyped parameter *)
				"CPU_ISSET_S"; (* accessing element of untyped parameter *)
				"CPU_OR"; (* accessing element of untyped parameter *)
				"CPU_OR_S"; (* accessing element of untyped parameter *)
				"CPU_SET"; (* accessing element of untyped parameter *)
				"CPU_SET_S"; (* accessing element of untyped parameter *)
				"CPU_XOR"; (* accessing element of untyped parameter *)
				"CPU_XOR_S"]]; (* accessing element of untyped parameter *)
		"stdc-predef.h", [
			`redefine_compiler_macro, [
				"__STDC_ISO_10646__";
				"__STDC_IEC_559__";
				"__STDC_IEC_559_COMPLEX__"]];
		"stdlib.h", [
			`unparsible_macro, [
				"__WAIT_INT"; (* new type is declared in expression statement *)
				"__WAIT_STATUS_DEFN"; (* declaration specifier and pointer *)
				"WEXITSTATUS"; (* using __WAIT_INT *)
				"WIFCONTINUED"; (* using __WAIT_INT *)
				"WIFEXITED"; (* using __WAIT_INT *)
				"WIFSIGNALED"; (* using __WAIT_INT *)
				"WIFSTOPPED"; (* using __WAIT_INT *)
				"WSTOPSIG"; (* using __WAIT_INT *)
				"WTERMSIG"]]; (* using __WAIT_INT *)
		"sys/cdefs.h", [
			`unparsible_macro, [
				"__ASMNAME"; (* # *)
				"__ASMNAME2"; (* # *)
				"__attribute_alloc_size__"; (* parameterized attribute with paren *)
				"__errordecl"; (* parameterized declaration *)
				"__extern_always_inline"; (* __inline and attributes *)
				"__flexarr"; (* [] *)
				"__fortify_function"; (* attribute and storage class *)
				"__glibc_macro_warning"; (* illegal _Pragma *)
				"__LEAF"; (* attribute and comma *)
				"__LDBL_REDIR"; (* parameterized partial declaration *)
				"__LDBL_REDIR_NTH"; (* parameterized partial declaration *)
				"__LDBL_REDIR1"; (* parameterized partial declaration *)
				"__LDBL_REDIR1_NTH"; (* parameterized partial declaration *)
				"__nonnull"; (* parameterized attribute with paren *)
				"__NTH"; (* attribute with an identifier *)
				"__ptr_t"; (* declaration specifier and pointer *)
				"__REDIRECT_LDBL"; (* parameterized partial declaration *)
				"__REDIRECT_NTH_LDBL"; (* parameterized partial declaration *)
				"__va_arg_pack"; (* other languages can not use this feature *)
				"__va_arg_pack_len"; (* other languages can not use this feature *)
				"__warnattr"; (* parameterized declaration *)
				"__warndecl"]]; (* parameterized declaration *)
		"sys/select.h", [
			`uninterpretable_macro, [
				"__FDS_BITS"; (* accessing element of untyped parameter *)
				"FD_CLR"; (* accessing element of untyped parameter *)
				"FD_ISSET"; (* accessing element of untyped parameter *)
				"FD_SET"; (* accessing element of untyped parameter *)
				"FD_ZERO"]]; (* accessing element of untyped parameter *)
		"sys/socket.h", [
			`unparsible_macro, [
				"__CONST_SOCKADDR_ARG"; (* declaration specifier and pointer *)
				"__SOCKADDR_ALLTYPES"; (* struct-declaration-list *)
				"__SOCKADDR_ARG"]]; (* declaration specifier and pointer *)
		"sys/stat.h", [
			`uninterpretable_macro, [
				"S_TYPEISMQ"; (* accessing element of untyped parameter *)
				"S_TYPEISSEM"; (* accessing element of untyped parameter *)
				"S_TYPEISSHM"]]; (* accessing element of untyped parameter *)
		"sys/sysmacros.h", [
			`unparsible_macro, [
				"makedev"; (* illegal text *)
				"major"; (* illegal text *)
				"minor"]]; (* illegal text *)
		"sys/time.h", [
			`unparsible_macro, [
				"timercmp"]; (* parameterized operator *)
			`uninterpretable_macro, [
				"timeradd"; (* accessing element of untyped parameter *)
				"timerclear"; (* accessing element of untyped parameter *)
				"timerisset"; (* accessing element of untyped parameter *)
				"timersub"; (* accessing element of untyped parameter *)
				"TIMESPEC_TO_TIMEVAL"; (* accessing element of untyped parameter *)
				"TIMEVAL_TO_TIMESPEC"]]; (* accessing element of untyped parameter *)
		"sys/wait.h", [
			`unparsible_macro, [
				"WCOREDUMP"]]]; (* using __WAIT_INT *)
	"*-w64-mingw*", [
		"apiset.h", [
			`unparsible_macro, [
				"API_SET_HELPER_NAME"; (* ApiSetHelp is undefined *)
				"API_SET_LEGACY_OVERRIDE_DEF"; (* XImplementation is undefined *)
				"API_SET_LIBRARY"; (* partial parameterized declaration ??? *)
				"API_SET_OVERRIDE_DEF"; (* partial parameterized declaration ??? *)
				"API_SET_PRIVATE"; (* partial parameterized declaration ??? *)
				"API_SET_SCHEMA_NAME"; (* ApiSetSchema is undefined *)
				"API_SET_STRING_U_X"]]; (* ## *)
		"bcrypt.h", [
			`unparsible_macro, [
				"BCRYPT_INIT_AUTH_MODE_INFO"; (* multi-statements *)
				"BCRYPT_MAKE_INTERFACE_VERSION"]; (* parameterized initializer *)
			`uninterpretable_macro, [
				"BCRYPT_IS_INTERFACE_VERSION_COMPATIBLE"]]; (* accessing element of untyped parameter *)
		"commdlg.h", [
			`unparsible_macro, [
				"CDSIZEOF_STRUCT"]]; (* parameterized field *)
		"crtdefs.h", [
			`unparsible_macro, [
				"_CRT_SECURE_CPP_NOTHROW"]]; (* C++ throw *)
		"ctype.h", [
			`unparsible_macro, [
				"_chvalidchk_l"; (* access element of opaque type *)
				"_isalnum_l"; (* using _ischartype_l *)
				"_isalpha_l"; (* using _ischartype_l *)
				"_ischartype_l"; (* using _chvalidchk_l *)
				"_iscsymf_l"; (* using _isalpha_l *)
				"_iscsym_l"]]; (* using isalnum_l *)
		"excpt.h", [
			`unparsible_macro, [
				"__except1"; (* extra semicolon *)
				"exception_info"; (* alias of function and cast *)
				"GetExceptionInformation"]]; (* alias of function and cast *)
		"float.h", [
			`undefined_macro, [
				"__clang_major__"]];
		"fltwinerror.h", [
			`uninterpretable_macro, [
				"FILTER_HRESULT_FROM_FLT_NTSTATUS"]]; (* NT_ASSERT is undefined *)
		"guiddef.h", [
			`unparsible_macro, [
				"CLSID_NULL"; (* GUID_NULL is undefined on WIN32_LEAN_AND_MEAN mode *)
				"FMTID_NULL"; (* GUID_NULL is undefined on WIN32_LEAN_AND_MEAN mode *)
				"IID_NULL"; (* GUID_NULL is undefined on WIN32_LEAN_AND_MEAN mode *)
				"REFCLSID"; (* declaration specifier and pointer *)
				"REFFMTID"; (* declaration specifier and pointer *)
				"REFGUID"; (* declaration specifier and pointer *)
				"REFIID"; (* declaration specifier and pointer *)
				"DEFINE_GUID"; (* parameterized declaration *)
				"DEFINE_OLEGUID"]; (* parameterized declaration *)
			`uninterpretable_macro, [
				"InlineIsEqualGUID"]]; (* accessing element of untyped parameter *)
		"malloc.h", [
			`unparsible_macro, [
				"_malloca"; (* for the case of RC_INVOKED is defined *)
				"_STATIC_ASSERT"]; (* parameterized declaration *)
			`uninterpretable_macro, [
				"_mm_free"; (* _aligned_free is undefined unless __DO_ALIGN_DEFINES *)
				"_mm_malloc"]]; (* _aligned_malloc is undefined unless __DO_ALIGN_DEFINES *)
		"math.h", [
			`unparsible_macro, [
				"fpclassify"; (* magic macro *)
				"isfinite"; (* magic macro *)
				"isinf"; (* magic macro *)
				"isnan"; (* magic macro *)
				"isnormal"; (* magic macro *)
				"signbit"]]; (* magic macro *)
		"_mingw.h", [
			`undefined_macro, [
				"__clang_major__";
				"__cplusplus"];
			`unparsible_macro, [
				"_CRT_DEPRECATE_TEXT"; (* __declspec *)
				"_CRT_glob"; (* _dowildcard is undefined *)
				"_CRT_WIDE"; (* ## *)
				"__MINGW_BROKEN_INTERFACE"; (* parameterized pragma *)
				"__UNUSED_PARAM"]]; (* attribute with an identifier *)
		"_mingw_secapi.h", [
			`unparsible_macro, [
				"__CRT_SECURE_CPP_OVERLOAD_SECURE_NAMES_0_2_"; (* parameterized declaraton *)
				"__CRT_SECURE_CPP_OVERLOAD_SECURE_NAMES_MEMORY_0_3_"; (* parameterized declaraton *)
				"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_0_2_"; (* parameterized declaraton *)
				"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT_0_3_"; (* parameterized declaraton *)
				"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT_1_4_"; (* parameterized declaraton *)
				"__CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_MEMORY_0_3_"; (* parameterized declaraton *)
				"__MINGW_CRT_NAME_CONCAT1"]]; (* :: *)
		"_mingw_unicode.h", [
			`unparsible_macro, [
				"__MINGW_TYPEDEF_AW"; (* parameterized declaration *)
				"__MINGW_TYPEDEF_UAW"]]; (* parameterized declaration *)
		"minwinbase.h", [
			`unparsible_macro, [
				"EXCEPTION_POSSIBLE_DEADLOCK"]]; (* STATUS_POSSIBLE_DEADLOCK is undefined *)
		"mstcpip.h", [
			`redefine_macro, [
				"SOCKET_SETTINGS_ALLOW_INSECURE";
				"SOCKET_SETTINGS_GUARANTEE_ENCRYPTION"]];
		"namedpipeapi.h", [
			`unparsible_macro, [
				"GetNamedPipeClientComputerName"]]; (* GetNamedPipeClientComputerNameW is undefined *)
		"ntsecapi.h", [
			`uninterpretable_macro, [
				"MSV1_0_NTLM3_MIN_NT_RESPONSE_LENGTH"]]; (* use RTL_SIZEOF_THROUGH_FIELD *)
		"oleauto.h", [
			`unparsible_macro, [
				"WINOLEAUTAPI"; (* storage class and type *)
				"WINOLEAUTAPI_"]; (* storage class and parameterized type *)
			`uninterpretable_macro, [
				"V_ARRAY"; (* accessing element of untyped parameter *)
				"V_ARRAYREF"; (* accessing element of untyped parameter *)
				"V_BOOL"; (* accessing element of untyped parameter *)
				"V_BOOLREF"; (* accessing element of untyped parameter *)
				"V_BSTR"; (* accessing element of untyped parameter *)
				"V_BSTRREF"; (* accessing element of untyped parameter *)
				"V_BYREF"; (* accessing element of untyped parameter *)
				"V_CY"; (* accessing element of untyped parameter *)
				"V_CYREF"; (* accessing element of untyped parameter *)
				"V_DATE"; (* accessing element of untyped parameter *)
				"V_DATEREF"; (* accessing element of untyped parameter *)
				"V_DECIMAL"; (* accessing element of untyped parameter *)
				"V_DECIMALREF"; (* accessing element of untyped parameter *)
				"V_DISPATCH"; (* accessing element of untyped parameter *)
				"V_DISPATCHREF"; (* accessing element of untyped parameter *)
				"V_ERROR"; (* accessing element of untyped parameter *)
				"V_ERRORREF"; (* accessing element of untyped parameter *)
				"V_I1"; (* accessing element of untyped parameter *)
				"V_I1REF"; (* accessing element of untyped parameter *)
				"V_I2"; (* accessing element of untyped parameter *)
				"V_I2REF"; (* accessing element of untyped parameter *)
				"V_I4"; (* accessing element of untyped parameter *)
				"V_I4REF"; (* accessing element of untyped parameter *)
				"V_I8"; (* accessing element of untyped parameter *)
				"V_I8REF"; (* accessing element of untyped parameter *)
				"V_INT"; (* accessing element of untyped parameter *)
				"V_INT_PTR"; (* accessing element of untyped parameter *)
				"V_INT_PTRREF"; (* accessing element of untyped parameter *)
				"V_INTREF"; (* accessing element of untyped parameter *)
				"V_ISARRAY"; (* accessing element of untyped parameter *)
				"V_ISBYREF"; (* accessing element of untyped parameter *)
				"V_ISVECTOR"; (* accessing element of untyped parameter *)
				"V_NONE"; (* accessing element of untyped parameter *)
				"V_R4"; (* accessing element of untyped parameter *)
				"V_R4REF"; (* accessing element of untyped parameter *)
				"V_R8"; (* accessing element of untyped parameter *)
				"V_R8REF"; (* accessing element of untyped parameter *)
				"V_RECORD"; (* accessing element of untyped parameter *)
				"V_RECORDINFO"; (* accessing element of untyped parameter *)
				"V_UI1"; (* accessing element of untyped parameter *)
				"V_UI1REF"; (* accessing element of untyped parameter *)
				"V_UI2"; (* accessing element of untyped parameter *)
				"V_UI2REF"; (* accessing element of untyped parameter *)
				"V_UI4"; (* accessing element of untyped parameter *)
				"V_UI4REF"; (* accessing element of untyped parameter *)
				"V_UI8"; (* accessing element of untyped parameter *)
				"V_UI8REF"; (* accessing element of untyped parameter *)
				"V_UINT"; (* accessing element of untyped parameter *)
				"V_UINT_PTR"; (* accessing element of untyped parameter *)
				"V_UINT_PTRREF"; (* accessing element of untyped parameter *)
				"V_UINTREF"; (* accessing element of untyped parameter *)
				"V_UNION"; (* accessing element of untyped parameter *)
				"V_UNKNOWN"; (* accessing element of untyped parameter *)
				"V_UNKNOWNREF"; (* accessing element of untyped parameter *)
				"V_VARIANTREF"; (* accessing element of untyped parameter *)
				"V_VT"]]; (* accessing element of untyped parameter *)
		"objbase.h", [
			`unparsible_macro, [
				"DECLARE_INTERFACE_"; (* parameterized type *)
				"DECLARE_INTERFACE_IID_"; (* parameterized type *)
				"IFACEMETHOD"; (* parameterized declaration *)
				"IFACEMETHOD_"; (* parameterized declaration *)
				"IFACEMETHODV"; (* parameterized declaration *)
				"IFACEMETHODV_"; (* parameterized declaration *)
				"STDMETHOD"; (* parameterized declaration *)
				"STDMETHOD_"; (* parameterized declaration *)
				"STDMETHODV"; (* parameterized declaration *)
				"STDMETHODV_"; (* parameterized declaration *)
				"THIS"; (* parameter list *)
				"THIS_"; (* parameter list *)
				"WINOLEAPI"; (* storage class and type *)
				"WINOLEAPI_"]; (* storage class and parameterized type *)
			`uninterpretable_macro, [
				"LISet32"; (* accessing element of untyped parameter *)
				"ULISet32"]]; (* accessing element of untyped parameter *)
		"propidl.h", [
			`unparsible_macro, [
				"REFPROPVARIANT"]; (* declaration specifier and pointer *)
			`uninterpretable_macro, [
				"PropVariantInit"]]; (* sizeof untyped parameter *)
		"prsht.h", [
			`unparsible_macro, [
				"CCSIZEOF_STRUCT"; (* parameterized field *)
				"PROPSHEETPAGEA_V1_FIELDS"; (* partial declaration *)
				"PROPSHEETPAGEW_V1_FIELDS"; (* partial declaration *)
				"PROPSHEETHEADER_V2_SIZE"; (* does not have DUMMYUNION5_MEMBER *)
				"PROPSHEETHEADERA_V2_SIZE"; (* does not have DUMMYUNION5_MEMBER *)
				"PROPSHEETHEADERW_V2_SIZE"]]; (* does not have DUMMYUNION5_MEMBER *)
		"psdk_inc/intrin-mac.h", [
			`unparsible_macro, [
				"__buildbittesti"; (* parameterized declaration *)
				"__buildstos"]]; (* parameterized declaration *)
		"psdk_inc/intrin-impl.h", [
			`unparsible_macro, [
				"__buildbitscan"; (* parameterized declaration *)
				"__buildbittest"; (* parameterized declaration *)
				"__buildbittestand"; (* parameterized declaration *)
				"__buildbittesti"; (* parameterized declaration *)
				"__build_inport"; (* parameterized declaration *)
				"__build_inportstring"; (* parameterized declaration *)
				"__buildmov"; (* parameterized declaration *)
				"__build_outport"; (* parameterized declaration *)
				"__build_outportstring"; (* parameterized declaration *)
				"__build_readcr"; (* parameterized declaration *)
				"__buildreadseg"; (* parameterized declaration *)
				"__buildstos"; (* parameterized declaration *)
				"__build_writecr"; (* parameterized declaration *)
				"__buildwriteseg"]]; (* parameterized declaration *)
		"psdk_inc/_ip_types.h", [
			`unparsible_macro, [
				"h_addr"]]; (* alias of element and dereferencing *)
		"rpc.h", [
			`unparsible_macro, [
				"interface"; (* reserved word *)
				"RpcEndFinally"; (* partial statement *)
				"RpcEndExcept"; (* partial statement *)
				"RpcExcept"; (* partial statement *)
				"RpcFinally"; (* partial statement *)
				"RpcTryExcept"; (* partial statement *)
				"RpcTryFinally"]]; (* partial statement *)
		"rpcasync.h", [
			`uninterpretable_macro, [
				"RpcAsyncGetCallHandle"]]; (* accessing element of untyped parameter *)
		"rpcndr.h", [
			`unparsible_macro, [
				"boolean_array_from_ndr"; (* NDRcopy is undefined *)
				"byte_array_from_ndr"; (* NDRcopy is undefined *)
				"EXTERN_GUID"; (* parameterized declaration *)
				"MIDL_INTERFACE"; (* reserved word *)
				"NdrFieldPad"; (* parameterized field *)
				"NdrFieldOffset"; (* parameterized field *)
				"NdrMarshSCtxtHdl"; (* bug? misspell of N*DR*SContextMarshall *)
				"NdrUnMarshSCtxtHdl"; (* bug? misspell of N*DR*SContextUn*m*arshall *)
				"small_array_from_ndr"]; (* NDRcopy is undefined *)
			`uninterpretable_macro, [
				"boolean_from_ndr"; (* accessing element of untyped parameter *)
				"byte_from_ndr"; (* accessing element of untyped parameter *)
				"NDRSContextValue"; (* accessing element of untyped parameter *)
				"small_from_ndr"]]; (* accessing element of untyped parameter *)
		"sdkddkver.h", [
			`unparsible_macro, [
				"NTDDI_VERSION_FROM_WIN32_WINNT"]]; (* ## *)
		"shellapi.h", [
			`unparsible_macro, [
				"SHDOCAPI"; (* storage class and type *)
				"SHDOCAPI_"; (* storage class and parameterized type *)
				"SHSTDAPI"; (* storage class and type *)
				"SHSTDAPI_"]]; (* storage class and parameterized type *)
		"specstrings.h", [
			`undefined_macro, [
				"_MSC_VER"]];
		"stdio.h", [
			`undefined_macro, [
				"__USE_MINGW_ANSI_STDIO"];
			`unparsible_macro, [
				"__MINGW_PRINTF_FORMAT"; (* ms_printf is undefined *)
				"__MINGW_SCANF_FORMAT"]; (* ms_scanf is undefined *)
			`uninterpretable_macro, [
				"_fgetc_nolock"; (* accessing element of untyped parameter *)
				"_fputc_nolock"; (* accessing element of untyped parameter *)
				"_getc_nolock"; (* accessing element of untyped parameter *)
				"_getchar_nolock"; (* accessing element of untyped parameter *)
				"_putc_nolock"; (* accessing element of untyped parameter *)
				"_putchar_nolock"]]; (* accessing element of untyped parameter *)
		"stdlib.h", [
			`push_defined_macro, [
				"_rotl";
				"_rotr"];
			`uninterpretable_macro, [
				"_PTR_LD"]]; (* accessing element of untyped parameter *)
		"stralign.h", [
			`redefine_macro, [
				"WSTR_ALIGNED"]; (* 64 bit mode *)
			`unparsible_macro, [
				"__UA_STACKCOPY"]]; (* _alloca is undefined *)
		"_timeval.h", [
			`unparsible_macro, [
				"timercmp"]; (* parameterized operator *)
			`uninterpretable_macro, [
				"timerclear"; (* accessing element of untyped parameter *)
				"timerisset"]]; (* accessing element of untyped parameter *)
		"unknwn.h", [
			`push_defined_macro, [
				"interface"]];
		"urlmon.h", [
			`unparsible_macro, [
				"LPOINETPROTOCOLSINKSTACKABLE"]]; (* bug? misspell of LPIINTERNETPROTOCOLSINKStackable *)
		"winbase.h", [
			`undefined_macro, [
				"NTDDI_WIN7SP1"];
			`push_defined_macro, [
				"GetEnvironmentStrings"];
			`unparsible_macro, [
				"EXCEPTION_POSSIBLE_DEADLOCK"; (* STATUS_POSSIBLE_DEADLOCK is undefined *)
				"MICROSOFT_WINDOWS_WINBASE_H_DEFINE_INTERLOCKED_CPLUSPLUS_OVERLOADS"]; (* "defined" used out of preprocessor *)
			`uninterpretable_macro, [
				"HasOverlappedIoCompleted"]]; (* accessing element of untyped parameter *)
		"wincrypt.h", [
			`unparsible_macro, [
				"_CRYPT32WTEXT"]]; (* ## *)
		"winerror.h", [
			`unparsible_macro, [
				"GetScode"; (* SCODE is undefined on WIN32_LEAN_AND_MEAN mode *)
				"MAKE_SCODE"]]; (* SCODE is undefined on WIN32_LEAN_AND_MEAN mode *)
		"winioctl.h", [
			`uninterpretable_macro, [
				"DiskGeometryGetDetect"; (* accessing element of untyped parameter *)
				"DiskGeometryGetPartition"]]; (* accessing element of untyped parameter *)
		"winnls.h", [
			`uninterpretable_macro, [
				"FILEMUIINFO_GET_CULTURE";
				"FILEMUIINFO_GET_MAIN_TYPEID"; (* accessing element of untyped parameter *)
				"FILEMUIINFO_GET_MAIN_TYPEIDS"; (* accessing element of untyped parameter *)
				"FILEMUIINFO_GET_MAIN_TYPENAMES"; (* accessing element of untyped parameter *)
				"FILEMUIINFO_GET_MUI_TYPEID"; (* accessing element of untyped parameter *)
				"FILEMUIINFO_GET_MUI_TYPEIDS"; (* accessing element of untyped parameter *)
				"FILEMUIINFO_GET_MUI_TYPENAMES"]]; (* accessing element of untyped parameter *)
		"winnt.h", [
			`undefined_macro, [
				"_MSC_VER"];
			`unparsible_macro, [
				"BitScanForward"; (* _BitScanForward is undefined on 32 bit mode *)
				"BitScanReverse"; (* _BitScanReverse is undefined on 32 bit mode *)
				"BitTest"; (* _bittest is undefined on 32 bit mode *)
				"BitTestAndComplement"; (* _bittestandcomplement is undefined on 32 bit mode *)
				"BitTestAndReset"; (* _bittestandreset is undefined on 32 bit mode *)
				"BitTestAndSet"; (* _bittestandset is undefined on 32 bit mode *)
				"C_ASSERT"; (* static assert *)
				"CacheLineFlush"; (* _mm_clflush *)
				"CONTAINING_RECORD"; (* parameterized field *)
				"FIELD_OFFSET"; (* parameterized field *)
				"IFACEMETHODIMP_"; (* storage class and parameterized type *)
				"IFACEMETHODIMPV_"; (* storage class and parameterized type *)
				"InterlockedCompareExchange16"; (* _InterlockedCompareExchange16 is undefined on 32 bit mode *)
				"InterlockedDecrement16"; (* _InterlockedDecrement16 is undefined on 32 bit mode *)
				"InterlockedIncrement16"; (* _InterlockedIncrement16 is undefined on 32 bit mode *)
				"LoadFence"; (* _mm_lfence *)
				"MemoryFence"; (* _mm_mfence *)
				"NOP_FUNCTION"; (* (void)0 ??? *)
				"PF_NON_TEMPORAL_LEVEL_ALL"; (* _MM_HINT_NTA *)
				"PF_TEMPORAL_LEVEL_1"; (* _MM_HINT_T0 *)
				"PF_TEMPORAL_LEVEL_2"; (* _MM_HINT_T1 *)
				"PF_TEMPORAL_LEVEL_3"; (* _MM_HINT_T2 *)
				"PreFetchCacheLine"; (* _mm_prefetch *)
				"PrefetchForWrite"; (* _m_prefetchw *)
				"PROBE_ALIGNMENT"; (* new struct in macro *)
				"PROBE_ALIGNMENT32"; (* new struct in macro *)
				"ReadMxCsr"; (* _mm_getcsr *)
				"ReadForWriteAccess"; (* _m_prefetchw *)
				"ReadTimeStampCounter"; (* __rdtsc *)
				"REPARSE_GUID_DATA_BUFFER_HEADER_SIZE"; (* accessing element of null *)
				"RTL_BITS_OF_FIELD"; (* parameterized field *)
				"RTL_CONTAINS_FIELD"; (* parameterized field *)
				"RTL_FIELD_SIZE"; (* parameterized field *)
				"RTL_FIELD_TYPE"; (* parameterized field *)
				"RTL_NUMBER_OF_FIELD"; (* parameterized field *)
				"RTL_PADDING_BETWEEN_FIELDS"; (* parameterized field *)
				"RTL_SIZEOF_THROUGH_FIELD"; (* parameterized field *)
				"STDAPI"; (* storage class and type *)
				"STDAPI_"; (* storage class and parameterized type *)
				"STDAPIV"; (* storage class and type *)
				"STDAPIV_"; (* storage class and parameterized type *)
				"STDMETHODIMP_"; (* storage class and parameterized type *)
				"STDMETHODIMPV_"; (* storage class and parameterized type *)
				"StoreFence"; (* _mm_sfence *)
				"TEXT"; (* ## *)
				"TYPE_ALIGNMENT"; (* new struct in macro *)
				"WriteMxCsr"; (* _mm_setcsr *)
				"YieldProcessor"]]; (* _mm_pause *)
		"winternl.h", [
			`uninterpretable_macro, [
				"InitializeObjectAttributes"; (* accessing element of untyped parameter *)
				"INTERNAL_TS_ACTIVE_CONSOLE_ID"]]; (* dereference the literal number as pointer *)
		"winuser.h", [
			`undefined_macro, [
				"_WIN32_WCE"];
			`uninterpretable_macro, [
				"NEXTRAWINPUTBLOCK"; (* accessing element of untyped parameter *)
				"POINTSTOPOINT"; (* accessing element of untyped parameter *)
				"POINTTOPOINTS"; (* accessing element of untyped parameter *)
				"RAWINPUT_ALIGN"]]; (* QWORD is undefined on 64 bit mode *)
		"wtypes.h", [
			`uninterpretable_macro, [
				"CBPCLIPDATA"; (* accessing element of untyped parameter *)
				"DECIMAL_SETZERO"]]; (* accessing element of untyped parameter *)
		predefined_name, [
			`unparsible_macro, [
				"__declspec";
				"__USER_LABEL_PREFIX__"]]];
	"*-*-*", [
		(* gcc or libc *)
		"stddef.h", [
			`unparsible_macro, [
				"offsetof"]]; (* parameterized field *)
		"tgmath.h", [
			`unparsible_macro, [
				"acos"; (* magic macro *)
				"acosh"; (* magic macro *)
				"asin"; (* magic macro *)
				"asinh"; (* magic macro *)
				"atan"; (* magic macro *)
				"atan2"; (* magic macro *)
				"atanh"; (* magic macro *)
				"carg"; (* magic macro *)
				"cbrt"; (* magic macro *)
				"ceil"; (* magic macro *)
				"cimag"; (* magic macro *)
				"conj"; (* magic macro *)
				"copysign"; (* magic macro *)
				"cos"; (* magic macro *)
				"cosh"; (* magic macro *)
				"cproj"; (* magic macro *)
				"creal"; (* magic macro *)
				"erf"; (* magic macro *)
				"erfc"; (* magic macro *)
				"exp"; (* magic macro *)
				"exp2"; (* magic macro *)
				"expm1"; (* magic macro *)
				"fabs"; (* magic macro *)
				"fdim"; (* magic macro *)
				"floor"; (* magic macro *)
				"__floating_type"; (* glibc / magic macro *)
				"fma"; (* magic macro *)
				"fmax"; (* magic macro *)
				"fmin"; (* magic macro *)
				"fmod"; (* magic macro *)
				"frexp"; (* magic macro *)
				"hypot"; (* magic macro *)
				"ilogb"; (* magic macro *)
				"ldexp"; (* magic macro *)
				"lgamma"; (* magic macro *)
				"llrint"; (* magic macro *)
				"llround"; (* magic macro *)
				"log"; (* magic macro *)
				"log10"; (* magic macro *)
				"log1p"; (* magic macro *)
				"log2"; (* magic macro *)
				"logb"; (* magic macro *)
				"lrint"; (* magic macro *)
				"lround"; (* magic macro *)
				"nearbyint"; (* magic macro *)
				"nextafter"; (* magic macro *)
				"nexttoward"; (* magic macro *)
				"pow"; (* magic macro *)
				"remainder"; (* magic macro *)
				"remquo"; (* magic macro *)
				"rint"; (* magic macro *)
				"round"; (* magic macro *)
				"scalb"; (* glibc / magic macro *)
				"scalbln"; (* magic macro *)
				"scalbn"; (* magic macro *)
				"sin"; (* magic macro *)
				"sinh"; (* magic macro *)
				"sqrt"; (* magic macro *)
				"tan"; (* magic macro *)
				"tanh"; (* magic macro *)
				"__tg_choose"; (* magic macro *)
				"__tg_choose_2"; (* magic macro *)
				"__tg_choose_3"; (* magic macro *)
				"__tg_cplx"; (* magic macro *)
				"__tg_generic_full"; (* freebsd9 / magic macro *)
				"__tg_generic_simple"; (* freebsd9 / magic macro *)
				"__tg_dbl"; (* magic macro *)
				"__tg_impl_full"; (* freebsd8 / magic macro *)
				"__tg_impl_simple"; (* freebsd8 / magic macro *)
				"__tg_integer"; (* freebsd8 / magic macro *)
				"__tg_is_complex"; (* freebsd8 / magic macro *)
				"__tg_ldbl"; (* magic macro *)
				"__tg_type"; (* freebsd8 / magic macro *)
				"__tg_type_corr"; (* freebsd8 / magic macro *)
				"__tg_type3"; (* freebsd8 / magic macro *)
				"__TGMATH_RCFUNCS"; (* glibc / parameterized declaration *)
				"__tgmath_real_type"; (* glibc / magic macro *)
				"__tgmath_real_type_sub"; (* glibc / magic macro *)
				"tgamma"; (* magic macro *)
				"trunc"]]; (* magic macro *)
		(* cairo *)
		"cairo-deprecated.h", [
			`unparsible_macro, [
				"cairo_atsui_font_face_create_for_atsu_font_id"; (* undefined *)
				"cairo_concat_matrix"; (* undefined *)
				"cairo_copy"; (* undefined *)
				"cairo_current_fill_rule"; (* undefined *)
				"cairo_current_font_extents"; (* undefined *)
				"cairo_current_line_cap"; (* undefined *)
				"cairo_current_line_join"; (* undefined *)
				"cairo_current_line_width"; (* undefined *)
				"cairo_current_matrix"; (* undefined *)
				"cairo_current_miter_limit"; (* undefined *)
				"cairo_current_operator"; (* undefined *)
				"cairo_current_path"; (* undefined *)
				"cairo_current_path_flat"; (* undefined *)
				"cairo_current_point"; (* undefined *)
				"cairo_current_target_surface"; (* undefined *)
				"cairo_current_tolerance"; (* undefined *)
				"cairo_default_matrix"; (* undefined *)
				"cairo_get_font_extents"; (* undefined *)
				"cairo_get_path"; (* undefined *)
				"cairo_get_path_flat"; (* undefined *)
				"cairo_get_status"; (* undefined *)
				"cairo_get_status_string"; (* undefined *)
				"cairo_init_clip"; (* undefined *)
				"cairo_inverse_transform_distance"; (* undefined *)
				"cairo_inverse_transform_point"; (* undefined *)
				"cairo_matrix_copy"; (* undefined *)
				"cairo_matrix_create"; (* undefined *)
				"cairo_matrix_destroy"; (* undefined *)
				"cairo_matrix_get_affine"; (* undefined *)
				"cairo_matrix_set_affine"; (* undefined *)
				"cairo_matrix_set_identity"; (* undefined *)
				"cairo_pattern_add_color_stop"; (* undefined *)
				"cairo_pdf_surface_set_dpi"; (* undefined *)
				"cairo_ps_surface_set_dpi"; (* undefined *)
				"cairo_scale_font"; (* undefined *)
				"cairo_select_font"; (* undefined *)
				"cairo_set_alpha"; (* undefined *)
				"cairo_set_pattern"; (* undefined *)
				"cairo_set_rgb_color"; (* undefined *)
				"cairo_set_target_drawable"; (* undefined *)
				"cairo_set_target_glitz"; (* undefined *)
				"cairo_set_target_image"; (* undefined *)
				"cairo_set_target_pdf"; (* undefined *)
				"cairo_set_target_png"; (* undefined *)
				"cairo_set_target_ps"; (* undefined *)
				"cairo_set_target_quartz"; (* undefined *)
				"cairo_set_target_surface"; (* undefined *)
				"cairo_set_target_win32"; (* undefined *)
				"cairo_set_target_xcb"; (* undefined *)
				"cairo_show_surface"; (* undefined *)
				"cairo_status_string"; (* undefined *)
				"cairo_surface_create_for_image"; (* undefined *)
				"cairo_surface_get_filter"; (* undefined *)
				"cairo_surface_get_matrix"; (* undefined *)
				"cairo_surface_set_filter"; (* undefined *)
				"cairo_surface_set_matrix"; (* undefined *)
				"cairo_surface_set_repeat"; (* undefined *)
				"cairo_svg_surface_set_dpi"; (* undefined *)
				"cairo_transform_distance"; (* undefined *)
				"cairo_transform_font"; (* undefined *)
				"cairo_transform_point"; (* undefined *)
				"cairo_xcb_surface_create_for_pixmap_with_visual"; (* undefined *)
				"cairo_xcb_surface_create_for_window_with_visual"; (* undefined *)
				"cairo_xlib_surface_create_for_pixmap_with_visual"; (* undefined *)
				"cairo_xlib_surface_create_for_window_with_visual"]]; (* undefined *)
		(* expat *)
		"expat.h", [
			`unparsible_macro, [
				"XML_STATUS_ERROR"; (* conflicated with enum element *)
				"XML_STATUS_OK"; (* conflicated with enum element *)
				"XML_STATUS_SUSPENDED"]]; (* conflicated with enum element *)
		"expat_external.h", [
			`unparsible_macro, [
				"XMLPARSEAPI"]]; (* storage class and parameterized type *)
		(* Boehm-GC *)
		"gc/gc.h", [
			`unparsible_macro, [
				"GC_EXTRAS"; (* parameter list *)
				"GC_EXTRA_PARAMS"]]; (* formal parameter list *)
		"gc/gc_typed.h", [
			`unparsible_macro, [
				"GC_WORD_OFFSET"]]; (* parameterized field *)
		(* GMP *)
		"gmp.h", [
			`unparsible_macro, [
				"__GMP_CAST"; (* unclear cast or function call *)
				"__GMP_DECLSPEC_EXPORT"; (* __declspec in no Windows *)
				"__GMP_DECLSPEC_IMPORT"; (* __declspec in no Windows *)
				"gmp_fprintf"; (* #include <stdio.h> *)
				"gmp_fscanf"; (* #include <stdio.h> *)
				"__GMPN_ADD"; (* partial function *)
				"__GMPN_ADD_1"; (* multi-statements *)
				"__GMPN_AORS"; (* partial function *)
				"__GMPN_AORS_1"; (* multi-statements *)
				"__GMPN_SUB"; (* partial function *)
				"__GMPN_SUB_1"; (* multi-statements *)
				"gmp_obstack_printf"; (* #include <stdio.h> *)
				"gmp_obstack_vprintf"; (* #include <stdio.h> *)
				"gmp_vasprintf"; (* #include <stdio.h> *)
				"gmp_vfprintf"; (* #include <stdio.h> *)
				"gmp_vfscanf"; (* #include <stdio.h> *)
				"gmp_vprintf"; (* #include <stdio.h> *)
				"gmp_vscanf"; (* #include <stdio.h> *)
				"gmp_vsnprintf"; (* #include <stdio.h> *)
				"gmp_vsprintf"; (* #include <stdio.h> *)
				"gmp_vsscanf"; (* #include <stdio.h> *)
				"__GMPZ_FITS_UTYPE_P"; (* multi-statements *)
				"mpf_inp_str"; (* #include <stdio.h> *)
				"mpf_out_str"; (* #include <stdio.h> *)
				"mpq_inp_str"; (* #include <stdio.h> *)
				"mpq_out_str"; (* #include <stdio.h> *)
				"mpz_inp_raw"; (* #include <stdio.h> *)
				"mpz_inp_str"; (* #include <stdio.h> *)
				"MPZ_ROINIT_N"; (* parameterized initializer *)
				"mpz_out_raw"; (* #include <stdio.h> *)
				"mpz_out_str"]; (* #include <stdio.h> *)
			`uninterpretable_macro, [
				"mpf_sgn"; (* accessing element of untyped parameter *)
				"mpq_cmp_si"; (* accessing element of untyped parameter *)
				"mpq_cmp_ui"; (* accessing element of untyped parameter *)
				"mpq_denref"; (* accessing element of untyped parameter *)
				"mpq_numref"; (* accessing element of untyped parameter *)
				"mpq_sgn"; (* accessing element of untyped parameter *)
				"mpz_cmp_si"; (* accessing element of untyped parameter *)
				"mpz_cmp_ui"; (* accessing element of untyped parameter *)
				"mpz_even_p"; (* accessing element of untyped parameter *)
				"mpz_odd_p"; (* accessing element of untyped parameter *)
				"mpz_sgn"]]; (* accessing element of untyped parameter *)
		(* libxml2 *)
		"libxml/tree.h", [
			`uninterpretable_macro, [
				"XML_GET_CONTENT"]]; (* accessing element of untyped parameter *)
		"libxml/xmlstring.h", [
			`unparsible_macro, [
				"BAD_CAST"]]; (* typename with paren *)
		"libxml/xmlversion.h", [
			`unparsible_macro, [
				"LIBXML_TEST_VERSION"]]; (* extra semicolon *)
		(* MPC *)
		"mpc.h", [
			`unparsible_macro, [
				"mpfr_exp_t"; (* conflicted with typedef *)
				"__MPC_PROTO"]; (* parameter list *)
			`uninterpretable_macro, [
				"mpc_imagref"; (* accessing element of untyped parameter *)
				"mpc_realref"; (* accessing element of untyped parameter *)
				"mpfr_set_fr"]]; (* accessing element of untyped parameter *)
		(* MPFR *)
		"mpfr.h", [
			`unparsible_macro, [
				"__mpfr_default_fp_bit_precision"; (* bug? mpfr_get_default_fp_bit_precision was undefined *)
				"_MPFR_PROTO"]; (* parameter list *)
			`uninterpretable_macro, [
				"mpfr_add_one_ulp"; (* accessing element of untyped parameter *)
				"mpfr_custom_get_mantissa"; (* accessing element of untyped parameter *)
				"mpfr_custom_get_significand"; (* accessing element of untyped parameter *)
				"mpfr_init_set"; (* accessing element of untyped parameter *)
				"mpfr_set"; (* accessing element of untyped parameter *)
				"mpfr_sub_one_ulp"; (* accessing element of untyped parameter *)
				"MPFR_SIGN"]]; (* accessing element of untyped parameter *)
		(* OpenSSL *)
		"openssl/opensslconf.h", [
			`unparsible_macro, [
				"DECLARE_DEPRECATED"; (* attribute with an identifier *)
				"DEPRECATEDIN_0_9_8"; (* use the above *)
				"DEPRECATEDIN_1_0_0";
				"DEPRECATEDIN_1_1_0";
				"NON_EMPTY_TRANSLATION_UNIT"; (* declaration *)
				"OPENSSL_UNISTD"]]; (* header *)
		(* libpng *)
		"png.h", [
			`unparsible_macro, [
				"PNG_GAMMA_THRESHOLD"; (* PNG_GAMMA_THRESHOLD_FIXED is undefined *)
				"PNG_IMAGE_BUFFER_SIZE"; (* accessing element of untyped parameter *)
				"PNG_IMAGE_COMPRESSED_SIZE_MAX"; (* accessing element of untyped parameter *)
				"PNG_IMAGE_COLORMAP_SIZE"; (* accessing element of untyped parameter *)
				"PNG_IMAGE_DATA_SIZE"; (* accessing element of untyped parameter *)
				"PNG_IMAGE_FAILED"; (* accessing element of untyped parameter *)
				"PNG_IMAGE_PIXEL_"; (* calling the untyped parameter as function *)
				"PNG_IMAGE_PNG_SIZE_MAX"; (* accessing element of untyped parameter *)
				"PNG_IMAGE_PNG_SIZE_MAX_"; (* accessing element of untyped parameter *)
				"PNG_IMAGE_ROW_STRIDE"; (* accessing element of untyped parameter *)
				"PNG_IMAGE_SIZE"; (* accessing element of untyped parameter *)
				"PNG_READ_16_TO_8"]]; (* SUPPORTED is undefined *)
		"pngconf.h", [
			`unparsible_macro, [
				"png_benign_error"; (* circular dependency *)
				"PNG_CALLBACK"; (* parameterized declaration *)
				"png_chunk_benign_error"; (* circular dependency *)
				"PNG_EXPORT"; (* parameterized declaration *)
				"PNG_EXPORTA"; (* parameterized declaration *)
				"PNG_FIXED_EXPORT"; (* parameterized declaration *)
				"PNG_FP_EXPORT"; (* parameterized declaration *)
				"PNG_FUNCTION"; (* parameterized declaration *)
				"png_sprintf"; (* sprintf is undefined, #include <stdio.h> *)
				"png_snprintf"; (* snprintf is undefined, #include <stdio.h> *)
				"png_snprintf2"; (* snprintf is undefined, #include <stdio.h> *)
				"png_snprintf6"]]; (* snprintf is undefined, #include <stdio.h> *)
		"zconf.h", [
			`undefined_macro, [
				"_LARGEFILE64_SOURCE"]]; (* 1.2.5 *)
		"zlib.h", [
			`undefined_macro, [
				"_FILE_OFFSET_BITS"]]]];; (* 1.2.5 *)

let make_is_known_error
	(target: string)
	(remove_include_dir: string -> string)
	: ranged_position -> string -> known_error -> bool =
(
	let header_num = List.fold_left (fun r (_, hs) -> r + List.length hs) 0 known_error_table in
	let header_table: (string, (string, known_error) Hashtbl.t) Hashtbl.t = Hashtbl.create header_num in
	List.iter (fun (pattern, hs) ->
		if match_target ~pattern target then (
			List.iter (fun (header_name, es) ->
				assert (not (Hashtbl.mem header_table header_name));
				let symbol_num = List.fold_left (fun r (_, ss) -> r + List.length ss) 0 es in
				let symbol_table: (string, known_error) Hashtbl.t = Hashtbl.create symbol_num in
				List.iter (fun (k, ss) ->
					List.iter (fun s ->
						assert (not (Hashtbl.mem symbol_table s));
						Hashtbl.add symbol_table s k
					) ss
				) es;
				Hashtbl.add header_table header_name symbol_table
			) hs
		)
	) known_error_table;
	let is_known_error (ps: ranged_position) (s: string) (k: known_error): bool = (
		let (filename, _, _, _), _ = ps in
		let header_name = remove_include_dir filename in
		begin match Hashtbl.find_opt header_table header_name with
		| Some symbol_table ->
			begin match Hashtbl.find_opt symbol_table s with
			| Some symbol_k ->
				symbol_k = k
			| None ->
				false
			end
		| None ->
			false
		end
	) in
	is_known_error
);;
