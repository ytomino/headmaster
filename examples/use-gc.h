#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#endif

#include <gc/gc.h>
#include <gc/gc_typed.h>

#if defined(GC_RETURN_ADDR)
#undef GC_RETURN_ADDR
#endif
#if defined(GC_RETURN_ADDR_PARENT)
#undef GC_RETURN_ADDR_PARENT
#endif
