#if defined(__FreeBSD__)
#define _DONT_USE_CTYPE_INLINE_
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#elif defined(__APPLE__)
#include <sys/resource.h> /* avoiding circular dependency */
#include <sys/signal.h> /* avoiding circular dependency */
#endif

#include <libxml/xmlversion.h>
#undef LIBXML_EXPR_ENABLED
/* unremovable circular dependency is in xmlregexp.h and xmltree.h */
#include <libxml/xmlreader.h>
#include <libxml/xmlwriter.h>

#if defined(__FreeBSD__)
#undef _CurrentRuneLocale /* conflicted */
#undef _DONT_USE_CTYPE_INLINE_
#endif
