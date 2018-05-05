#if defined(__FreeBSD__)
#define _DONT_USE_CTYPE_INLINE_
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#elif defined(__APPLE__)
#include <sys/resource.h> /* avoiding circular dependency */
#include <sys/signal.h> /* avoiding circular dependency */
#elif defined(__gnu_linux__)
#if !defined(_BITS_LIBIO_H)
#include <features.h> /* __GLIBC_PREREQ */
#if __GLIBC_PREREQ(2, 27)
#define _LIBIO_H
#include <bits/libio.h> /* before stdio.h */
#undef _LIBIO_H
#endif
#endif
#endif

#include <libxml/xmlversion.h>
#undef LIBXML_EXPR_ENABLED
#if defined(LIBXML_ICU_ENABLED)
#undef LIBXML_ICU_ENABLED
#endif
/* unremovable circular dependency is in xmlregexp.h and xmltree.h */
#include <libxml/xmlreader.h>
#include <libxml/xmlwriter.h>

#if defined(__FreeBSD__)
#undef _CurrentRuneLocale /* conflicted */
#undef _DONT_USE_CTYPE_INLINE_
#endif
