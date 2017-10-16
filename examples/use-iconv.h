#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#define _DONT_USE_CTYPE_INLINE_
#endif

#include <iconv.h>
#include <errno.h>

#ifdef __APPLE__
#pragma for Ada overload size_t iconv (iconv_t cd, \
	const char **inbuf, size_t *inbytesleft, \
	char **outbuf, size_t *outbytesleft)
#pragma for Ada "errno.h" include "sys/errno.h"
#elif defined(__FreeBSD__)
#undef _DONT_USE_CTYPE_INLINE_
#endif
