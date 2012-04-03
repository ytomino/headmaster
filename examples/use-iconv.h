#include <iconv.h>
#include <errno.h>
#ifdef __APPLE__
#pragma for Ada overload size_t iconv (iconv_t cd, \
	const char **inbuf, size_t *inbytesleft, \
	char **outbuf, size_t *outbytesleft)
#endif
#pragma for Ada "errno.h" include "sys/errno.h"
