#if defined(__APPLE__)
#include <sys/types.h> /* avoiding circular dependency */
#elif defined(__FreeBSD__)
#include <sys/_types.h> /* __time_t for sys/timespec.h */
#include <sys/timespec.h> /* avoiding circular dependency */
#elif defined (__MINGW32__)
#include <sys/types.h> /* _dev_t for _mingw_stat64.h */
#include <sys/stat.h> /* avoiding circular dependency */
#endif

#include <stddef.h>
#include <assert.h>
#include <complex.h>
#include <ctype.h>
#include <errno.h>
#include <fenv.h>
#include <float.h>
#include <inttypes.h>
#include <iso646.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <setjmp.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>
#include <time.h>
#include <wchar.h>
#include <wctype.h>
