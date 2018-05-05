#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#elif defined(__APPLE__)
#include <string.h> /* bcopy */
#include <sys/time.h> /* struct timeval */
#include <sys/termios.h> /* struct termios */
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

#include <stdio.h>
#include <readline/readline.h>

#pragma for Ada "_ctype.h" include "runetype.h"
/* with clause in package body is unimplemented, force to with. */
