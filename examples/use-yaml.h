#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#elif defined(__APPLE__)
#include <sys/types.h> /* avoiding circular dependency */
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

#include <yaml.h>
