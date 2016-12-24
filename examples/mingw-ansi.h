#define WIN32_LEAN_AND_MEAN

#include <limits.h> /* before winuser.h */
#include <basetsd.h>
/* avoiding circular dependency between windef.h and winnt.h */
#define NT_INCLUDED
#define SHORT short
#define LONG long
#define HANDLE void *
#define DECLARE_HANDLE(name) \
	struct name##__ { int unused; }; \
	typedef struct name##__ *name
#include <windef.h>
#undef DECLARE_HANDLE
#undef HANDLE
#undef LONG
#undef SHORT
#undef NT_INCLUDED
#define _X86INTRIN_H_INCLUDED
#include <winnt.h>
#undef _X86INTRIN_H_INCLUDED
#define NOWINBASEINTERLOCK
#include <windows.h>
#undef NOWINBASEINTERLOCK
#include <winsock2.h>
/* #pragma for Ada "winsock2.h" monolithic_include "psdk_inc/_wsa_errnos.h" */
#include <winternl.h> /* before wincrypt.h */
#include <wincrypt.h>
#include <ws2tcpip.h>

#undef WIN32_LEAN_AND_MEAN
