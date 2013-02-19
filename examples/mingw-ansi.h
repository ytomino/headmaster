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
#include <winnt.h>
#include <winsock2.h> /* before windows.h */
#include <windows.h>
#include <wincrypt.h>

#undef WIN32_LEAN_AND_MEAN

/* #pragma for Ada "winsock2.h" monolithic_include "psdk_inc/_wsa_errnos.h" */
#undef h_errno
