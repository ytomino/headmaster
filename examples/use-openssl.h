#if defined(__FreeBSD__)
#include <stdint.h> /* uintptr_t is used in <sys/cdefs.h> */
#endif

#define OPENSSL_NO_DEPRECATED
#define OPENSSL_NO_FILENAMES
#include <openssl/md5.h>
#include <openssl/sha.h>
#undef OPENSSL_NO_DEPRECATED
#undef OPENSSL_NO_FILENAMES
