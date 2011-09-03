#if defined(__FreeBSD__)
#include <sys/_types.h> /* __time_t */
#include <sys/timespec.h> /* struct timespec */
#endif
#include <pthread.h>
#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
