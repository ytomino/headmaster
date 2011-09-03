#if defined(__FreeBSD__)
#include <sys/_types.h> /* __time_t */
#include <sys/timespec.h> /* struct timespec */
#include <sys/param.h>
#endif
#include <sys/fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>
#include <pthread.h>
#pragma instance pthread_rwlock_t "PTHREAD_RWLOCK_INITIALIZER"
#pragma instance pthread_mutex_t "PTHREAD_MUTEX_INITIALIZER"
#pragma instance pthread_cond_t "PTHREAD_COND_INITIALIZER"
#pragma instance pthread_once_t "PTHREAD_ONCE_INIT"
