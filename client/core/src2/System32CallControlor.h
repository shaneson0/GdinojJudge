
#ifndef SYSTEM_CALL_CONTROLOR
#define SYSTEM_CALL_CONTROLOR

#include <sys/syscall.h>

int LANG_CV[256] = { 85, 8, SYS_time, SYS_read, SYS_uname, SYS_write, SYS_open,
		SYS_close, SYS_execve, SYS_access, SYS_brk, SYS_munmap, SYS_mprotect,
		SYS_mmap2, SYS_fstat64, SYS_set_thread_area, 252, 0   };

#endif