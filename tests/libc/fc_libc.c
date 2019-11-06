/* run.config*
   EXECNOW: make -s @PTEST_DIR@/check_libc_naming_conventions.cmxs
   EXECNOW: make -s @PTEST_DIR@/check_const.cmxs
   EXECNOW: make -s @PTEST_DIR@/check_parsing_individual_headers.cmxs
   EXECNOW: make -s @PTEST_DIR@/check_libc_anonymous_tags.cmxs
   EXECNOW: make -s @PTEST_DIR@/check_compliance.cmxs
   OPT: -load-module @PTEST_DIR@/check_libc_naming_conventions -print -cpp-extra-args='-nostdinc -Ishare/libc' -metrics -metrics-libc -load-module @PTEST_DIR@/check_const -load-module metrics -eva @EVA_CONFIG@ -then -lib-entry -no-print -metrics-no-libc
   OPT: -print -print-libc
   OPT: -load-module @PTEST_DIR@/check_parsing_individual_headers
   OPT: -load-module @PTEST_DIR@/check_libc_anonymous_tags
   OPT: -load-module @PTEST_DIR@/check_compliance -kernel-msg-key printer:attrs
   CMD: ./tests/libc/check_full_libc.sh
   OPT:
**/
#define __FC_REG_TEST

// Some functions such as usleep() are only defined for older of POSIX headers,
// while others may be defined only by newer ones, so it is not possible to
// test all of them. We nevertheless define some headers to test additional
// functions.
#define _XOPEN_SOURCE 600
#define _POSIX_C_SOURCE 200112L
#define _GNU_SOURCE 1

#include "share/libc/__fc_runtime.c"

#include "alloca.h"
#include "arpa/inet.h"
#include "assert.h"
#include "byteswap.h"
#include "complex.h"
#include "ctype.h"
#include "dirent.h"
#include "dlfcn.h"
#include "endian.h"
#include "errno.h"
#include "__fc_alloc_axiomatic.h"
#include "__fc_builtin.h"
#include "__fc_define_blkcnt_t.h"
#include "__fc_define_blksize_t.h"
#include "__fc_define_clockid_t.h"
#include "__fc_define_dev_t.h"
#include "__fc_define_eof.h"
#include "__fc_define_fd_set_t.h"
#include "__fc_define_file.h"
#include "__fc_define_fpos_t.h"
#include "__fc_define_id_t.h"
#include "__fc_define_ino_t.h"
#include "__fc_define_intptr_t.h"
#include "__fc_define_iovec.h"
#include "__fc_define_key_t.h"
#include "__fc_define_mode_t.h"
#include "__fc_define_nlink_t.h"
#include "__fc_define_null.h"
#include "__fc_define_off_t.h"
#include "__fc_define_pid_t.h"
#include "__fc_define_pthread_types.h"
#include "__fc_define_sa_family_t.h"
#include "__fc_define_seek_macros.h"
#include "__fc_define_sigset_t.h"
#include "__fc_define_size_t.h"
#include "__fc_define_sockaddr.h"
#include "__fc_define_ssize_t.h"
#include "__fc_define_stat.h"
#include "__fc_define_suseconds_t.h"
#include "__fc_define_timespec.h"
#include "__fc_define_time_t.h"
#include "__fc_define_timer_t.h"
#include "__fc_define_uid_and_gid.h"
#include "__fc_define_useconds_t.h"
#include "__fc_define_wchar_t.h"
#include "__fc_define_wint_t.h"
#include "__fc_gcc_builtins.h"
#include "__fc_inet.h"
#include "__fc_machdep.h"
//#include "__fc_machdep_linux_shared.h"
#include "fcntl.h"
#include "__fc_select.h"
#include "__fc_string_axiomatic.h"
#include "features.h"
#include "fenv.h"
#include "float.h"
#include "fnmatch.h"
#include "ftw.h"
#include "getopt.h"
#include "glob.h"
#include "grp.h"
#include "iconv.h"
#include "ifaddrs.h"
#include "inttypes.h"
#include "iso646.h"
#include "libgen.h"
#include "limits.h"
#include "locale.h"
#include "malloc.h"
#include "math.h"
#include "memory.h"
#include "netdb.h"
#include "net/if.h"
#include "netinet/in.h"
#include "netinet/tcp.h"
#include "nl_types.h"
#include "poll.h"
#include "pthread.h"
#include "pwd.h"
#include "regex.h"
#include "resolv.h"
#include "sched.h"
#include "semaphore.h"
#include "setjmp.h"
#include "signal.h"
#include "stdarg.h"
#include "stdbool.h"
#include "stddef.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "strings.h"
#include "stropts.h"
#include "sys/file.h"
#include "sys/ioctl.h"
#include "sys/ipc.h"
#include "syslog.h"
#include "sys/mman.h"
#include "sys/random.h"
#include "sys/resource.h"
#include "sys/select.h"
#include "sys/shm.h"
#include "sys/signal.h"
#include "sys/socket.h"
#include "sys/stat.h"
#include "sys/time.h"
#include "sys/times.h"
#include "sys/timex.h"
#include "sys/types.h"
#include "sys/uio.h"
#include "sys/un.h"
#include "sys/utsname.h"
#include "sys/wait.h"
#include "termios.h"
#include "tgmath.h"
#include "time.h"
#include "unistd.h"
#include "utime.h"
#include "utmpx.h"
#include "wchar.h"
#include "wctype.h"

void main() {
  /* The variables below must be const; otherwise the preconditions
     and the assigns/from of some functions will not match */
  //@ assert __fc_p_fopen == (FILE *)&__fc_fopen;
  //@ assert __fc_p_opendir == (DIR*)&__fc_opendir;
  //@ assert __fc_p_time_tm == &__fc_time_tm;
  //@ assert __fc_p_strerror == __fc_strerror;
}
