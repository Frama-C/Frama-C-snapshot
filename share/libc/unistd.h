/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_UNISTD
#define __FC_UNISTD

#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_string_axiomatic.h"
#include "__fc_define_size_t.h"
#include "__fc_define_null.h"
#include "__fc_define_ssize_t.h"
#include "__fc_define_uid_and_gid.h"
#include "__fc_define_off_t.h"
#include "__fc_define_pid_t.h"
#include "__fc_define_useconds_t.h"
#include "__fc_define_intptr_t.h"



#include "limits.h"

extern volatile int Frama_C_entropy_source;

/* Values for the second argument to access.
   These may be OR'd together.  */
#define R_OK    4               /* Test for read permission.  */
#define W_OK    2               /* Test for write permission.  */
#define X_OK    1               /* Test for execute permission.  */
#define F_OK    0               /* Test for existence.  */

/* Standard file descriptors.  */
#define	STDIN_FILENO	0	/* Standard input.  */
#define	STDOUT_FILENO	1	/* Standard output.  */
#define	STDERR_FILENO	2	/* Standard error output.  */

#include "__fc_define_seek_macros.h"

/* compatibility macros */

#ifndef __FC_NO_MONOTONIC_CLOCK
/* 0 indicates that the feature is supported at compile time, but might not
   be supported at runtime. Frama-C can't make promises about the runtime
   environment.
*/
#define _POSIX_MONOTONIC_CLOCK 0
#endif

__BEGIN_DECLS

/* Values for the NAME argument to `pathconf' and `fpathconf'.  */
enum __fc_pathconf_name
  {
    _PC_LINK_MAX,
#define	_PC_LINK_MAX			_PC_LINK_MAX
    _PC_MAX_CANON,
#define	_PC_MAX_CANON			_PC_MAX_CANON
    _PC_MAX_INPUT,
#define	_PC_MAX_INPUT			_PC_MAX_INPUT
    _PC_NAME_MAX,
#define	_PC_NAME_MAX			_PC_NAME_MAX
    _PC_PATH_MAX,
#define	_PC_PATH_MAX			_PC_PATH_MAX
    _PC_PIPE_BUF,
#define	_PC_PIPE_BUF			_PC_PIPE_BUF
    _PC_CHOWN_RESTRICTED,
#define	_PC_CHOWN_RESTRICTED		_PC_CHOWN_RESTRICTED
    _PC_NO_TRUNC,
#define	_PC_NO_TRUNC			_PC_NO_TRUNC
    _PC_VDISABLE,
#define _PC_VDISABLE			_PC_VDISABLE
    _PC_SYNC_IO,
#define	_PC_SYNC_IO			_PC_SYNC_IO
    _PC_ASYNC_IO,
#define	_PC_ASYNC_IO			_PC_ASYNC_IO
    _PC_PRIO_IO,
#define	_PC_PRIO_IO			_PC_PRIO_IO
    _PC_SOCK_MAXBUF,
#define	_PC_SOCK_MAXBUF			_PC_SOCK_MAXBUF
    _PC_FILESIZEBITS,
#define _PC_FILESIZEBITS		_PC_FILESIZEBITS
    _PC_REC_INCR_XFER_SIZE,
#define _PC_REC_INCR_XFER_SIZE		_PC_REC_INCR_XFER_SIZE
    _PC_REC_MAX_XFER_SIZE,
#define _PC_REC_MAX_XFER_SIZE		_PC_REC_MAX_XFER_SIZE
    _PC_REC_MIN_XFER_SIZE,
#define _PC_REC_MIN_XFER_SIZE		_PC_REC_MIN_XFER_SIZE
    _PC_REC_XFER_ALIGN,
#define _PC_REC_XFER_ALIGN		_PC_REC_XFER_ALIGN
    _PC_ALLOC_SIZE_MIN,
#define _PC_ALLOC_SIZE_MIN		_PC_ALLOC_SIZE_MIN
    _PC_SYMLINK_MAX,
#define _PC_SYMLINK_MAX			_PC_SYMLINK_MAX
    _PC_2_SYMLINKS
#define _PC_2_SYMLINKS			_PC_2_SYMLINKS
  };

/* Values for the argument to `sysconf'.  */
enum __fc_sysconf_name
  {
    _SC_ARG_MAX,
#define	_SC_ARG_MAX			_SC_ARG_MAX
    _SC_CHILD_MAX,
#define	_SC_CHILD_MAX			_SC_CHILD_MAX
    _SC_CLK_TCK,
#define	_SC_CLK_TCK			_SC_CLK_TCK
    _SC_NGROUPS_MAX,
#define	_SC_NGROUPS_MAX			_SC_NGROUPS_MAX
    _SC_OPEN_MAX,
#define	_SC_OPEN_MAX			_SC_OPEN_MAX
    _SC_STREAM_MAX,
#define	_SC_STREAM_MAX			_SC_STREAM_MAX
    _SC_TZNAME_MAX,
#define	_SC_TZNAME_MAX			_SC_TZNAME_MAX
    _SC_JOB_CONTROL,
#define	_SC_JOB_CONTROL			_SC_JOB_CONTROL
    _SC_SAVED_IDS,
#define	_SC_SAVED_IDS			_SC_SAVED_IDS
    _SC_REALTIME_SIGNALS,
#define	_SC_REALTIME_SIGNALS		_SC_REALTIME_SIGNALS
    _SC_PRIORITY_SCHEDULING,
#define	_SC_PRIORITY_SCHEDULING		_SC_PRIORITY_SCHEDULING
    _SC_TIMERS,
#define	_SC_TIMERS			_SC_TIMERS
    _SC_ASYNCHRONOUS_IO,
#define	_SC_ASYNCHRONOUS_IO		_SC_ASYNCHRONOUS_IO
    _SC_PRIORITIZED_IO,
#define	_SC_PRIORITIZED_IO		_SC_PRIORITIZED_IO
    _SC_SYNCHRONIZED_IO,
#define	_SC_SYNCHRONIZED_IO		_SC_SYNCHRONIZED_IO
    _SC_FSYNC,
#define	_SC_FSYNC			_SC_FSYNC
    _SC_MAPPED_FILES,
#define	_SC_MAPPED_FILES		_SC_MAPPED_FILES
    _SC_MEMLOCK,
#define	_SC_MEMLOCK			_SC_MEMLOCK
    _SC_MEMLOCK_RANGE,
#define	_SC_MEMLOCK_RANGE		_SC_MEMLOCK_RANGE
    _SC_MEMORY_PROTECTION,
#define	_SC_MEMORY_PROTECTION		_SC_MEMORY_PROTECTION
    _SC_MESSAGE_PASSING,
#define	_SC_MESSAGE_PASSING		_SC_MESSAGE_PASSING
    _SC_SEMAPHORES,
#define	_SC_SEMAPHORES			_SC_SEMAPHORES
    _SC_SHARED_MEMORY_OBJECTS,
#define	_SC_SHARED_MEMORY_OBJECTS	_SC_SHARED_MEMORY_OBJECTS
    _SC_AIO_LISTIO_MAX,
#define	_SC_AIO_LISTIO_MAX		_SC_AIO_LISTIO_MAX
    _SC_AIO_MAX,
#define	_SC_AIO_MAX			_SC_AIO_MAX
    _SC_AIO_PRIO_DELTA_MAX,
#define	_SC_AIO_PRIO_DELTA_MAX		_SC_AIO_PRIO_DELTA_MAX
    _SC_DELAYTIMER_MAX,
#define	_SC_DELAYTIMER_MAX		_SC_DELAYTIMER_MAX
    _SC_MQ_OPEN_MAX,
#define	_SC_MQ_OPEN_MAX			_SC_MQ_OPEN_MAX
    _SC_MQ_PRIO_MAX,
#define	_SC_MQ_PRIO_MAX			_SC_MQ_PRIO_MAX
    _SC_VERSION,
#define	_SC_VERSION			_SC_VERSION
    _SC_PAGESIZE,
#define	_SC_PAGESIZE			_SC_PAGESIZE
#define	_SC_PAGE_SIZE			_SC_PAGESIZE
    _SC_RTSIG_MAX,
#define	_SC_RTSIG_MAX			_SC_RTSIG_MAX
    _SC_SEM_NSEMS_MAX,
#define	_SC_SEM_NSEMS_MAX		_SC_SEM_NSEMS_MAX
    _SC_SEM_VALUE_MAX,
#define	_SC_SEM_VALUE_MAX		_SC_SEM_VALUE_MAX
    _SC_SIGQUEUE_MAX,
#define	_SC_SIGQUEUE_MAX		_SC_SIGQUEUE_MAX
    _SC_TIMER_MAX,
#define	_SC_TIMER_MAX			_SC_TIMER_MAX

    /* Values for the argument to `sysconf'
       corresponding to _POSIX2_* symbols.  */
    _SC_BC_BASE_MAX,
#define	_SC_BC_BASE_MAX			_SC_BC_BASE_MAX
    _SC_BC_DIM_MAX,
#define	_SC_BC_DIM_MAX			_SC_BC_DIM_MAX
    _SC_BC_SCALE_MAX,
#define	_SC_BC_SCALE_MAX		_SC_BC_SCALE_MAX
    _SC_BC_STRING_MAX,
#define	_SC_BC_STRING_MAX		_SC_BC_STRING_MAX
    _SC_COLL_WEIGHTS_MAX,
#define	_SC_COLL_WEIGHTS_MAX		_SC_COLL_WEIGHTS_MAX
    _SC_EQUIV_CLASS_MAX,
#define	_SC_EQUIV_CLASS_MAX		_SC_EQUIV_CLASS_MAX
    _SC_EXPR_NEST_MAX,
#define	_SC_EXPR_NEST_MAX		_SC_EXPR_NEST_MAX
    _SC_LINE_MAX,
#define	_SC_LINE_MAX			_SC_LINE_MAX
    _SC_RE_DUP_MAX,
#define	_SC_RE_DUP_MAX			_SC_RE_DUP_MAX
    _SC_CHARCLASS_NAME_MAX,
#define	_SC_CHARCLASS_NAME_MAX		_SC_CHARCLASS_NAME_MAX

    _SC_2_VERSION,
#define	_SC_2_VERSION			_SC_2_VERSION
    _SC_2_C_BIND,
#define	_SC_2_C_BIND			_SC_2_C_BIND
    _SC_2_C_DEV,
#define	_SC_2_C_DEV			_SC_2_C_DEV
    _SC_2_FORT_DEV,
#define	_SC_2_FORT_DEV			_SC_2_FORT_DEV
    _SC_2_FORT_RUN,
#define	_SC_2_FORT_RUN			_SC_2_FORT_RUN
    _SC_2_SW_DEV,
#define	_SC_2_SW_DEV			_SC_2_SW_DEV
    _SC_2_LOCALEDEF,
#define	_SC_2_LOCALEDEF			_SC_2_LOCALEDEF

    _SC_PII,
#define	_SC_PII				_SC_PII
    _SC_PII_XTI,
#define	_SC_PII_XTI			_SC_PII_XTI
    _SC_PII_SOCKET,
#define	_SC_PII_SOCKET			_SC_PII_SOCKET
    _SC_PII_INTERNET,
#define	_SC_PII_INTERNET		_SC_PII_INTERNET
    _SC_PII_OSI,
#define	_SC_PII_OSI			_SC_PII_OSI
    _SC_POLL,
#define	_SC_POLL			_SC_POLL
    _SC_SELECT,
#define	_SC_SELECT			_SC_SELECT
    _SC_UIO_MAXIOV,
#define	_SC_UIO_MAXIOV			_SC_UIO_MAXIOV
    _SC_IOV_MAX = _SC_UIO_MAXIOV,
#define _SC_IOV_MAX			_SC_IOV_MAX
    _SC_PII_INTERNET_STREAM,
#define	_SC_PII_INTERNET_STREAM		_SC_PII_INTERNET_STREAM
    _SC_PII_INTERNET_DGRAM,
#define	_SC_PII_INTERNET_DGRAM		_SC_PII_INTERNET_DGRAM
    _SC_PII_OSI_COTS,
#define	_SC_PII_OSI_COTS		_SC_PII_OSI_COTS
    _SC_PII_OSI_CLTS,
#define	_SC_PII_OSI_CLTS		_SC_PII_OSI_CLTS
    _SC_PII_OSI_M,
#define	_SC_PII_OSI_M			_SC_PII_OSI_M
    _SC_T_IOV_MAX,
#define	_SC_T_IOV_MAX			_SC_T_IOV_MAX

    /* Values according to POSIX 1003.1c (POSIX threads).  */
    _SC_THREADS,
#define	_SC_THREADS			_SC_THREADS
    _SC_THREAD_SAFE_FUNCTIONS,
#define _SC_THREAD_SAFE_FUNCTIONS	_SC_THREAD_SAFE_FUNCTIONS
    _SC_GETGR_R_SIZE_MAX,
#define	_SC_GETGR_R_SIZE_MAX		_SC_GETGR_R_SIZE_MAX
    _SC_GETPW_R_SIZE_MAX,
#define	_SC_GETPW_R_SIZE_MAX		_SC_GETPW_R_SIZE_MAX
    _SC_LOGIN_NAME_MAX,
#define	_SC_LOGIN_NAME_MAX		_SC_LOGIN_NAME_MAX
    _SC_TTY_NAME_MAX,
#define	_SC_TTY_NAME_MAX		_SC_TTY_NAME_MAX
    _SC_THREAD_DESTRUCTOR_ITERATIONS,
#define	_SC_THREAD_DESTRUCTOR_ITERATIONS _SC_THREAD_DESTRUCTOR_ITERATIONS
    _SC_THREAD_KEYS_MAX,
#define	_SC_THREAD_KEYS_MAX		_SC_THREAD_KEYS_MAX
    _SC_THREAD_STACK_MIN,
#define	_SC_THREAD_STACK_MIN		_SC_THREAD_STACK_MIN
    _SC_THREAD_THREADS_MAX,
#define	_SC_THREAD_THREADS_MAX		_SC_THREAD_THREADS_MAX
    _SC_THREAD_ATTR_STACKADDR,
#define	_SC_THREAD_ATTR_STACKADDR	_SC_THREAD_ATTR_STACKADDR
    _SC_THREAD_ATTR_STACKSIZE,
#define	_SC_THREAD_ATTR_STACKSIZE	_SC_THREAD_ATTR_STACKSIZE
    _SC_THREAD_PRIORITY_SCHEDULING,
#define	_SC_THREAD_PRIORITY_SCHEDULING	_SC_THREAD_PRIORITY_SCHEDULING
    _SC_THREAD_PRIO_INHERIT,
#define	_SC_THREAD_PRIO_INHERIT		_SC_THREAD_PRIO_INHERIT
    _SC_THREAD_PRIO_PROTECT,
#define	_SC_THREAD_PRIO_PROTECT		_SC_THREAD_PRIO_PROTECT
    _SC_THREAD_PROCESS_SHARED,
#define	_SC_THREAD_PROCESS_SHARED	_SC_THREAD_PROCESS_SHARED

    _SC_NPROCESSORS_CONF,
#define _SC_NPROCESSORS_CONF		_SC_NPROCESSORS_CONF
    _SC_NPROCESSORS_ONLN,
#define _SC_NPROCESSORS_ONLN		_SC_NPROCESSORS_ONLN
    _SC_PHYS_PAGES,
#define _SC_PHYS_PAGES			_SC_PHYS_PAGES
    _SC_AVPHYS_PAGES,
#define _SC_AVPHYS_PAGES		_SC_AVPHYS_PAGES
    _SC_ATEXIT_MAX,
#define _SC_ATEXIT_MAX			_SC_ATEXIT_MAX
    _SC_PASS_MAX,
#define _SC_PASS_MAX			_SC_PASS_MAX

    _SC_XOPEN_VERSION,
#define _SC_XOPEN_VERSION		_SC_XOPEN_VERSION
    _SC_XOPEN_XCU_VERSION,
#define _SC_XOPEN_XCU_VERSION		_SC_XOPEN_XCU_VERSION
    _SC_XOPEN_UNIX,
#define _SC_XOPEN_UNIX			_SC_XOPEN_UNIX
    _SC_XOPEN_CRYPT,
#define _SC_XOPEN_CRYPT			_SC_XOPEN_CRYPT
    _SC_XOPEN_ENH_I18N,
#define _SC_XOPEN_ENH_I18N		_SC_XOPEN_ENH_I18N
    _SC_XOPEN_SHM,
#define _SC_XOPEN_SHM			_SC_XOPEN_SHM

    _SC_2_CHAR_TERM,
#define _SC_2_CHAR_TERM			_SC_2_CHAR_TERM
    _SC_2_C_VERSION,
#define _SC_2_C_VERSION			_SC_2_C_VERSION
    _SC_2_UPE,
#define _SC_2_UPE			_SC_2_UPE

    _SC_XOPEN_XPG2,
#define _SC_XOPEN_XPG2			_SC_XOPEN_XPG2
    _SC_XOPEN_XPG3,
#define _SC_XOPEN_XPG3			_SC_XOPEN_XPG3
    _SC_XOPEN_XPG4,
#define _SC_XOPEN_XPG4			_SC_XOPEN_XPG4

    _SC_CHAR_BIT,
#define	_SC_CHAR_BIT			_SC_CHAR_BIT
    _SC_CHAR_MAX,
#define	_SC_CHAR_MAX			_SC_CHAR_MAX
    _SC_CHAR_MIN,
#define	_SC_CHAR_MIN			_SC_CHAR_MIN
    _SC_INT_MAX,
#define	_SC_INT_MAX			_SC_INT_MAX
    _SC_INT_MIN,
#define	_SC_INT_MIN			_SC_INT_MIN
    _SC_LONG_BIT,
#define	_SC_LONG_BIT			_SC_LONG_BIT
    _SC_WORD_BIT,
#define	_SC_WORD_BIT			_SC_WORD_BIT
    _SC_MB_LEN_MAX,
#define	_SC_MB_LEN_MAX			_SC_MB_LEN_MAX
    _SC_NZERO,
#define	_SC_NZERO			_SC_NZERO
    _SC_SSIZE_MAX,
#define	_SC_SSIZE_MAX			_SC_SSIZE_MAX
    _SC_SCHAR_MAX,
#define	_SC_SCHAR_MAX			_SC_SCHAR_MAX
    _SC_SCHAR_MIN,
#define	_SC_SCHAR_MIN			_SC_SCHAR_MIN
    _SC_SHRT_MAX,
#define	_SC_SHRT_MAX			_SC_SHRT_MAX
    _SC_SHRT_MIN,
#define	_SC_SHRT_MIN			_SC_SHRT_MIN
    _SC_UCHAR_MAX,
#define	_SC_UCHAR_MAX			_SC_UCHAR_MAX
    _SC_UINT_MAX,
#define	_SC_UINT_MAX			_SC_UINT_MAX
    _SC_ULONG_MAX,
#define	_SC_ULONG_MAX			_SC_ULONG_MAX
    _SC_USHRT_MAX,
#define	_SC_USHRT_MAX			_SC_USHRT_MAX

    _SC_NL_ARGMAX,
#define	_SC_NL_ARGMAX			_SC_NL_ARGMAX
    _SC_NL_LANGMAX,
#define	_SC_NL_LANGMAX			_SC_NL_LANGMAX
    _SC_NL_MSGMAX,
#define	_SC_NL_MSGMAX			_SC_NL_MSGMAX
    _SC_NL_NMAX,
#define	_SC_NL_NMAX			_SC_NL_NMAX
    _SC_NL_SETMAX,
#define	_SC_NL_SETMAX			_SC_NL_SETMAX
    _SC_NL_TEXTMAX,
#define	_SC_NL_TEXTMAX			_SC_NL_TEXTMAX

    _SC_XBS5_ILP32_OFF32,
#define _SC_XBS5_ILP32_OFF32		_SC_XBS5_ILP32_OFF32
    _SC_XBS5_ILP32_OFFBIG,
#define _SC_XBS5_ILP32_OFFBIG		_SC_XBS5_ILP32_OFFBIG
    _SC_XBS5_LP64_OFF64,
#define _SC_XBS5_LP64_OFF64		_SC_XBS5_LP64_OFF64
    _SC_XBS5_LPBIG_OFFBIG,
#define _SC_XBS5_LPBIG_OFFBIG		_SC_XBS5_LPBIG_OFFBIG

    _SC_XOPEN_LEGACY,
#define _SC_XOPEN_LEGACY		_SC_XOPEN_LEGACY
    _SC_XOPEN_REALTIME,
#define _SC_XOPEN_REALTIME		_SC_XOPEN_REALTIME
    _SC_XOPEN_REALTIME_THREADS,
#define _SC_XOPEN_REALTIME_THREADS	_SC_XOPEN_REALTIME_THREADS

    _SC_ADVISORY_INFO,
#define _SC_ADVISORY_INFO		_SC_ADVISORY_INFO
    _SC_BARRIERS,
#define _SC_BARRIERS			_SC_BARRIERS
    _SC_BASE,
#define _SC_BASE			_SC_BASE
    _SC_C_LANG_SUPPORT,
#define _SC_C_LANG_SUPPORT		_SC_C_LANG_SUPPORT
    _SC_C_LANG_SUPPORT_R,
#define _SC_C_LANG_SUPPORT_R		_SC_C_LANG_SUPPORT_R
    _SC_CLOCK_SELECTION,
#define _SC_CLOCK_SELECTION		_SC_CLOCK_SELECTION
    _SC_CPUTIME,
#define _SC_CPUTIME			_SC_CPUTIME
    _SC_THREAD_CPUTIME,
#define _SC_THREAD_CPUTIME		_SC_THREAD_CPUTIME
    _SC_DEVICE_IO,
#define _SC_DEVICE_IO			_SC_DEVICE_IO
    _SC_DEVICE_SPECIFIC,
#define _SC_DEVICE_SPECIFIC		_SC_DEVICE_SPECIFIC
    _SC_DEVICE_SPECIFIC_R,
#define _SC_DEVICE_SPECIFIC_R		_SC_DEVICE_SPECIFIC_R
    _SC_FD_MGMT,
#define _SC_FD_MGMT			_SC_FD_MGMT
    _SC_FIFO,
#define _SC_FIFO			_SC_FIFO
    _SC_PIPE,
#define _SC_PIPE			_SC_PIPE
    _SC_FILE_ATTRIBUTES,
#define _SC_FILE_ATTRIBUTES		_SC_FILE_ATTRIBUTES
    _SC_FILE_LOCKING,
#define _SC_FILE_LOCKING		_SC_FILE_LOCKING
    _SC_FILE_SYSTEM,
#define _SC_FILE_SYSTEM			_SC_FILE_SYSTEM
    _SC_MONOTONIC_CLOCK,
#define _SC_MONOTONIC_CLOCK		_SC_MONOTONIC_CLOCK
    _SC_MULTI_PROCESS,
#define _SC_MULTI_PROCESS		_SC_MULTI_PROCESS
    _SC_SINGLE_PROCESS,
#define _SC_SINGLE_PROCESS		_SC_SINGLE_PROCESS
    _SC_NETWORKING,
#define _SC_NETWORKING			_SC_NETWORKING
    _SC_READER_WRITER_LOCKS,
#define _SC_READER_WRITER_LOCKS		_SC_READER_WRITER_LOCKS
    _SC_SPIN_LOCKS,
#define _SC_SPIN_LOCKS			_SC_SPIN_LOCKS
    _SC_REGEXP,
#define _SC_REGEXP			_SC_REGEXP
    _SC_REGEX_VERSION,
#define _SC_REGEX_VERSION		_SC_REGEX_VERSION
    _SC_SHELL,
#define _SC_SHELL			_SC_SHELL
    _SC_SIGNALS,
#define _SC_SIGNALS			_SC_SIGNALS
    _SC_SPAWN,
#define _SC_SPAWN			_SC_SPAWN
    _SC_SPORADIC_SERVER,
#define _SC_SPORADIC_SERVER		_SC_SPORADIC_SERVER
    _SC_THREAD_SPORADIC_SERVER,
#define _SC_THREAD_SPORADIC_SERVER	_SC_THREAD_SPORADIC_SERVER
    _SC_SYSTEM_DATABASE,
#define _SC_SYSTEM_DATABASE		_SC_SYSTEM_DATABASE
    _SC_SYSTEM_DATABASE_R,
#define _SC_SYSTEM_DATABASE_R		_SC_SYSTEM_DATABASE_R
    _SC_TIMEOUTS,
#define _SC_TIMEOUTS			_SC_TIMEOUTS
    _SC_TYPED_MEMORY_OBJECTS,
#define _SC_TYPED_MEMORY_OBJECTS	_SC_TYPED_MEMORY_OBJECTS
    _SC_USER_GROUPS,
#define _SC_USER_GROUPS			_SC_USER_GROUPS
    _SC_USER_GROUPS_R,
#define _SC_USER_GROUPS_R		_SC_USER_GROUPS_R
    _SC_2_PBS,
#define _SC_2_PBS			_SC_2_PBS
    _SC_2_PBS_ACCOUNTING,
#define _SC_2_PBS_ACCOUNTING		_SC_2_PBS_ACCOUNTING
    _SC_2_PBS_LOCATE,
#define _SC_2_PBS_LOCATE		_SC_2_PBS_LOCATE
    _SC_2_PBS_MESSAGE,
#define _SC_2_PBS_MESSAGE		_SC_2_PBS_MESSAGE
    _SC_2_PBS_TRACK,
#define _SC_2_PBS_TRACK			_SC_2_PBS_TRACK
    _SC_SYMLOOP_MAX,
#define _SC_SYMLOOP_MAX			_SC_SYMLOOP_MAX
    _SC_STREAMS,
#define _SC_STREAMS			_SC_STREAMS
    _SC_2_PBS_CHECKPOINT,
#define _SC_2_PBS_CHECKPOINT		_SC_2_PBS_CHECKPOINT

    _SC_V6_ILP32_OFF32,
#define _SC_V6_ILP32_OFF32		_SC_V6_ILP32_OFF32
    _SC_V6_ILP32_OFFBIG,
#define _SC_V6_ILP32_OFFBIG		_SC_V6_ILP32_OFFBIG
    _SC_V6_LP64_OFF64,
#define _SC_V6_LP64_OFF64		_SC_V6_LP64_OFF64
    _SC_V6_LPBIG_OFFBIG,
#define _SC_V6_LPBIG_OFFBIG		_SC_V6_LPBIG_OFFBIG

    _SC_HOST_NAME_MAX,
#define _SC_HOST_NAME_MAX		_SC_HOST_NAME_MAX
    _SC_TRACE,
#define _SC_TRACE			_SC_TRACE
    _SC_TRACE_EVENT_FILTER,
#define _SC_TRACE_EVENT_FILTER		_SC_TRACE_EVENT_FILTER
    _SC_TRACE_INHERIT,
#define _SC_TRACE_INHERIT		_SC_TRACE_INHERIT
    _SC_TRACE_LOG,
#define _SC_TRACE_LOG			_SC_TRACE_LOG

    _SC_LEVEL1_ICACHE_SIZE,
#define _SC_LEVEL1_ICACHE_SIZE		_SC_LEVEL1_ICACHE_SIZE
    _SC_LEVEL1_ICACHE_ASSOC,
#define _SC_LEVEL1_ICACHE_ASSOC		_SC_LEVEL1_ICACHE_ASSOC
    _SC_LEVEL1_ICACHE_LINESIZE,
#define _SC_LEVEL1_ICACHE_LINESIZE	_SC_LEVEL1_ICACHE_LINESIZE
    _SC_LEVEL1_DCACHE_SIZE,
#define _SC_LEVEL1_DCACHE_SIZE		_SC_LEVEL1_DCACHE_SIZE
    _SC_LEVEL1_DCACHE_ASSOC,
#define _SC_LEVEL1_DCACHE_ASSOC		_SC_LEVEL1_DCACHE_ASSOC
    _SC_LEVEL1_DCACHE_LINESIZE,
#define _SC_LEVEL1_DCACHE_LINESIZE	_SC_LEVEL1_DCACHE_LINESIZE
    _SC_LEVEL2_CACHE_SIZE,
#define _SC_LEVEL2_CACHE_SIZE		_SC_LEVEL2_CACHE_SIZE
    _SC_LEVEL2_CACHE_ASSOC,
#define _SC_LEVEL2_CACHE_ASSOC		_SC_LEVEL2_CACHE_ASSOC
    _SC_LEVEL2_CACHE_LINESIZE,
#define _SC_LEVEL2_CACHE_LINESIZE	_SC_LEVEL2_CACHE_LINESIZE
    _SC_LEVEL3_CACHE_SIZE,
#define _SC_LEVEL3_CACHE_SIZE		_SC_LEVEL3_CACHE_SIZE
    _SC_LEVEL3_CACHE_ASSOC,
#define _SC_LEVEL3_CACHE_ASSOC		_SC_LEVEL3_CACHE_ASSOC
    _SC_LEVEL3_CACHE_LINESIZE,
#define _SC_LEVEL3_CACHE_LINESIZE	_SC_LEVEL3_CACHE_LINESIZE
    _SC_LEVEL4_CACHE_SIZE,
#define _SC_LEVEL4_CACHE_SIZE		_SC_LEVEL4_CACHE_SIZE
    _SC_LEVEL4_CACHE_ASSOC,
#define _SC_LEVEL4_CACHE_ASSOC		_SC_LEVEL4_CACHE_ASSOC
    _SC_LEVEL4_CACHE_LINESIZE,
#define _SC_LEVEL4_CACHE_LINESIZE	_SC_LEVEL4_CACHE_LINESIZE
    /* Leave room here, maybe we need a few more cache levels some day.  */

    _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50,
#define _SC_IPV6			_SC_IPV6
    _SC_RAW_SOCKETS,
#define _SC_RAW_SOCKETS			_SC_RAW_SOCKETS

    _SC_V7_ILP32_OFF32,
#define _SC_V7_ILP32_OFF32		_SC_V7_ILP32_OFF32
    _SC_V7_ILP32_OFFBIG,
#define _SC_V7_ILP32_OFFBIG		_SC_V7_ILP32_OFFBIG
    _SC_V7_LP64_OFF64,
#define _SC_V7_LP64_OFF64		_SC_V7_LP64_OFF64
    _SC_V7_LPBIG_OFFBIG,
#define _SC_V7_LPBIG_OFFBIG		_SC_V7_LPBIG_OFFBIG

    _SC_SS_REPL_MAX,
#define _SC_SS_REPL_MAX			_SC_SS_REPL_MAX

    _SC_TRACE_EVENT_NAME_MAX,
#define _SC_TRACE_EVENT_NAME_MAX	_SC_TRACE_EVENT_NAME_MAX
    _SC_TRACE_NAME_MAX,
#define _SC_TRACE_NAME_MAX		_SC_TRACE_NAME_MAX
    _SC_TRACE_SYS_MAX,
#define _SC_TRACE_SYS_MAX		_SC_TRACE_SYS_MAX
    _SC_TRACE_USER_EVENT_MAX,
#define _SC_TRACE_USER_EVENT_MAX	_SC_TRACE_USER_EVENT_MAX

    _SC_XOPEN_STREAMS,
#define _SC_XOPEN_STREAMS		_SC_XOPEN_STREAMS

    _SC_THREAD_ROBUST_PRIO_INHERIT,
#define _SC_THREAD_ROBUST_PRIO_INHERIT	_SC_THREAD_ROBUST_PRIO_INHERIT
    _SC_THREAD_ROBUST_PRIO_PROTECT
#define _SC_THREAD_ROBUST_PRIO_PROTECT	_SC_THREAD_ROBUST_PRIO_PROTECT
  };

/* Values for the NAME argument to `confstr'.  */
enum __fc_confstr_name
  {
    _CS_PATH,			/* The default search path.  */
#define _CS_PATH		_CS_PATH

    _CS_V6_WIDTH_RESTRICTED_ENVS,
#define _CS_V6_WIDTH_RESTRICTED_ENVS	_CS_V6_WIDTH_RESTRICTED_ENVS
#define _CS_POSIX_V6_WIDTH_RESTRICTED_ENVS	_CS_V6_WIDTH_RESTRICTED_ENVS

    _CS_GNU_LIBC_VERSION,
#define _CS_GNU_LIBC_VERSION	_CS_GNU_LIBC_VERSION
    _CS_GNU_LIBPTHREAD_VERSION,
#define _CS_GNU_LIBPTHREAD_VERSION	_CS_GNU_LIBPTHREAD_VERSION

    _CS_V5_WIDTH_RESTRICTED_ENVS,
#define _CS_V5_WIDTH_RESTRICTED_ENVS	_CS_V5_WIDTH_RESTRICTED_ENVS
#define _CS_POSIX_V5_WIDTH_RESTRICTED_ENVS	_CS_V5_WIDTH_RESTRICTED_ENVS

    _CS_V7_WIDTH_RESTRICTED_ENVS,
#define _CS_V7_WIDTH_RESTRICTED_ENVS	_CS_V7_WIDTH_RESTRICTED_ENVS
#define _CS_POSIX_V7_WIDTH_RESTRICTED_ENVS	_CS_V7_WIDTH_RESTRICTED_ENVS

    _CS_LFS_CFLAGS = 1000,
#define _CS_LFS_CFLAGS	_CS_LFS_CFLAGS
    _CS_LFS_LDFLAGS,
#define _CS_LFS_LDFLAGS	_CS_LFS_LDFLAGS
    _CS_LFS_LIBS,
#define _CS_LFS_LIBS		_CS_LFS_LIBS
    _CS_LFS_LINTFLAGS,
#define _CS_LFS_LINTFLAGS	_CS_LFS_LINTFLAGS
    _CS_LFS64_CFLAGS,
#define _CS_LFS64_CFLAGS	_CS_LFS64_CFLAGS
    _CS_LFS64_LDFLAGS,
#define _CS_LFS64_LDFLAGS	_CS_LFS64_LDFLAGS
    _CS_LFS64_LIBS,
#define _CS_LFS64_LIBS	_CS_LFS64_LIBS
    _CS_LFS64_LINTFLAGS,
#define _CS_LFS64_LINTFLAGS	_CS_LFS64_LINTFLAGS

    _CS_XBS5_ILP32_OFF32_CFLAGS = 1100,
#define _CS_XBS5_ILP32_OFF32_CFLAGS _CS_XBS5_ILP32_OFF32_CFLAGS
    _CS_XBS5_ILP32_OFF32_LDFLAGS,
#define _CS_XBS5_ILP32_OFF32_LDFLAGS _CS_XBS5_ILP32_OFF32_LDFLAGS
    _CS_XBS5_ILP32_OFF32_LIBS,
#define _CS_XBS5_ILP32_OFF32_LIBS _CS_XBS5_ILP32_OFF32_LIBS
    _CS_XBS5_ILP32_OFF32_LINTFLAGS,
#define _CS_XBS5_ILP32_OFF32_LINTFLAGS _CS_XBS5_ILP32_OFF32_LINTFLAGS
    _CS_XBS5_ILP32_OFFBIG_CFLAGS,
#define _CS_XBS5_ILP32_OFFBIG_CFLAGS _CS_XBS5_ILP32_OFFBIG_CFLAGS
    _CS_XBS5_ILP32_OFFBIG_LDFLAGS,
#define _CS_XBS5_ILP32_OFFBIG_LDFLAGS _CS_XBS5_ILP32_OFFBIG_LDFLAGS
    _CS_XBS5_ILP32_OFFBIG_LIBS,
#define _CS_XBS5_ILP32_OFFBIG_LIBS _CS_XBS5_ILP32_OFFBIG_LIBS
    _CS_XBS5_ILP32_OFFBIG_LINTFLAGS,
#define _CS_XBS5_ILP32_OFFBIG_LINTFLAGS _CS_XBS5_ILP32_OFFBIG_LINTFLAGS
    _CS_XBS5_LP64_OFF64_CFLAGS,
#define _CS_XBS5_LP64_OFF64_CFLAGS _CS_XBS5_LP64_OFF64_CFLAGS
    _CS_XBS5_LP64_OFF64_LDFLAGS,
#define _CS_XBS5_LP64_OFF64_LDFLAGS _CS_XBS5_LP64_OFF64_LDFLAGS
    _CS_XBS5_LP64_OFF64_LIBS,
#define _CS_XBS5_LP64_OFF64_LIBS _CS_XBS5_LP64_OFF64_LIBS
    _CS_XBS5_LP64_OFF64_LINTFLAGS,
#define _CS_XBS5_LP64_OFF64_LINTFLAGS _CS_XBS5_LP64_OFF64_LINTFLAGS
    _CS_XBS5_LPBIG_OFFBIG_CFLAGS,
#define _CS_XBS5_LPBIG_OFFBIG_CFLAGS _CS_XBS5_LPBIG_OFFBIG_CFLAGS
    _CS_XBS5_LPBIG_OFFBIG_LDFLAGS,
#define _CS_XBS5_LPBIG_OFFBIG_LDFLAGS _CS_XBS5_LPBIG_OFFBIG_LDFLAGS
    _CS_XBS5_LPBIG_OFFBIG_LIBS,
#define _CS_XBS5_LPBIG_OFFBIG_LIBS _CS_XBS5_LPBIG_OFFBIG_LIBS
    _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS,
#define _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS

    _CS_POSIX_V6_ILP32_OFF32_CFLAGS,
#define _CS_POSIX_V6_ILP32_OFF32_CFLAGS _CS_POSIX_V6_ILP32_OFF32_CFLAGS
    _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,
#define _CS_POSIX_V6_ILP32_OFF32_LDFLAGS _CS_POSIX_V6_ILP32_OFF32_LDFLAGS
    _CS_POSIX_V6_ILP32_OFF32_LIBS,
#define _CS_POSIX_V6_ILP32_OFF32_LIBS _CS_POSIX_V6_ILP32_OFF32_LIBS
    _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS,
#define _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS
    _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,
#define _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS
    _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,
#define _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS
    _CS_POSIX_V6_ILP32_OFFBIG_LIBS,
#define _CS_POSIX_V6_ILP32_OFFBIG_LIBS _CS_POSIX_V6_ILP32_OFFBIG_LIBS
    _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS,
#define _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS
    _CS_POSIX_V6_LP64_OFF64_CFLAGS,
#define _CS_POSIX_V6_LP64_OFF64_CFLAGS _CS_POSIX_V6_LP64_OFF64_CFLAGS
    _CS_POSIX_V6_LP64_OFF64_LDFLAGS,
#define _CS_POSIX_V6_LP64_OFF64_LDFLAGS _CS_POSIX_V6_LP64_OFF64_LDFLAGS
    _CS_POSIX_V6_LP64_OFF64_LIBS,
#define _CS_POSIX_V6_LP64_OFF64_LIBS _CS_POSIX_V6_LP64_OFF64_LIBS
    _CS_POSIX_V6_LP64_OFF64_LINTFLAGS,
#define _CS_POSIX_V6_LP64_OFF64_LINTFLAGS _CS_POSIX_V6_LP64_OFF64_LINTFLAGS
    _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,
#define _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS
    _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,
#define _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS
    _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,
#define _CS_POSIX_V6_LPBIG_OFFBIG_LIBS _CS_POSIX_V6_LPBIG_OFFBIG_LIBS
    _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS,
#define _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS

    _CS_POSIX_V7_ILP32_OFF32_CFLAGS,
#define _CS_POSIX_V7_ILP32_OFF32_CFLAGS _CS_POSIX_V7_ILP32_OFF32_CFLAGS
    _CS_POSIX_V7_ILP32_OFF32_LDFLAGS,
#define _CS_POSIX_V7_ILP32_OFF32_LDFLAGS _CS_POSIX_V7_ILP32_OFF32_LDFLAGS
    _CS_POSIX_V7_ILP32_OFF32_LIBS,
#define _CS_POSIX_V7_ILP32_OFF32_LIBS _CS_POSIX_V7_ILP32_OFF32_LIBS
    _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS,
#define _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS
    _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS,
#define _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS
    _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS,
#define _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS
    _CS_POSIX_V7_ILP32_OFFBIG_LIBS,
#define _CS_POSIX_V7_ILP32_OFFBIG_LIBS _CS_POSIX_V7_ILP32_OFFBIG_LIBS
    _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS,
#define _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS
    _CS_POSIX_V7_LP64_OFF64_CFLAGS,
#define _CS_POSIX_V7_LP64_OFF64_CFLAGS _CS_POSIX_V7_LP64_OFF64_CFLAGS
    _CS_POSIX_V7_LP64_OFF64_LDFLAGS,
#define _CS_POSIX_V7_LP64_OFF64_LDFLAGS _CS_POSIX_V7_LP64_OFF64_LDFLAGS
    _CS_POSIX_V7_LP64_OFF64_LIBS,
#define _CS_POSIX_V7_LP64_OFF64_LIBS _CS_POSIX_V7_LP64_OFF64_LIBS
    _CS_POSIX_V7_LP64_OFF64_LINTFLAGS,
#define _CS_POSIX_V7_LP64_OFF64_LINTFLAGS _CS_POSIX_V7_LP64_OFF64_LINTFLAGS
    _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS,
#define _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS
    _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS,
#define _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS
    _CS_POSIX_V7_LPBIG_OFFBIG_LIBS,
#define _CS_POSIX_V7_LPBIG_OFFBIG_LIBS _CS_POSIX_V7_LPBIG_OFFBIG_LIBS
    _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS,
#define _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS

    _CS_V6_ENV,
#define _CS_V6_ENV			_CS_V6_ENV
    _CS_V7_ENV
#define _CS_V7_ENV			_CS_V7_ENV
  };


// arbitrary number
#define __FC_MAX_OPEN_FILES 1024

// __fc_fds represents the state of open file descriptors.
//@ ghost int __fc_fds[__FC_MAX_OPEN_FILES];
// TODO: Model the state of some functions more precisely.
// TODO: define __fc_fds as volatile.


/*@ // missing: may assign to errno: EACCES, ELOOP, ENAMETOOLONG, ENOENT,
    //                               ENOTDIR, EROFS, ETXTBSY
    //                               (EINVAL prevented by precondition)
    // missing: assigns \result \from 'filesystem, permissions'
  requires valid_string_path: valid_read_string(path);
  requires valid_amode: (amode & ~(R_OK | W_OK | X_OK)) == 0 ||
                        amode == F_OK;
  assigns \result \from indirect:path, indirect:path[0..], indirect:amode;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          access(const char *path, int amode);

extern unsigned int alarm(unsigned int);
extern int          brk(void *);

/*@ // missing: may assign to errno: EACCES, ELOOP, ENAMETOOLONG, ENOENT,
    //                               ENOTDIR
    // missing: assigns \result \from 'filesystem'
  requires valid_string_path: valid_read_string(path);
  assigns \result \from indirect:path, indirect:path[0..];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          chdir(const char *path);


/*@ // missing: may assign to errno: EACCES, ELOOP, ENAMETOOLONG, ENOENT,
    //                               ENOTDIR, EPERM
    // missing: assigns \result \from 'filesystem, permissions'
    // missing: assigns 'filesystem view' \from path[0..];
  requires valid_string_path: valid_read_string(path);
  assigns \result \from indirect:path, indirect:path[0..];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          chroot(const char *path);


/*@ // missing: may assign to errno: EACCES, ELOOP, ENAMETOOLONG, ENOENT,
    //                               ENOTDIR, EROFS, EIO, EINTR, EINVAL
    // missing: assigns \result \from 'filesystem, permissions'
    // missing: assigns 'file permissions' \from owner, group;
  requires valid_string_path: valid_read_string(path);
  assigns \result \from indirect:path, indirect:path[0..], indirect:owner,
                        indirect:group;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          chown(const char *path, uid_t owner, gid_t group);

/*@
  requires valid_fd: 0 <= fd < __FC_MAX_OPEN_FILES;
  assigns __fc_fds[fd] \from fd, __fc_fds[fd];
  assigns \result \from indirect:fd, indirect:__fc_fds[fd];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          close(int fd);
extern size_t       confstr(int, char *, size_t);
extern char        *crypt(const char *, const char *);
extern char        *ctermid(char *);
extern char        *cuserid(char *s);

/*@ // missing: may assign errno EBADF, EMFILE
  requires valid_fildes: 0 <= fildes < __FC_MAX_OPEN_FILES;
  assigns __fc_fds[fildes..] \from fildes; // missing: \from 'filesystem'
  assigns \result \from fildes;
  ensures result_valid_fildes_or_error: \result == -1 ||
    fildes <= \result < __FC_MAX_OPEN_FILES;
*/
extern int          dup(int fildes);

/*@ // missing: may assign errno EBADF, EINTR, EIO
  requires valid_fildes: 0 <= fildes < __FC_MAX_OPEN_FILES;
  requires valid_fildes2: 0 <= fildes2 < __FC_MAX_OPEN_FILES;
  assigns __fc_fds[fildes2] \from fildes, fildes2, __fc_fds[fildes2];
  assigns \result \from fildes, fildes2, __fc_fds[fildes], __fc_fds[fildes2];
  ensures result_fildes2_or_error: \result == fildes2 || \result == -1;
*/
extern int          dup2(int fildes, int fildes2);

extern void         encrypt(char[64], int);

/*@ requires valid_string_path: valid_read_string(path);
    requires valid_string_arg: valid_read_string(arg);
    assigns \result \from path[0..], arg[0..];
*/
extern int          execl(const char *path, const char *arg, ...);
/*@ requires valid_string_path: valid_read_string(path);
    requires valid_string_arg: valid_read_string(arg);
    assigns \result \from path[0..], arg[0..];
*/
extern int          execle(const char *path, const char *arg, ...);
/*@ requires valid_string_path: valid_read_string(path);
    requires valid_string_arg: valid_read_string(arg);
    assigns \result \from path[0..], arg[0..];
*/
extern int          execlp(const char *path, const char *arg, ...);
/*@ requires valid_string_path: valid_read_string(path);
    requires valid_string_argv0: valid_read_string(argv[0]);
    assigns \result \from path[0..], argv[0..];
*/
extern int          execv(const char *path, char *const argv[]);
/*@ requires valid_path: valid_read_string(path);
    requires valid_argv0: valid_read_string(argv[0]);
    assigns \result \from path[0..], argv[0..];
*/
extern int          execve(const char *path, char *const argv[], char *const env[]);
/*@ requires valid_string_path: valid_read_string(path);
    requires valid_string_argv0: valid_read_string(argv[0]);
    assigns \result \from path[0..], argv[0..];
*/
extern int          execvp(const char *path, char *const argv[]);

/*@
  assigns \nothing;
  ensures never_terminates: \false;
*/
extern void         _exit(int) __attribute__ ((__noreturn__));

extern int          fchown(int, uid_t, gid_t);
extern int          fchdir(int);
extern int          fdatasync(int);

/*@ // missing: assigns \result \from 'other processes, internal state'
    // missing: may assign errno EAGAIN, ENOMEM
  assigns \result \from \nothing;
  ensures result_ok_child_or_error: \result == 0 || \result > 0 ||
    \result == -1;
*/
extern pid_t        fork(void);

extern long int     fpathconf(int, int);
extern int          fsync(int);
extern int          ftruncate(int, off_t);

/*@ // missing: assigns buf[0..size-1] \from 'cwd'
    // missing: may assign to errno: EACCES, EINVAL, ENAMETOOLONG, ENOENT,
    //                               ENOMEM, ERANGE
  requires valid_buf: \valid(buf + (0 .. size-1));
  assigns buf[0 .. size-1], \result;
  assigns buf[0 .. size-1] \from indirect:buf, indirect:size;
  assigns \result \from buf, indirect: size;
  ensures result_ok_or_error: \result == \null || \result == buf;
*/
extern char        *getcwd(char *buf, size_t size);

extern int          getdtablesize(void);

/*@ //missing: assigns \result \from 'process effective gid'
  assigns \result \from \nothing;
*/
extern gid_t        getegid(void);

/*@ //missing: assigns \result \from 'process effective uid'
  assigns \result \from \nothing;
*/
extern uid_t        geteuid(void);

/*@ //missing: assigns \result \from 'process gid'
  assigns \result \from \nothing;
*/
extern gid_t        getgid(void);

extern int          getgroups(int, gid_t []);
extern long         gethostid(void);

extern volatile char __fc_hostname[HOST_NAME_MAX];

/*@
  requires name_has_room: \valid(name + (0 .. len-1));
  assigns \result, name[0 .. len-1]
      \from indirect:__fc_hostname[0 .. len], indirect:len;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int gethostname(char *name, size_t len);

// Non-POSIX
/*@
  requires name_valid_string: valid_read_nstring(name, len);
  requires bounded_len: len <= HOST_NAME_MAX;
  assigns __fc_hostname[0 .. len] \from name[0 .. len-1], indirect:len;
  assigns \result \from indirect:__fc_hostname[0 .. len];
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int sethostname(const char *name, size_t len);

extern char        *getlogin(void);
extern int          getlogin_r(char *, size_t);
extern int          getpagesize(void);
extern char        *getpass(const char *);

/*@ //missing: assigns \result \from 'process PGID'
  assigns \result \from indirect:pid;
*/
extern pid_t        getpgid(pid_t pid);

/*@ //missing: assigns \result \from 'calling process PGID'
  assigns \result \from \nothing;
*/
extern pid_t        getpgrp(void);

/*@ //missing: assigns \result \from 'process id'
  assigns \result \from \nothing;
*/
extern pid_t        getpid(void);

/*@ //missing: assigns \result \from 'parent process id'
  assigns \result \from \nothing;
*/
extern pid_t        getppid(void);

/*@ //missing: assigns \result \from 'process sid'
  assigns \result \from \nothing;
*/
extern pid_t        getsid(pid_t);

/*@ //missing: assigns \result \from 'process uid'
  assigns \result \from \nothing;
*/
extern uid_t        getuid(void);

extern char        *getwd(char *);

/*@ //missing: may assign to errno: EBADF, ENOTTY (POSIX) / EINVAL (Linux)
  assigns \result \from indirect:fd, indirect:__fc_fds[fd];
  ensures result_true_or_false: \result == 0 || \result == 1;
 */
extern int          isatty(int fd);

extern int          lchown(const char *, uid_t, gid_t);
extern int          link(const char *, const char *);
extern int          lockf(int, int, off_t);

/*@ //missing: may assign to errno: EBADF, EINVAL, EOVERFLOW, ESPIPE, ENXIO (Linux);
  requires valid_fd: 0 <= fd < __FC_MAX_OPEN_FILES;
  requires valid_whence: whence == SEEK_SET || whence == SEEK_CUR ||
                         whence == SEEK_END;
  assigns \result \from indirect:fd, indirect:__fc_fds[fd], indirect:offset,
                        indirect:whence;
  assigns __fc_fds[fd] \from indirect:fd, __fc_fds[fd], indirect:offset,
                             indirect:whence;
  ensures result_error_or_offset: \result == -1 || 0 <= \result;
 */
extern off_t        lseek(int fd, off_t offset, int whence);

extern int          nice(int);

/*@ // missing: may assign to errno: EACCES, EINVAL, ELOOP, ENOENT, ENOTDIR
    // missing: assigns \result \from 'file path in filesystem'
  requires valid_path: valid_read_string(path);
  assigns \result \from indirect:path[0 ..], indirect:name;
*/
extern long pathconf(char const *path, int name);

extern int          pause(void);

/*@
  requires valid_pipefd: \valid(pipefd+(0..1));
  assigns pipefd[0..1] \from indirect:__fc_fds[0..];
  assigns \result \from indirect:__fc_fds[0..];
  ensures initialization:pipefd: \initialized(&pipefd[0..1]);
  ensures valid_fd0: 0 <= pipefd[0] < __FC_MAX_OPEN_FILES;
  ensures valid_fd1: 0 <= pipefd[1] < __FC_MAX_OPEN_FILES;
  ensures result_ok_or_error: \result == 0 || \result == -1;
 */
extern int          pipe(int pipefd[2]);

extern ssize_t      pread(int, void *, size_t, off_t);
extern int          pthread_atfork(void (*)(void), void (*)(void),
                 void(*)(void));
extern ssize_t      pwrite(int, const void *, size_t, off_t);

/*@
  requires valid_fd: 0 <= fd < __FC_MAX_OPEN_FILES;
  requires buf_has_room: \valid((char *)buf+(0..count-1));
  assigns __fc_fds[fd] \from __fc_fds[fd];
  assigns \result, *((char *)buf+(0..count-1))
          \from indirect:__fc_fds[fd], indirect:count;
  ensures  result_error_or_read_length: 0 <= \result <= count || \result == -1;
  ensures  initialization:buf: \initialized(((char*)buf)+(0..\result-1));
*/
extern ssize_t      read(int fd, void *buf, size_t count);

extern int          readlink(const char *, char *, size_t);
extern int          rmdir(const char *);
extern void        *sbrk(intptr_t);

/*@ // missing: may assign errno to EINVAL or EPERM
    // missing: assigns 'process egid' \from gid
  assigns \result \from indirect:gid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int setegid(gid_t gid);

/*@ // missing: may assign errno to EINVAL or EPERM
    // missing: assigns 'process euid' \from uid
  assigns \result \from indirect:uid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int seteuid(uid_t uid);

/*@ // missing: may assign errno to EINVAL or EPERM
    // missing: assigns 'process gid' \from gid, 'process permissions'
    // missing: assigns \result \from 'process permissions'
  assigns \result \from indirect:gid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          setgid(gid_t gid);

/*@ // missing: may assign to errno
  // missing: assigns \result \from 'processes'
  assigns \result \from indirect:pid, indirect:pgid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          setpgid(pid_t pid, pid_t pgid);

extern pid_t        setpgrp(void);

/*@ // missing: may assign errno to EINVAL, EPERM or EAGAIN
    // missing: assigns 'process real/effective gid' \from gid
    // missing: assigns \result \from 'process gid and permissions'
  assigns \result \from indirect:rgid, indirect:egid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          setregid(gid_t rgid, gid_t egid);

/*@ // missing: may assign errno to EINVAL, EPERM or EAGAIN
    // missing: assigns 'process real/effective uid' \from uid
    // missing: assigns \result \from 'process uid and permissions'
  assigns \result \from indirect:ruid, indirect:euid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          setreuid(uid_t ruid, uid_t euid);

/*@ // missing: may assign errno to EPERM
    // missing: assigns \result, 'session, process, gid' \from 'process';
  assigns \result \from \nothing;
  ensures result_pgid_or_error: \result == -1 || \result >= 0;
 */
extern pid_t        setsid(void);

/*@ // missing: may assign errno to EINVAL, EPERM or EAGAIN
    // missing: assigns 'process uid' \from uid, 'process permissions'
    // missing: assigns \result \from 'process permissions'
  assigns \result \from indirect:uid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
*/
extern int          setuid(uid_t uid);

extern unsigned int sleep(unsigned int);
extern void         swab(const void *, void *, ssize_t);
extern int          symlink(const char *, const char *);

/*@ //missing: assigns 'filesystem' \from 'filesystem'
  assigns \nothing;
*/
extern void         sync(void);

/*@ //missing: assigns 'filesystem', \result \from 'filesystem'
    //missing: may set errno to EINVAL
  assigns \result \from indirect:name;
*/
extern long int     sysconf(int name);

extern pid_t        tcgetpgrp(int);
extern int          tcsetpgrp(int, pid_t);
extern int          truncate(const char *, off_t);

extern volatile char __fc_ttyname[TTY_NAME_MAX];
volatile char *__fc_p_ttyname = __fc_ttyname;

/*@
  // missing: may assign to errno: EBADF, ENOTTY
  requires valid_fildes: 0 <= fildes < __FC_MAX_OPEN_FILES;
  assigns \result \from __fc_p_ttyname, indirect:fildes;
  ensures result_name_or_null: \result == __fc_p_ttyname || \result == \null;
 */
extern char        *ttyname(int fildes);

extern int          ttyname_r(int, char *, size_t);
extern useconds_t   ualarm(useconds_t, useconds_t);

/*@ // missing: may assign errno
  // missing: assigns 'filesystem' \from path[0..];
  // missing: assigns \result \from 'filesystem';
  requires valid_string_path: valid_read_string(path);
  assigns \result \from path[0..];
  ensures result_ok_or_error: \result == 0 || \result == -1;
 */
extern int          unlink(const char *path);

/*@
  assigns \result \from indirect:usec, indirect:Frama_C_entropy_source;
  assigns Frama_C_entropy_source \from Frama_C_entropy_source;
  ensures result_ok_or_error: \result == 0 || \result == -1;
 */
extern int          usleep(useconds_t usec);

extern pid_t        vfork(void);

/*@
  requires valid_fd: 0 <= fd < __FC_MAX_OPEN_FILES;
  requires buf_has_room: \valid_read(((char *)buf)+(0..count-1));
  assigns __fc_fds[fd] \from indirect:fd, indirect:count, __fc_fds[fd];
  assigns \result \from indirect:fd, indirect:count, indirect:__fc_fds[fd];
  ensures result_error_or_written_bytes: \result == -1 || 0 <= \result <= count;
*/
extern ssize_t      write(int fd, const void *buf, size_t count);

// setgroups() is not POSIX
extern int setgroups(size_t size, const gid_t *list);

// The following functions are GNU extensions

/*@
  // missing: assigns \result, *ruid, *euid, *suid \from 'process'
  // missing: may assign to errno: EFAULT
  requires valid_ruid: \valid(ruid);
  requires valid_euid: \valid(suid);
  requires valid_suid: \valid(euid);
  assigns *ruid, *euid, *suid \from \nothing;
  assigns \result \from indirect:ruid, indirect:euid, indirect:suid;
  ensures initialization:result_ok_or_error:
    (\result == 0 &&
     \initialized(ruid) && \initialized(euid) && \initialized(suid))
    || \result == -1;
 */
int getresuid(uid_t *ruid, uid_t *euid, uid_t *suid);

/*@
  // missing: assigns 'process uid' \from ruid, euid, suid
  // missing: assigns \result \from 'process permissions'
  // missing: may assign to errno: EAGAIN, EINVAL, EPERM
  assigns \result \from indirect:ruid, indirect:euid, indirect:suid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
 */
int setresuid(uid_t ruid, uid_t euid, uid_t suid);

/*@
  // missing: assigns \result, *ruid, *euid, *suid \from 'process'
  // missing: may assign to errno: EFAULT
  requires valid_rgid: \valid(rgid);
  requires valid_egid: \valid(sgid);
  requires valid_sgid: \valid(egid);
  assigns *rgid, *egid, *sgid \from \nothing;
  assigns \result \from indirect:rgid, indirect:egid, indirect:sgid;
  ensures initialization:result_ok_or_error:
    (\result == 0 &&
     \initialized(rgid) && \initialized(egid) && \initialized(sgid))
    || \result == -1;
 */
int getresgid(gid_t *rgid, gid_t *egid, gid_t *sgid);

/*@
  // missing: assigns 'process gid' \from rgid, egid, sgid
  // missing: assigns \result \from 'process permissions'
  // missing: may assign to errno: EAGAIN, EINVAL, EPERM
  assigns \result \from indirect:rgid, indirect:egid, indirect:sgid;
  ensures result_ok_or_error: \result == 0 || \result == -1;
 */
int setresgid(gid_t rgid, gid_t egid, gid_t sgid);

extern char *optarg;
extern int optind, opterr, optopt;

/*@
  assigns \result, *optarg, optind, opterr, optopt
             \from argc, argv[0..argc-1], optstring[0..];
 */
extern int getopt(int argc, char * const argv[], const char *optstring);


__END_DECLS

__POP_FC_STDLIB
#endif
