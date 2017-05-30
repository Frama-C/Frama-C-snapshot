/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2017                                               */
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

/*! ***********************************************************************
 * \file  e_acsl_shadow_layout.h
 * \brief Setup for memory tracking using shadowing
***************************************************************************/

/* Symbols exported by the linker script */

/*!\brief The first address past the end of the text segment. */
extern char etext;
/*!\brief The first address past the end of the initialized data segment. */
extern char edata;
/*!\brief The first address past the end of the uninitialized data segment. */
extern char end;
/*!\brief The first address of a program. */
extern char __executable_start;

/* \cond */
void *sbrk(intptr_t increment);
char *strerror(int errnum);

/* MAP_ANONYMOUS is a mmap flag indicating that the contents of allocated blocks
 * should be nullified. Set value from <bits/mman-linux.h>, if MAP_ANONYMOUS is
 * undefined */
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS 0x20
#endif
/* \endcond */

/* Block size units in bytes */
#define KB (1024) //!< Bytes in a kilobyte
#define MB (1024*KB) //!< Bytes in a megabyte
#define GB (1024*MB) //!< Bytes in a gigabyte
#define KB_SZ(_s) (_s/KB) //!< Convert bytes to kilobytes
#define MB_SZ(_s) (_s/MB) //!< Convert bytes to megabytes
#define GB_SZ(_s) (_s/GB) //!< Convert bytes to gigabytes

/*! \brief Byte-width of a pointer */
#define PTR_SZ sizeof(uintptr_t)

/*! \brief Byte-width of the largest integer type usable with bitwise
 * operators */
#define ULONG_BYTES 8
/*! \brief Bit-width of the largest integer type usable with bitwise
 * operators */
#define ULONG_BITS 64

/** Hardcoded sizes of tracked program segments {{{ */
/*! \brief Size of a program's heap */
#define PGM_HEAP_SIZE (512 * MB)

/*! \brief Size of a program's Thread-local storage (TLS) */
#define PGM_TLS_SIZE (16 * MB)
/* }}} */

/** Thread-local storage information {{{ */

/*! Thread-local storage (TLS) keeps track of copies of per-thread variables.
 * Even though at the present stage RTL of E-ACSL is not thread-safe, some
 * of the variables (for instance ::errno) are allocated there. In X86 TLS
 * is typically located somewhere below the program's stack but above mmap
 * areas. TLS is typically separated into two sections: .tdata and .tbss.
 * Similar to globals using .data and .bss, .tdata keeps track of initialized
 * thread-local variables, while .tbss holds uninitialized ones.
 *
 * Start and end addresses of TLS are obtained by taking addresses of
 * initialized and initialized variables in TLS (::id_tdata and ::id_tss)
 * and adding fixed amount of shadow space around them. Visually it looks
 * as follows:
 *
 *   end TLS address (&id_tdata + TLS_SHADOW_SIZE/2)
 *   id_tdata address
 *   ...
 *   id_tbss address
 *   start TLS address (&id_bss - TLS_SHADOW_SIZE/2)
 */

/*! \brief Return byte-size of the TLS segment */
static size_t get_tls_size() {
  return PGM_TLS_SIZE;
}

static __thread int id_tdata = 1;
static __thread int id_tbss;

/*! \brief Return start address of a program's TLS */
static uintptr_t get_tls_start() {
  size_t tls_size = get_tls_size();
  uintptr_t data = (uintptr_t)&id_tdata,
            bss = (uintptr_t)&id_tbss;
  return ((data > bss ? bss  : data) - tls_size/2);
}

/* }}} */

/** Program stack information {{{ */
extern char ** environ;

/*! \brief Return byte-size of a program's stack. The return value is the soft
 * stack limit, i.e., it can be programmatically increased at runtime. */
static size_t get_stack_size() {
  struct rlimit rlim;
  assert(!getrlimit(RLIMIT_STACK, &rlim));
  return rlim.rlim_cur;
}

/*! \brief Return greatest (known) address on a program's stack.
 * This function presently determines the address using the address of the
 * last string in `environ`. That is, it assumes that argc and argv are
 * stored below environ, which holds for GCC/Glibc but is not necessarily
 * true for some other compilers/libraries. */
static uintptr_t get_stack_start(int *argc_ref,  char *** argv_ref) {
  char **env = environ;
  while (env[1])
    env++;
  uintptr_t addr = (uintptr_t)*env + strlen(*env);

  /* When returning the end stack address we need to make sure that
   * ::ULONG_BITS past that address are actually writeable. This is
   * to be able to set initialization and read-only bits ::ULONG_BITS
   * at a time. If not respected, this may cause a segfault in
   * ::argv_alloca. */
  uintptr_t stack_end = addr + ULONG_BITS;
  uintptr_t stack_start = stack_end - get_stack_size();
  return stack_start;
}

/*! \brief Set a new soft stack limit
 * \param size - new stack size in bytes */
static void increase_stack_limit(const size_t size) {
  const rlim_t stacksz = (rlim_t)size;
  struct rlimit rl;
  int result = getrlimit(RLIMIT_STACK, &rl);
  if (result == 0) {
    if (rl.rlim_cur < stacksz) {
      rl.rlim_cur = stacksz;
      result = setrlimit(RLIMIT_STACK, &rl);
      if (result != 0) {
        vabort("setrlimit: %s \n", strerror(errno));
      }
    }
  }
}
/* }}} */

/** Program heap information {{{ */
/*! \brief Return the start address of a program's heap. In this implementation
 * the start address on a program's heap is its initial breakpoint. */
static uintptr_t get_heap_start() {
  return (uintptr_t)sbrk(0);
}

/*! \brief Return the tracked size of a program's heap. */
static size_t get_heap_size() {
  return PGM_HEAP_SIZE;
}

/*! \brief Return the size of a secondary shadow region tracking
 * initialization (i.e., init shadow). */
static size_t get_heap_init_size() {
  return get_heap_size()/8;
}

/** }}} */

/** Program global information {{{ */
/*! \brief Return the start address of a segment holding globals (generally
 * BSS and Data segments). */
static uintptr_t get_global_start() {
  return (uintptr_t)&__executable_start;
}

/*! \brief Return byte-size of global segment */
static size_t get_global_size() {
  return ((uintptr_t)&end - get_global_start());
}
/** }}} */

/** MMAP allocation {{{ */
/*! \brief Allocate a memory block of `size` bytes with `mmap` and return a
 * pointer to its base address. Since this function is used to set-up shadowing
 * the program is aborted if `mmap` fails to allocate a new memory block. */
static void *do_mmap(size_t size) {
  void *res = mmap(0, size, PROT_READ|PROT_WRITE,
    MAP_ANONYMOUS|MAP_PRIVATE, -1, (size_t)0);
  if (res == MAP_FAILED) {
    DLOG("<<< Request to allocate %lu MB with mmap failed >>>\n", MB_SZ(size));
    vabort("mmap error: %s\n", strerror(errno));
  }
  /* Make sure that mmap returned a fully nullified mapping */
  DVASSERT(zeroed_out(res, size),
    "Memory mapping of size %lu at address %a not fully nullified", size, res);
  return res;
}
/* }}} */

/** Shadow Offset {{{ */
/*! \brief Compute shadow offset between the start address of a shadow area
 * and a start address of a segment */
static uintptr_t shadow_offset(void *shadow, uintptr_t start_addr) {
  uintptr_t start_shadow = (uintptr_t)shadow;
  return (start_shadow > start_addr) ?
    start_shadow - start_addr : start_addr - start_shadow;
}
/* }}} */

/** Program Layout {{{ */
/*****************************************************************************
 * Memory Layout *************************************************************
 *****************************************************************************
  ----------------------------------------> Max address
  Kernel Space
  ---------------------------------------->
  Non-canonical address space (only in 64-bit)
  ---------------------------------------->
  Environment variables [ GLIBC extension ]
 ----------------------------------------->
  Program arguments [ argc, argv ]
 -----------------------------------------> Stack End
  Stack [ Grows downwards ]
 ----------------------------------------->
  Thread-local storage (TLS) [ TDATA and TBSS ]
 ----------------------------------------->
  Shadow memory [ Heap, Stack, Global, TLS ]
 ----------------------------------------->
  Object mappings
 ----------------------------------------->
 ----------------------------------------->
  Heap [ Grows upwards^ ]
 -----------------------------------------> Heap Start [Initial Brk]
  BSS Segment  [ Uninitialised Globals ]
 ----------------------------------------->
  Data Segment [ Initialised Globals   ]
 ----------------------------------------->
  ROData [ Potentially ]
 ----------------------------------------->
  Text Segment [ Constants ]
 -----------------------------------------> NULL (0)
 *****************************************************************************
NOTE: Above memory layout scheme generally applies to Linux Kernel/gcc/glibc.
  It is also an approximation slanted towards 64-bit virtual process layout.
  In reality layouts may vary.

NOTE: With mmap allocations heap does not necessarily grows from program break
  upwards. Typically mmap will allocate memory somewhere closer to stack. This
  implementation, however, uses brk allocations, thus forcing stack to grow
  upwards from program break.
*/

/* Struct representing a memory segment along with information about its
 * shadow spaces. */
struct memory_segment {
  const char *name;
  uintptr_t start; //!< Least address in the application segment
  uintptr_t end; //!< Greatest address in the application segment
  uintptr_t size; //!< Size of the tracked segment in application memory
  /* Primary shadow space */
  size_t    prim_size; //!< Byte-size of the primary shadow
  size_t    prim_ratio; //! Ratio of shadow to application memory
  uintptr_t prim_start; //!< Least address in the primary shadow
  uintptr_t prim_end; //!< Greatest address in the primary shadow
  uintptr_t prim_offset; //!< Primary shadow offset
  /* Secondary shadow space */
  size_t    sec_size; //!< Byte-size of shadow area
  size_t    sec_ratio; //! Ratio of shadow to application memory
  uintptr_t sec_start; //!< Least address in the secondary shadow
  uintptr_t sec_end; //!< Greatest address in the secondary shadow
  uintptr_t sec_offset; //!< Secondary shadow offset

  int initialized; //! Notion on whether the layout is initialized
};

/*! \brief Full program memory layout. */
static struct memory_layout mem_layout;

struct memory_layout {
  struct memory_segment heap;
  struct memory_segment stack;
  struct memory_segment global;
  struct memory_segment tls;
  int initialized;
};

/*! \brief Set a given memory segment and its shadow spaces.
 *
 * \param start - least address in an application's segment
 * \param size - byte size of a tracked application's segment
 * \param prim_ratio - compression ratio of the primary shadow segment
 * \param sec_ratio - compression ratio of the secondary shadow segment
 * \param name - segment name
*/
static void set_shadow_segment(struct memory_segment *seg, uintptr_t start,
    size_t size, size_t prim_ratio, size_t sec_ratio, const char *name) {

  seg->name = name;
  seg->start = start;
  seg->size = size;
  seg->end = seg->start + seg->size - 1;

  if (prim_ratio) {
    seg->prim_ratio = prim_ratio;
    seg->prim_size = seg->size/seg->prim_ratio;
    void *prim_shadow = do_mmap(seg->prim_size);
    seg->prim_start = (uintptr_t)prim_shadow;
    seg->prim_end = seg->prim_start + seg->prim_size - 1;
    seg->prim_offset = shadow_offset(prim_shadow, start);
  } else {
    seg->prim_start = seg->prim_end = seg->prim_offset = 0;
  }

  if (sec_ratio) {
    seg->sec_ratio = sec_ratio;
    seg->sec_size = seg->size/seg->sec_ratio;
    void *sec_shadow = do_mmap(seg->sec_size);
    seg->sec_start = (uintptr_t)sec_shadow;
    seg->sec_end = seg->sec_start + seg->sec_size - 1;
    seg->sec_offset = shadow_offset(sec_shadow, seg->start);
  } else {
    seg->sec_start = seg->sec_end = seg->sec_offset = 0;
  }
}

/*! \brief Initialize memory layout, i.e., determine bounds of program segments,
 * allocate shadow memory spaces and compute offsets. This function populates
 * global struct ::mem_layout holding that information with data. */
static void init_memory_layout(int *argc_ref, char ***argv_ref) {
  /* Use DEBUG_PRINT_LAYOUT to output the details (if they are needed) */
  set_shadow_segment(&mem_layout.heap,
    get_heap_start(), get_heap_size(), 1, 8, "heap");
  set_shadow_segment(&mem_layout.stack,
    get_stack_start(argc_ref, argv_ref), get_stack_size(), 1, 1, "stack");
  set_shadow_segment(&mem_layout.global,
    get_global_start(), get_global_size(), 1, 1, "global");
  set_shadow_segment(&mem_layout.tls,
    get_tls_start(), get_tls_size(), 1, 1, "tls");
  mem_layout.initialized = 1;
}

/*! \brief Deallocate a shadow segment */
void clean_memory_segment(struct memory_segment *seg) {
  if (seg->prim_start)
    munmap((void*)seg->prim_start, seg->prim_size);
  if (seg->sec_start)
    munmap((void*)seg->sec_start, seg->prim_size);
}

/*! \brief Deallocate shadow regions used by runtime analysis */
static void clean_memory_layout() {
  DLOG("<<< Clean shadow layout >>>\n");
  if (mem_layout.initialized) {
    clean_memory_segment(&mem_layout.heap);
    clean_memory_segment(&mem_layout.stack);
    clean_memory_segment(&mem_layout.global);
    clean_memory_segment(&mem_layout.tls);
  }
}
/* }}} */

/** Shadow access {{{
 *
 * In a typical case shadow regions reside in the high memory but below
 * stack. Provided that shadow displacement offsets are stored using
 * unsigned, integers computing some shadow address `S` of an application-space
 * address `A` using a shadow displacement offset `OFF` is as follows:
 *
 *  Stack address:
 *    S = A - OFF
 *  Global, heap of RTL address:
 *    S = A + OFF
 *
 * Conversions between application-space and shadow memory addresses
 * are given using the following macros.
*/

/*! \brief Compute a shadow address using displacement offset
 * @param _addr - an application space address
 * @param _offset - a shadow displacement offset
 * @param _direction - while displacement offsets are stored as unsigned
 * integers, _direction (`+` or `-`) indicates the sign of the offset. */
#define SHADOW_ACCESS(_addr,_offset,_direction)  \
  ((uintptr_t)((uintptr_t)_addr _direction _offset))

/*! \brief Access to a shadow space below an application's segment */
#define LOWER_SHADOW_ACCESS(_addr,_offset) \
  SHADOW_ACCESS(_addr,_offset,-)

/*! \brief Access to a shadow space above an application's segment */
#define HIGHER_SHADOW_ACCESS(_addr,_offset) \
  SHADOW_ACCESS(_addr,_offset,+)

/*! \brief Same as SHADOW_ACCESS but with an additional scale factor given via
 * _scale argument. Scale factor describes ratio of application to shadow bytes,
 * for instance if one bit shadow memory is used to track one byte of
 * application memory then the scale factor is 8. */
#define SCALED_SHADOW_ACCESS(_addr,_start,_offset,_scale,_direction)  \
  (_addr _direction \
    (_offset - \
      ((uintptr_t)_addr - _start) + \
      ((uintptr_t)_addr - _start)/_scale))

/*! \brief Same as `LOWER_SHADOW_ACCESS` but with an additional scale factor */
#define LOWER_SCALED_SHADOW_ACCESS(_addr,_start,_offset,_scale)  \
  SCALED_SHADOW_ACCESS(_addr,_start,_offset,_scale, -)

/*! \brief Same as `HIGHER_SHADOW_ACCESS` but with an additional scale factor */
#define HIGHER_SCALED_SHADOW_ACCESS(_addr,_start,_offset,_scale)  \
  SCALED_SHADOW_ACCESS(_addr,_start,_offset,_scale, +)

/*! \brief Convert a stack address into its primary shadow counterpart */
#define PRIMARY_STACK_SHADOW(_addr) \
  LOWER_SHADOW_ACCESS(_addr, mem_layout.stack.prim_offset)

/*! \brief Convert a stack address into its secondary shadow counterpart */
#define SECONDARY_STACK_SHADOW(_addr) \
  LOWER_SHADOW_ACCESS(_addr, mem_layout.stack.sec_offset)

/*! \brief Convert a global address into its primary shadow counterpart */
#define PRIMARY_GLOBAL_SHADOW(_addr)  \
  HIGHER_SHADOW_ACCESS(_addr, mem_layout.global.prim_offset)

/*! \brief Convert a global address into its secondary shadow counterpart */
#define SECONDARY_GLOBAL_SHADOW(_addr) \
  HIGHER_SHADOW_ACCESS(_addr, mem_layout.global.sec_offset)

/*! \brief Convert a TLS address into its primary shadow counterpart */
#define PRIMARY_TLS_SHADOW(_addr)  \
  LOWER_SHADOW_ACCESS(_addr, mem_layout.tls.prim_offset)

/*! \brief Convert a TLS address into its secondary shadow counterpart */
#define SECONDARY_TLS_SHADOW(_addr) \
  LOWER_SHADOW_ACCESS(_addr, mem_layout.tls.sec_offset)

/* \brief Compute a primary or a secondary shadow address (based on the value of
 * parameter `_region`) of an address tracked via an offset-based encoding.
 * For an untracked address `0` is returned. */
#define SHADOW_REGION_ADDRESS(_addr, _region) \
  (IS_ON_STACK(_addr) ? _region##_STACK_SHADOW(_addr) : \
    IS_ON_GLOBAL(_addr) ? _region##_GLOBAL_SHADOW(_addr) : \
      IS_ON_TLS(_addr) ? _region##_TLS_SHADOW(_addr) : 0)

/*! \brief Primary shadow address of a non-dynamic region */
#define PRIMARY_SHADOW(_addr) SHADOW_REGION_ADDRESS(_addr, PRIMARY)
/*! \brief Secondary shadow address of a non-dynamic region */
#define SECONDARY_SHADOW(_addr) SHADOW_REGION_ADDRESS(_addr, SECONDARY)

/*! \brief Convert a heap address into its shadow counterpart */
#define HEAP_SHADOW(_addr) \
  HIGHER_SHADOW_ACCESS(_addr, mem_layout.heap.prim_offset)

#define HEAP_START mem_layout.heap.start

/*! \brief Convert a heap address into its init shadow counterpart */
#define HEAP_INIT_SHADOW(_addr) \
  HIGHER_SCALED_SHADOW_ACCESS(_addr, \
      mem_layout.heap.start, \
      mem_layout.heap.sec_offset, \
      mem_layout.heap.sec_ratio)

/* }}} */

/** Memory segment ranges {{{ */
/*! \brief Evaluate to a true value if address _addr resides within a given
 * memory segment.
 * \param _addr - a memory address
 * \param _seg - a memory segment (one of the structs within ::mem_layout)
*/
#define IS_ON(_addr,_seg) ( \
  ((uintptr_t)_addr) >= _seg.start && \
  ((uintptr_t)_addr) <= _seg.end \
)

/*! \brief Evaluate to true if `_addr` is a heap address */
#define IS_ON_HEAP(_addr) IS_ON(_addr, mem_layout.heap)

/*! \brief Evaluate to true if `_addr` is a stack address */
#define IS_ON_STACK(_addr) IS_ON(_addr, mem_layout.stack)

/*! \brief Evaluate to true if `_addr` is a global address */
#define IS_ON_GLOBAL(_addr) IS_ON(_addr, mem_layout.global)

/*! \brief Evaluate to true if _addr is a TLS address */
#define IS_ON_TLS(_addr) IS_ON(_addr, mem_layout.tls)

/*! \brief Shortcut for evaluating an address via ::IS_ON_STACK,
 * ::IS_ON_GLOBAL or ::IS_ON_TLS  */
#define IS_ON_STATIC(_addr) \
  (IS_ON_STACK(_addr) || IS_ON_GLOBAL(_addr) || IS_ON_TLS(_addr))

/*! \brief Evaluate to a true value if a given address belongs to tracked
 * allocation (i.e., found within tls, stack, heap or globally) */
#define IS_ON_VALID(_addr) \
  (IS_ON_STACK(_addr) || IS_ON_HEAP(_addr) || \
   IS_ON_GLOBAL(_addr) || IS_ON_TLS(_addr))
/* }}} */
