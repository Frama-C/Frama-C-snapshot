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

/* Default size of a program's heap tracked via shadow memory */
#ifndef E_ACSL_HEAP_SIZE
#define E_ACSL_HEAP_SIZE 512
#endif

/* Default size of a program's stack tracked via shadow memory */
#ifndef E_ACSL_STACK_SIZE
#define E_ACSL_STACK_SIZE 64
#endif

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
#define PGM_HEAP_SIZE (E_ACSL_HEAP_SIZE * MB)

/*! \brief Size of a program's Thread-local storage (TLS) */
#define PGM_TLS_SIZE (16 * MB)

/*! \brief Mspace padding used by shadow segments. This is to make sure that
 * some allocation which exceeds the size of an initial memspace does not
 * move the mspace somewhere else. 512KB is a bit of an overkill, but should
 * not hurt too much in general unless memory space is really a constraint */
#define SHADOW_SEGMENT_PADDING (512*KB)
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

/*! \brief Set a new soft stack limit
 * \param size - new stack size in bytes */
static size_t increase_stack_limit(const size_t size) {
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
  } else {
    vabort("getrlimit: %s \n", strerror(errno));
  }
  return size;
}

/*! \brief Return byte-size of a program's stack. The return value is the soft
 * stack limit, i.e., it can be programmatically increased at runtime. */
static size_t get_default_stack_size() {
  struct rlimit rlim;
  vassert(!getrlimit(RLIMIT_STACK, &rlim),
    "Cannot detect program's stack size", NULL);
  return rlim.rlim_cur;
}

static size_t get_stack_size() {
#ifndef E_ACSL_STACK_SIZE
  return get_default_stack_size();
#else
  return increase_stack_limit(E_ACSL_STACK_SIZE*MB);
#endif
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
/* }}} */

/** Program heap information {{{ */
/*! \brief Return the start address of a program's heap. */
static uintptr_t get_heap_start() {
  return mem_spaces.heap_start;
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

/** Shadow Layout {{{ */
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
  In reality layouts may vary. Also, with mmap allocations heap does not
  necessarily grows from program break upwards. Typically mmap will allocate
  memory somewhere closer to stack. */

/* Struct representing a contigous memory region. Effectively this describes
 * a memory segment, such as heap, stack or segments in the shadow memory
 * used to track them. */
struct memory_segment {
  const char *name; //!< Symbolic name
  size_t    size; //!< Byte-size
  uintptr_t start; //!< Least address
  uintptr_t end; //!< Greatest address
  mspace    mspace; // !< Mspace used for the partition
  /* The following are only set if the segment is a shadow segment */
  struct memory_segment *parent; //!< Pointer to the tracked segment
  size_t shadow_ratio; //!< Ratio of shadow to application memory
  /*!< Offset between the start of the tracked segment and the start of this
     segment */
  intptr_t shadow_offset;
};

typedef struct memory_segment memory_segment;

/* Struct representing a memory segment along with information about its
 * shadow spaces. */
struct memory_partition {
  memory_segment application; /* Application memory segment */
  memory_segment primary; /* Primary shadow segment */
  memory_segment secondary; /* Secondary shadow segment */
#ifdef E_ACSL_TEMPORAL
  memory_segment temporal_primary; /* Primary temporal shadow segment */
  memory_segment temporal_secondary; /* Secondary temporal shadow segment */
#endif
};

typedef struct memory_partition memory_partition;

/* Struct representing memory layout of a program consisting of heap, stack,
   global and tls segments */
struct memory_layout {
  memory_partition heap;
  memory_partition stack;
  memory_partition global;
  memory_partition tls;
  int is_initialized;
};

/*! \brief Full program memory layout. */
static struct memory_layout mem_layout;

/*! \brief Array of used partitions */
static memory_partition *mem_partitions [] = {
  &mem_layout.heap,
  &mem_layout.stack,
  &mem_layout.global,
  &mem_layout.tls
};

/*! \brief Initialize an application memory segment.
 *
 * \param seg - pointer to a segment to initialize
 * \param start - least address in an application's segment
 * \param size - size in bytes
 * \param name - segment name
 * \param msp - mspace used for this segment (defined only for heap) */
static void set_application_segment(memory_segment *seg, uintptr_t start,
    size_t size, const char *name, mspace msp) {
  seg->name = name;
  seg->start = start;
  seg->size = size;
  seg->end = seg->start + seg->size;
  seg->mspace = msp;
  seg->parent = NULL;
  seg->shadow_ratio = 0;
  seg->shadow_offset = 0;
}

/*! \brief Set a shadow memory segment
 *
 * \param seg - pointer to a segment to initialize
 * \param parent - pointer to the segment ::seg tracks. Should be initialized
 * \param ratio - ratio of shadow to application memory
 * \param name - symbolic name of the segment
 */
static void set_shadow_segment(memory_segment *seg, memory_segment *parent,
    size_t ratio, const char *name) {
  seg->parent = parent;
  seg->name = name;
  seg->shadow_ratio = ratio;
  seg->size = parent->size/seg->shadow_ratio;
  seg->mspace = create_mspace(seg->size + SHADOW_SEGMENT_PADDING, 0);
  seg->start = (uintptr_t)mspace_malloc(seg->mspace,1);
  seg->end = seg->start + seg->size;
  seg->shadow_offset = parent->start - seg->start;
}

/*! \brief Initialize memory layout, i.e., determine bounds of program segments,
 * allocate shadow memory spaces and compute offsets. This function populates
 * global struct ::memory_layout holding that information with data. */
static void init_shadow_layout(int *argc_ref, char ***argv_ref) {
  memory_partition *pheap = &mem_layout.heap;
  set_application_segment(&pheap->application, get_heap_start(),
    get_heap_size(), "heap", mem_spaces.heap_mspace);
  set_shadow_segment(&pheap->primary, &pheap->application, 1, "heap_primary");
  set_shadow_segment(&pheap->secondary, &pheap->application, 8, "heap_secondary");
#ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pheap->temporal_primary, &pheap->application, 1, "temporal_heap_primary");
  set_shadow_segment(&pheap->temporal_secondary, &pheap->application, 1, "temporal_heap_secondary");
#endif

  memory_partition *pstack = &mem_layout.stack;
  set_application_segment(&pstack->application, get_stack_start(argc_ref, argv_ref),
    get_stack_size(), "stack", NULL);
  set_shadow_segment(&pstack->primary, &pstack->application, 1, "stack_primary");
  set_shadow_segment(&pstack->secondary, &pstack->application, 1, "stack_secondary");
#ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pstack->temporal_primary, &pstack->application, 1, "temporal_stack_primary");
  set_shadow_segment(&pstack->temporal_secondary, &pstack->application, 1, "temporal_stack_secondary");
#endif

  memory_partition *pglobal = &mem_layout.global;
  set_application_segment(&pglobal->application, get_global_start(),
    get_global_size(), "global", NULL);
  set_shadow_segment(&pglobal->primary, &pglobal->application, 1, "global_primary");
  set_shadow_segment(&pglobal->secondary, &pglobal->application, 1, "global_secondary");
#ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&pglobal->temporal_primary, &pglobal->application, 1, "temporal_global_primary");
  set_shadow_segment(&pglobal->temporal_secondary, &pglobal->application, 1, "temporal_global_secondary");
#endif

  memory_partition *ptls = &mem_layout.tls;
  set_application_segment(&ptls->application, get_tls_start(),
    get_tls_size(), "tls", NULL);
  set_shadow_segment(&ptls->primary, &ptls->application, 1, "tls_primary");
  set_shadow_segment(&ptls->secondary, &ptls->application, 1, "tls_secondary");
#ifdef E_ACSL_TEMPORAL
  set_shadow_segment(&ptls->temporal_primary, &ptls->application, 1, "temporal_tls_primary");
  set_shadow_segment(&ptls->temporal_secondary, &ptls->application, 1, "temporal_tls_secondary");
#endif

  mem_layout.is_initialized = 1;
}

/*! \brief Deallocate shadow regions used by runtime analysis */
static void clean_shadow_layout() {
  if (mem_layout.is_initialized) {
    int i;
    for (i = 0; i < sizeof(mem_partitions)/sizeof(memory_partition*); i++) {
      if (mem_partitions[i]->primary.mspace)
        destroy_mspace(mem_partitions[i]->primary.mspace);
      if (mem_partitions[i]->secondary.mspace)
        destroy_mspace(mem_partitions[i]->secondary.mspace);
    }
  }
}
/* }}} */

/** Shadow access {{{
 *
 * Shadow displacement offsets are stored using signed integers.
 * Displacement offset between an application memory space Ma and a shadow
 * memory space Ms is computed by [min(Ma) - min(Ms)], where min(Ma) and min(Ms)
 * denote least addresses in application and shadow spaces Ma and Ms respectively.
 *
 * Correspondense between a shadow address S and an application address A
 * using a displacement offset OFF is therefore as follows:
 *    OFF = A - S
 *    S = A - OFF
 *    A = S + OFF
 *
 * Conversions between application-space and shadow memory addresses
 * are given by following macros.
*/

#define heap_primary_offset     mem_layout.heap.primary.shadow_offset
#define heap_secondary_offset   mem_layout.heap.secondary.shadow_offset
#define stack_primary_offset    mem_layout.stack.primary.shadow_offset
#define stack_secondary_offset  mem_layout.stack.secondary.shadow_offset
#define global_primary_offset   mem_layout.global.primary.shadow_offset
#define global_secondary_offset mem_layout.global.secondary.shadow_offset
#define tls_primary_offset      mem_layout.tls.primary.shadow_offset
#define tls_secondary_offset    mem_layout.tls.secondary.shadow_offset

/*! \brief Compute a shadow address using displacement offset
 * @param _addr - an application space address
 * @param _offset - a shadow displacement offset */
#define SHADOW_ACCESS(_addr,_offset)  \
  ((intptr_t)((intptr_t)_addr - (intptr_t)_offset))

/*! \brief Same as SHADOW_ACCESS but with an additional scale factor given via
 * _scale argument. Scale factor describes ratio of application to shadow bytes,
 * for instance if one bit shadow memory is used to track one byte of
 * application memory then the scale factor is 8.
 * Here, scale factor is the ration of application to shadow memory. */
#define SCALED_SHADOW_ACCESS(_addr, _start, _offset, _scale)  \
  (((uintptr_t)_start - _offset) + \
   ((uintptr_t)_addr - (uintptr_t)_start)/_scale)

/*! \brief Convert a heap address into its shadow counterpart */
#define HEAP_SHADOW(_addr) \
  SHADOW_ACCESS(_addr, heap_primary_offset)

/*! \brief Convert a heap address into its init shadow counterpart */
#define HEAP_INIT_SHADOW(_addr) \
  SCALED_SHADOW_ACCESS(_addr, \
      mem_layout.heap.application.start, \
      mem_layout.heap.secondary.shadow_offset, \
      mem_layout.heap.secondary.shadow_ratio)

#define HEAP_START mem_layout.heap.application.start

/*! \brief Convert a stack address into its primary shadow counterpart */
#define PRIMARY_STACK_SHADOW(_addr) \
  SHADOW_ACCESS(_addr, stack_primary_offset)

/*! \brief Convert a stack address into its secondary shadow counterpart */
#define SECONDARY_STACK_SHADOW(_addr) \
  SHADOW_ACCESS(_addr, stack_secondary_offset)

/*! \brief Convert a global address into its primary shadow counterpart */
#define PRIMARY_GLOBAL_SHADOW(_addr)  \
  SHADOW_ACCESS(_addr, global_primary_offset)

/*! \brief Convert a global address into its secondary shadow counterpart */
#define SECONDARY_GLOBAL_SHADOW(_addr) \
  SHADOW_ACCESS(_addr, global_secondary_offset)

/*! \brief Convert a TLS address into its primary shadow counterpart */
#define PRIMARY_TLS_SHADOW(_addr)  \
  SHADOW_ACCESS(_addr, tls_primary_offset)

/*! \brief Convert a TLS address into its secondary shadow counterpart */
#define SECONDARY_TLS_SHADOW(_addr) \
  SHADOW_ACCESS(_addr, tls_secondary_offset)

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
#define IS_ON_HEAP(_addr) IS_ON(_addr, mem_layout.heap.application)

/*! \brief Evaluate to true if `_addr` is a stack address */
#define IS_ON_STACK(_addr) IS_ON(_addr, mem_layout.stack.application)

/*! \brief Evaluate to true if `_addr` is a global address */
#define IS_ON_GLOBAL(_addr) IS_ON(_addr, mem_layout.global.application)

/*! \brief Evaluate to true if _addr is a TLS address */
#define IS_ON_TLS(_addr) IS_ON(_addr, mem_layout.tls.application)

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

#ifdef E_ACSL_TEMPORAL /* {{{ */
/*! \brief Convert a heap address into its shadow counterpart */
#define TEMPORAL_HEAP_SHADOW(_addr) \
  SHADOW_ACCESS(_addr, mem_layout.heap.temporal_primary.shadow_offset)

/*! \brief Convert a stack address into its primary temporal shadow counterpart */
#define TEMPORAL_PRIMARY_STACK_SHADOW(_addr) \
  SHADOW_ACCESS(_addr, mem_layout.stack.temporal_primary.shadow_offset)

/*! \brief Convert a stack address into its secondary temporal shadow counterpart */
#define TEMPORAL_SECONDARY_STACK_SHADOW(_addr) \
  SHADOW_ACCESS(_addr, mem_layout.stack.temporal_secondary.shadow_offset)

/*! \brief Convert a global address into its primary temporal shadow counterpart */
#define TEMPORAL_PRIMARY_GLOBAL_SHADOW(_addr)  \
  SHADOW_ACCESS(_addr, mem_layout.global.temporal_primary.shadow_offset)

/*! \brief Convert a global address into its primary temporal shadow counterpart */
#define TEMPORAL_SECONDARY_GLOBAL_SHADOW(_addr)  \
  SHADOW_ACCESS(_addr, mem_layout.global.temporal_secondary.shadow_offset)

/*! \brief Convert a TLS address into its primary temporal shadow counterpart */
#define TEMPORAL_PRIMARY_TLS_SHADOW(_addr)  \
  SHADOW_ACCESS(_addr, mem_layout.tls.temporal_primary.shadow_offset)

/*! \brief Convert a TLS address into its secondary temporal shadow counterpart */
#define TEMPORAL_SECONDARY_TLS_SHADOW(_addr)  \
  SHADOW_ACCESS(_addr, mem_layout.tls.temporal_secondary.shadow_offset)

/*! \brief Temporal primary shadow address of a non-dynamic region */
#define TEMPORAL_PRIMARY_STATIC_SHADOW(_addr) \
  SHADOW_REGION_ADDRESS(_addr, TEMPORAL_PRIMARY)

/*! \brief Temporal secondary shadow address of a non-dynamic region */
#define TEMPORAL_SECONDARY_STATIC_SHADOW(_addr) \
  SHADOW_REGION_ADDRESS(_addr, TEMPORAL_SECONDARY)
#endif /* }}} */
