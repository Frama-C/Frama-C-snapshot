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
 * \file  e_acsl_segment_tracking.h
 * \brief Core functionality of the segment-based memory model
***************************************************************************/

/* Segment settings and shadow values interpretation {{{ */

/* This file implements segment-based and offset-based shadow memory models
 * (shadow encodings) (see draft of the PLDI'17 paper).
 *
 * IMPORTANT: While the implementation of the offset-based encoding mostly
 * follows the description given by the paper, there are differences in the
 * segment-based encoding for tracking heap memory. Some of these differences
 * are as follows:
 *  1) Size of a heap segment is increased to 32 bytes
 *  2) Heap meta-segments are no longer used, segment-based representation of
 *    a heap block considers only block segments, such that:
 *    - Lowest `intptr_t` bytes of each shadow segment tracking an application
 *      block store the base address of that block;
 *    - `intptr_t` bytes of the first segment following the initial `intptr_t`
 *      bytes store the length of the block. Note, the length is only stored
 *      by the first segment.
 *  3) Per-byte initialization of application bytes is tracked via a disjoint
 *    shadow region, which maps one bit of shadow memory to a byte of
 *    application memory. Comments within this file often refer to a shadow
 *    region tracking application blocks by segments as to `block shadow`,
 *    and to the region tracking initialization as to `init shadow`.
*/

/*! @brief Byte size of a heap segment.
 * This size is potentially used as an argument to `memalign`.
 * It SHOULD be a multiple of 2 and a multiple of a pointer size.
 *
 * \b FIXME: in the current implementation there might be issues with segment
 * size greater than 64 bytes. This is because presently some initialization
 * functionality relies on the fact that initialization per segment can be set
 * and/or evaluated using an 8-byte bitmask. */
#define HEAP_SEGMENT 32

/*! \brief Size (in bytes) of a long block on the stack. */
#define LONG_BLOCK 8

/*! \brief Bit offset in a primary shadow byte that represents initialization. */
#define INIT_BIT 0

/*! \brief Bit offset in a primary shadow byte that represents read-only or
 * read-write access.
 *
 * This is such that the value of 1 is read-only, and 0 is read/write */
#define READONLY_BIT 1

/*! \brief Evaluate to a non-zero value if the size of a memory
 * block indicates that it is a long one */
#define IS_LONG_BLOCK(_size) (_size > LONG_BLOCK)

/*! \brief Offset within a long block that identifies the portion of the block
 * that does not have a corresponding shadow and reuse the shadow of a previous
 * segment.
 * E.g., given a long block of 11 bytes the boundary is 8. Then, bytes [0,7] of
 * the block are shadowed (storing block offset and size) and bytes 8-10 are
 * not. This is because 3 bytes are not sufficient to store size and offset.
 * These remaining bytes reuse the shadow of [0,7]. */
#define LONG_BLOCK_BOUNDARY(_size) (_size - _size%LONG_BLOCK)

/*! \brief Primary shadow of a long block consists of a 8-byte segment + a
 * remainder. For instance, a 18-byte block is represented by two 8-byte
 * segments + 2 bytes.  Each byte of a segment stores an offset in the secondary
 * shadow. The offsets for each such segment can be expressed using the
 * following number obtained by compressing all eight bytes with offsets set
 * into a single block. */
#define LONG_BLOCK_MASK 15913703276567643328UL

/*! \brief 6 higher bytes of a memory cell on stack that belongs to a long
 * memory block store offsets relative to meta-data in the secondary shadow. The
 * offsets start with the below number. E.g., if the bits store 51, then the
 * offset at which to read meta-data is (51 - 48). */
#define LONG_BLOCK_INDEX_START 48

/*! \brief  Decrease _n to be a multiple of _m */
#define ALIGN_LEFT(_n, _m) (_n - _n%_m)

/*! \brief  Increase _n to be a multiple of _m */
#define ALIGN_RIGHT(_n, _m) (_n + ((_n%_m) ? (_m - _n%_m) : 0))

/*! \brief Heap shadow address aligned at a segment boundary */
#define ALIGNED_HEAP_SHADOW(_addr) \
  HEAP_SHADOW(ALIGN_LEFT(_addr,HEAP_SEGMENT))

/* \brief Maximal size_t value that does not cause overflow via addition
 * when segment size is added. */
static const size_t max_allocated = ALIGN_LEFT(SIZE_MAX,HEAP_SEGMENT);

/* \brief Return actual allocation size which takes into account aligned
 * allocation. In the present implementation it is the requested size of
 * a heap block aligned at a segment boundary */
#define ALLOC_SIZE(_s) \
  (_s < max_allocated ? ALIGN_RIGHT(_s,	HEAP_SEGMENT) : 0)

/** \brief Evaluate to `true` if address _addr belongs to a memory block
 * with base address _base and length _length */
#define BELONGS(_addr, _base, _length) \
  (_addr >= _base && _addr < _base + _length)

/*! \brief For short blocks numbers 1 to 36 represent lengths and offsets,
 * such that:
 * - 0 -> length 0, offset 0
 * - 1 -> length 1, offset 0,
 * - 2 -> length 2, offset 0,
 * - 3 -> length 2, offset 1 and so on.
 *
 * The below data is used to identify lengths and offsets:
 * Given x is a number from [1, 36] range:
 *   - short_lengths[x] -> length of a block
 *   - short_offsets[x] -> offset within a block */
static const char short_lengths[] = {
  0,
  1,
  2,2,
  3,3,3,
  4,4,4,4,
  5,5,5,5,5,
  6,6,6,6,6,6,
  7,7,7,7,7,7,7,
  8,8,8,8,8,8,8,8
};

static const char short_offsets[] = {
  0,
  0,
  0,1,
  0,1,2,
  0,1,2,3,
  0,1,2,3,4,
  0,1,2,3,4,5,
  0,1,2,3,4,5,6,
  0,1,2,3,4,5,6,7
};

/*! \brief Mask for marking a heap segment as initialized.
 * For instance, let `uintptr_t *p' point to the start of a heap segment
 * in the heap shadow, then 'p[1] | heap_init_mask` sets initialization bits.
 * NOTE: This approach cannot deal with segments larger than 64 bits. */
static const uint64_t heap_init_mask = ~(ONE << HEAP_SEGMENT);

/*! \brief Masks for checking of initialization of global/stack allocated blocks.
 * A byte allocated globally or on stack is deemed initialized if its
 * least significant bit is set to `1' and uninitialized otherwise.
 * The binary representation is then as follows (assuming the leftmost
 * bit is the least significant one):
 *
 *  00000000 00000000 00000000 00000000 ... (0)
 *  10000000 00000000 00000000 00000000 ... (1)
 *  10000000 10000000 00000000 00000000 ... (257)
 *  10000000 10000000 10000000 00000000 ... (65793)
 *  10000000 10000000 10000000 10000000 ... (16843009)
 *  ...
 *
 * For instance, mark first X bytes of a number N as initialised:
 *    N |= static_init_masks[X] */
static const uint64_t static_init_masks [] = {
  0, /* 0 bytes */
  1,  /* 1 byte */
  257,  /* 2 bytes */
  65793,  /* 3 bytes */
  16843009,  /* 4 bytes */
  4311810305,  /* 5 bytes */
  1103823438081,  /* 6 bytes */
  282578800148737,	/* 7 bytes */
  72340172838076673		/* 8 bytes */
};

/*! \brief Bit masks for setting read-only (second least significant) bits.
 * Binary representation (assuming the least significant bit is the
 * leftmost bit) is follows:
 *
 *  00000000 00000000 00000000 00000000 ... (0)
 *  01000000 00000000 00000000 00000000 ... (2)
 *  01000000 01000000 00000000 00000000 ... (514)
 *  01000000 01000000 01000000 00000000 ... (131586)
 *  01000000 01000000 01000000 01000000 ... (33686018)
 *  ...
 *
 *  For instance, mark first X bytes of a number N as read-only:
 *    N |= static_readonly_masks[X] */
static const uint64_t static_readonly_masks [] = {
  0, /* 0 bytes */
  2, /* 1 byte */
  514, /* 2 bytes */
  131586, /* 3 bytes */
  33686018, /* 4 bytes */
  8623620610, /* 5 bytes */
  2207646876162, /* 6 bytes */
  565157600297474, /* 7 bytes */
  144680345676153346 /* 8 bytes */
};
/* }}} */

/* Runtime assertions (debug mode) {{{ */
#ifdef E_ACSL_DEBUG
#define DVALIDATE_ALIGNMENT(_addr) \
  DVASSERT(((uintptr_t)_addr) % HEAP_SEGMENT == 0,  \
      "Heap base address %a is unaligned", _addr)

#define DVALIDATE_MEMORY_INIT \
  DVASSERT(mem_layout.is_initialized != 0, "Un-initialized shadow layout", NULL)

/* Debug function making sure that the order of program segments is as expected
 * and that the program and the shadow segments used do not overlap. */
static void validate_shadow_layout() {
  /* Check that the struct holding memory layout is marked as initialized. */
  DVALIDATE_MEMORY_INIT;

  /* Each segment has 3 partitions:
	 - application memory
     - primary/secondary shadows */
  int num_partitions = sizeof(mem_partitions)/sizeof(memory_partition*);
  int num_seg_in_part = 3;
#ifdef E_ACSL_TEMPORAL
  num_seg_in_part = 5;
#endif
  int num_segments = num_partitions*num_seg_in_part;
  uintptr_t segments[num_segments][2];

  size_t i;
  for (i = 0; i < num_partitions; i++) {
    memory_partition *p = mem_partitions[i];
    segments[num_seg_in_part*i][0] = p->application.start;
    segments[num_seg_in_part*i][1] = p->application.end;
    segments[num_seg_in_part*i+1][0] = p->primary.start;
    segments[num_seg_in_part*i+1][1] = p->primary.end;
    segments[num_seg_in_part*i+2][0] = p->secondary.start;
    segments[num_seg_in_part*i+2][1] = p->secondary.end;
#ifdef E_ACSL_TEMPORAL
    segments[num_seg_in_part*i+3][0] = p->temporal_primary.start;
    segments[num_seg_in_part*i+3][1] = p->temporal_primary.end;
    segments[num_seg_in_part*i+4][0] = p->temporal_secondary.start;
    segments[num_seg_in_part*i+4][1] = p->temporal_secondary.end;
#endif
  }

  /* Make sure all segments (shadow or otherwise) are disjoint */
  size_t j;
  for (int i = 0; i < num_segments; i++) {
    uintptr_t *src = segments[i];
    DVASSERT(src[0] < src[1],
      "Segment start is greater than segment end %lu < %lu\n", src[0], src[1]);
    for (j = 0; j < num_segments; j++) {
      if (i != j) {
        uintptr_t *dest = segments[j];
        DVASSERT(src[1] < dest[0] || src[0] > dest[1],
          "Segment [%lu, %lu] overlaps with segment [%lu, %lu]",
          src[0], src[1], dest[0], dest[1]);
      }
    }
  }
}

/* Assert that memory layout has been initialized and all segments appear
 * in the expected order */
# define DVALIDATE_SHADOW_LAYOUT validate_shadow_layout()

/* Assert that boundaries of a block [_addr, _addr+_size] are within a segment
 * given by `_s`. `_s` is either HEAP, STACK, TLS, GLOBAL or STATIC. */
#define DVALIDATE_IS_ON(_addr, _size, _s) \
  DVASSERT(IS_ON_##_s(_addr), "Address %a not on %s", _addr, #_s); \
  DVASSERT(IS_ON_##_s(_addr+_size), "Address %a not on %s", _addr+_size, #_s)

/* Assert that [_addr, _addr+_size] are within heap segment */
#define DVALIDATE_IS_ON_HEAP(_addr, _size) \
  DVALIDATE_IS_ON(_addr, _size, HEAP)
/* Assert that [_addr, _addr+_size] are within stack segment */
#define DVALIDATE_IS_ON_STACK(_addr, _size) \
  DVALIDATE_IS_ON(_addr, _size, STACK)
/* Assert that [_addr, _addr+_size] are within global segment */
#define DVALIDATE_IS_ON_GLOBAL(_addr, _size) \
  DVALIDATE_IS_ON(_addr, _size, GLOBAL)
/* Assert that [_addr, _addr+_size] are within TLS segment */
#define DVALIDATE_IS_ON_TLS(_addr, _size) \
  DVALIDATE_IS_ON(_addr, _size, TLS)
/* Assert that [_addr, _addr+_size] are within stack, global or TLS segments */
#define DVALIDATE_IS_ON_STATIC(_addr, _size) \
  DVALIDATE_IS_ON(_addr, _size, STATIC)

/* Assert that `_addr` is on heap and it is the base address of an allocated
 * heap memory block */
#define DVALIDATE_FREEABLE(_addr) \
  DVASSERT(IS_ON_HEAP(_addr), "Expected heap location: %a\n", _addr); \
  DVASSERT(_addr == _base_addr(_addr), \
      "Expected base address, i.e., %a, not %a\n", _base_addr(_addr), _addr);

/* Assert that a memory block [_addr, _addr + _size] is allocated on a
 * program's heap */
# define DVALIDATE_HEAP_ACCESS(_addr, _size) \
    DVASSERT(IS_ON_HEAP(_addr), "Expected heap location: %a\n", _addr); \
    DVASSERT(heap_allocated((uintptr_t)_addr, _size, (uintptr_t)_addr), \
       "Operation on unallocated heap block [%a + %lu]\n",  _addr, _size)

/* Assert that every location belonging to the range [_addr, _addr + _size] is
 * - belongs to a tracked static region (i.e., stack, TLS or global)
 * - not allocated */
# define DVALIDATE_HEAP_FREE(_addr, _size) { \
  uintptr_t i, a = (uintptr_t)_addr; \
  for (i = 0; i < _size; i++) { \
    DVASSERT(IS_ON_HEAP(a + i), "Expected heap location: %a\n", a + i); \
    DVASSERT(!heap_allocated(a + i, 1, a + i), \
      "Expected heap unallocated location: [%a + %lu]\n", a, i); \
  } \
}

/* Assert that memory block [_addr, _addr + _size] is allocated on stack, TLS
 * or globally */
# define DVALIDATE_STATIC_ACCESS(_addr, _size) \
    DVASSERT(IS_ON_STATIC(_addr), \
        "Expected static location: [%a + %lu], \n", _addr, _size); \
    DVASSERT(static_allocated((uintptr_t)_addr, _size,(uintptr_t)_addr), \
       "Operation on unallocated static block [%a + %lu]\n", _addr, _size)

/* Same as ::DVALIDATE_STATIC_LOCATION but for a single memory location */
# define DVALIDATE_STATIC_LOCATION(_addr) \
    DVASSERT(IS_ON_STATIC(_addr), \
      "Expected static location: %a\n", _addr); \
    DVASSERT(static_allocated_one((uintptr_t)_addr), \
      "Operation on unallocated static block [%a]\n", _addr)

/* Assert that every location belonging to the range [_addr, _addr + _size] is
 * - belongs to a tracked static region (i.e., stack, TLS or global)
 * - not allocated */
# define DVALIDATE_STATIC_FREE(_addr, _size) { \
  uintptr_t i, a = (uintptr_t)_addr; \
  for (i = 0; i < _size; i++) { \
    DVASSERT(IS_ON_STATIC(a + i), \
      "Expected static location in freea: %a\n", a + i); \
    DVASSERT(!static_allocated_one(a + i), \
      "Expected static unallocated location in freea: [%a + %lu]\n", a, i); \
  } \
}

/* Assert that neither of `_len - 1` addresses immediately preceding `_addr`
 * are base addresses of some other block and that `_len` addresses past
 * `_addr` are free */
#define DVALIDATE_STATIC_SUFFICIENTLY_ALIGNED(_addr, _len) { \
  int _i; \
  for (_i = 0; _i < _len; _i++) { \
    uintptr_t _prev = _addr - _i; \
    if (static_allocated_one(_prev)) { \
      vassert(_base_addr(_prev) != _prev, \
        "Potential backward overlap of: \n  previous block [%a]\n" \
        "  with allocated block [%a]\n", _prev, _addr); \
    } \
    uintptr_t _next = _addr + _i; \
    vassert(!static_allocated_one(_next), \
      "Potential forward overlap of:\n  following block location [%a]\n" \
      "  with allocated block [%a]\n", _next, _addr); \
  } \
}

/* Assert that a memory block [_addr, _addr + _size] is nullified */
# define DVALIDATE_NULLIFIED(_addr, _size) \
  DVASSERT(zeroed_out((void *)_addr, _size), \
    "Block [%a, %a+%lu] not nullified", _addr, _addr, _size)

/* Assert that memory block [_addr, _addr + _size] is allocated */
# define DVALIDATE_ALLOCATED(_addr, _size, _base) \
  DVASSERT(allocated((uintptr_t)_addr, _size, (uintptr_t)_base), \
    "Operation on unallocated block [%a + %lu] with base %a\n", \
    _addr, _size, _base)

/* Assert that memory block [_addr, _addr + _size] is allocated
 * and can be written to */
# define DVALIDATE_WRITEABLE(_addr, _size, _base) { \
  DVALIDATE_ALLOCATED((uintptr_t)_addr, _size, (uintptr_t)_base); \
  DVASSERT(!readonly((void*)_addr), \
    "Unexpected readonly address: %lu\n", _addr); \
}

#else
/*! \cond exclude from doxygen */
#  define DVALIDATE_MEMORY_INIT
#  define DVALIDATE_SHADOW_LAYOUT
#  define DVALIDATE_HEAP_ACCESS
#  define DVALIDATE_STATIC_ACCESS
#  define DVALIDATE_STATIC_LOCATION
#  define DVALIDATE_ALIGNMENT
#  define DVALIDATE_NULLIFIED
#  define DVALIDATE_IS_ON
#  define DVALIDATE_IS_ON_HEAP
#  define DVALIDATE_IS_ON_STACK
#  define DVALIDATE_IS_ON_GLOBAL
#  define DVALIDATE_IS_ON_TLS
#  define DVALIDATE_IS_ON_STATIC
#  define DVALIDATE_FREEABLE
#  define DVALIDATE_STATIC_FREE
#  define DVALIDATE_HEAP_FREE
#  define DVALIDATE_ALLOCATED
#  define DVALIDATE_WRITEABLE
#  define DVALIDATE_STATIC_SUFFICIENTLY_ALIGNED
/*! \endcond */
#endif
/* }}} */

/* E-ACSL predicates {{{ */
/* See definitions for documentation */
static void *shadow_copy(const void *ptr, size_t size, int init);
static uintptr_t heap_info(uintptr_t addr, char type);
static uintptr_t static_info(uintptr_t addr, char type);
static int heap_allocated(uintptr_t addr, size_t size, uintptr_t base_ptr);
static int static_allocated(uintptr_t addr, long size, uintptr_t base_ptr);
static int allocated(uintptr_t addr, long size, uintptr_t base_ptr);

/*! \brief Quick test to check if a static location belongs to allocation.
 * This macro really belongs where static_allocated is defined, but
 * since it is used across this whole file it needs to be defined here. */
#define static_allocated_one(_addr) \
  (*((unsigned char*)PRIMARY_SHADOW(_addr)))

/*! \brief Shortcut for executing statements based on the segment a given
 * address belongs to.
 * \param intptr_t _addr - a memory address
 * \param code_block _heap_stmt - code executed if `_addr` is a heap address
 * \param code_block _static_stmt - code executed if `_addr` is a static address */
#define TRY_SEGMENT_WEAK(_addr, _heap_stmt, _static_stmt)  \
  if (IS_ON_HEAP(_addr)) { \
    _heap_stmt; \
  } else if (IS_ON_STATIC(_addr)) { \
    _static_stmt; \
  }

/*! \brief Same as TRY_SEGMENT but performs additional checks aborting the
 * execution if the given address is `NULL` or does not belong to known
 * segments. Note that `NULL` also does not belong to any of the tracked
 * segments but it is treated separately for debugging purposes.
 *
 * The \b WEAK notion refers to the behaviour where no action is performed if
 * the given address does not belong to any of the known segments. */
#define TRY_SEGMENT(_addr, _heap_stmt, _static_stmt) { \
  TRY_SEGMENT_WEAK(_addr, _heap_stmt, _static_stmt) \
  else { \
    vassert(0, "Use of invalid address %a in %s\n", _addr, __func__); \
  } \
}

/*! \brief Wrapper around ::heap_info and ::static_info functions that
 * dispatches one of the above functions based on the type of supplied memory
 * address (`addr`) (static, global, tls or heap). For the case when the
 * supplied address does not belong to the track segments 0 is returned.
 *
 * \param uintptr_t addr - a memory address
 * \param char p - predicate type. See ::static_info for further details. */
static uintptr_t predicate(uintptr_t addr, char p) {
  TRY_SEGMENT(
    addr,
    return heap_info((uintptr_t)addr, p),
    return static_info((uintptr_t)addr, p));
  return 0;
}

/*! \brief Return the byte length of the memory block containing `_addr` */
#define _block_length(_addr) predicate((uintptr_t)_addr, 'L')
/*! \brief Return the base address of the memory block containing `_addr` */
#define _base_addr(_addr) predicate((uintptr_t)_addr, 'B')
/* }}} */

/* Static allocation {{{ */

/** The below numbers identify offset "bases" for short block lengths.
 * An offset base is a number (a code) that represents the length of a
 * short block with a byte offset of `0`.
 * For instance, for a block of 4 bytes its offset base if 7, that is
 *  length 4, offset 0 => 7,
 *  length 4, offset 1 => 8,
 *  length 4, offset 2 => 9,
 *  length 4, offset 3 => 10,
 * and then for a block of 5 bytes its base offset if 11 etc. */
static const char short_offsets_base [] = { 0, 1, 2, 4, 7, 11, 16, 22, 29 };

/** Shadow masks for setting values of short blocks */
static const uint64_t short_shadow_masks[] = {
  0UL,
  4UL,
  3080UL,
  1578000UL,
  673456156UL,
  258640982060UL,
  92703853921344UL,
  31644393008028760UL,
  10415850140873816180UL
};

/*! \brief Record allocation of a given memory block and update shadows
 *  using offset-based encoding.
 *
 * \param ptr - pointer to a base memory address of the stack memory block.
 * \param size - size of the stack memory block. */
static void shadow_alloca(void *ptr, size_t size) {
  DVALIDATE_IS_ON_STATIC(ptr, size);
#ifdef E_ACSL_TEMPORAL
  /* Make sure that during temporal analysis there is
   * sufficient space to store an origin timestamp.
   * NOTE: This does not apply to globals, because all the globals
   * have the timestamp of `1`. */
  if (!IS_ON_GLOBAL(ptr)) {
    DVALIDATE_STATIC_SUFFICIENTLY_ALIGNED((uintptr_t)ptr, 4);
  }
#endif

  unsigned char *prim_shadow = (unsigned char*)PRIMARY_SHADOW(ptr);
  uint64_t *prim_shadow_alt = (uint64_t *)PRIMARY_SHADOW(ptr);
  unsigned int *sec_shadow = (unsigned int*)SECONDARY_SHADOW(ptr);

  /* Make sure shadows are nullified */
  DVALIDATE_NULLIFIED(prim_shadow, size);
  DVALIDATE_NULLIFIED(sec_shadow, size);

  /* Flip read-only bit for zero-size blocks. That is, physically it exists
   * but one cannot write to it. Further, the flipped read-only bit will also
   * identify such block as allocated */
  if (!size)
    setbit(READONLY_BIT, prim_shadow[0]);

  unsigned int i, j = 0, k = 0;
  if (IS_LONG_BLOCK(size)) { /* Long blocks */
    unsigned int i, j = 0, k = 0;
    int boundary = LONG_BLOCK_BOUNDARY(size);
    for (i = 0; i < boundary; i += LONG_BLOCK) {
      /* Set-up a secondary shadow segment */
      sec_shadow[j++] = size;
      sec_shadow[j++] = i;
      /* Set primary shadow offsets */
      prim_shadow_alt[k++] = LONG_BLOCK_MASK;
    }

    /* Write out the remainder */
    for (i = boundary; i < size; i++) {
      unsigned char offset = i%LONG_BLOCK + LONG_BLOCK_INDEX_START + LONG_BLOCK;
      prim_shadow[i] = (offset << 2);
    }
  } else { /* Short blocks */
    for (i = 0; i < size; i++) {
      unsigned char code = short_offsets_base[size] + i;
      prim_shadow[i] = (code << 2);
    }
  }
#ifdef E_ACSL_TEMPORAL /*{{{*/
  /* Store a temporal origin timestamp in the first 4 bytes of a temporal
   * shadow. This, however applies only to TLS of stack blocks. Global blocks
   * are never deallocated, an origin time stamp of any global block is given
   * via `GLOBAL_TEMPORAL_TIMESTAMP` */
  if (!IS_ON_GLOBAL(ptr)) {
    uint32_t* temporal_shadow = (uint32_t*)TEMPORAL_PRIMARY_STATIC_SHADOW(ptr);
    *temporal_shadow = NEW_TEMPORAL_TIMESTAMP();
  }
#endif /*}}} E_ACSL_TEMPORAL*/
}
/* }}} */

/* Deletion of static blocks {{{ */

/*! \brief Nullifies shadow regions of a memory block given by its address.
 * \param ptr - base memory address of the stack memory block. */
void shadow_freea(void *ptr) {
  DVALIDATE_STATIC_LOCATION(ptr);
  DASSERT(ptr == (void*)_base_addr(ptr));
  size_t size = _block_length(ptr);
  memset((void*)PRIMARY_SHADOW(ptr), 0, size);
  memset((void*)SECONDARY_SHADOW(ptr), 0, size);
#ifdef E_ACSL_TEMPORAL /*{{{*/
  memset((void*)TEMPORAL_PRIMARY_STATIC_SHADOW(ptr), 0, size);
  memset((void*)TEMPORAL_SECONDARY_STATIC_SHADOW(ptr), 0, size);
#endif  /*}}} E_ACSL_TEMPORAL*/
}
/* }}} */

/* Static querying {{{ */

/*! \brief Return a non-zero value if a memory region of length `size`
 * starting at address `addr` belongs to a tracked stack, tls or
 * global memory block and 0 otherwise.
 * This function is only safe if applied to a tls, stack or global address.
 * Explanations regarding the third argument - `base_ptr` - are given
 * via inline documentation of function ::heap_allocated */
static int static_allocated(uintptr_t addr, long size, uintptr_t base_ptr) {
  unsigned char *prim_shadow = (unsigned char*)PRIMARY_SHADOW(addr);
  /* Unless the address belongs to tracked allocation 0 is returned */
  if (prim_shadow[0]) {
    unsigned int code = (prim_shadow[0] >> 2);
    unsigned int long_block = (code >= LONG_BLOCK_INDEX_START);
    size_t length, offset;
    if (long_block) {
      offset = code - LONG_BLOCK_INDEX_START;
      unsigned int *sec_shadow =
        (unsigned int*)SECONDARY_SHADOW(addr - offset) ;
      length = sec_shadow[0];
      offset = sec_shadow[1] + offset;
    } else {
      offset = short_offsets[code];
      length = short_lengths[code];
    }

#ifndef E_ACSL_WEAK_VALIDITY
    if (addr != base_ptr) {
      return BELONGS(base_ptr, addr - offset, length)
        && offset + size <= length;
    }
#endif
    return offset + size <= length;
  }
  return 0;
}

/*! \brief Return a non-zero value if a statically allocated memory block
 * starting at `addr` of length `size` is fully initialized (i.e., each of
 * its cells is initialized). */
static int static_initialized(uintptr_t addr, long size) {
  /* Return 0 right away if the address does not belong to
   * static allocation */
  if (!static_allocated(addr, size, addr))
    return 0;
  DVALIDATE_STATIC_ACCESS(addr, size);

  int result = 1;
  uint64_t *shadow = (uint64_t*)PRIMARY_SHADOW(addr);
  while (size > 0) {
    int rem = (size >= ULONG_BYTES) ? ULONG_BYTES : size;
    uint64_t mask = static_init_masks[rem];
    size -= ULONG_BYTES;
    /* Note that most of the blocks checked for initialization will be smaller
    * than 64 bits, therefore in most cases it is more efficient to complete
    * the loop rather than do a test and return if the result is false */
    result = result && (((*shadow) & mask) == mask);
    shadow++;
  }
  return result;
}

/*! \brief Checking whether a globally allocated memory block containing an
 * address _addr has read-only access. Note, this is light checking that
 * relies on the fact that a single block cannot contain read/write and
 * read-only parts, that is to check whether the block has read-only access it
 * is sufficient to check any of its bytes. */
#define global_readonly(_addr) \
  checkbit(READONLY_BIT, (*(char*)PRIMARY_GLOBAL_SHADOW(addr)))

/*! \brief Querying information about a specific global or stack memory address
 * (based on the value of parameter `global'). The return value is interpreted
 * based on the second argument that specifies parameters of the query:
 *
 * - 'B' - return the base address of the memory block `addr` belongs to or `0`
 *     if `addr` lies outside of tracked allocation.
 * - 'O' - return the offset of `addr` within its memory block or `0`
 *     if `addr` lies outside of tracked allocation.
 * - 'L' - return the size in bytes of the memory block `addr` belongs to or `0`
 *     if `addr` lies outside of tracked allocation.
 *
 * NB: One should make sure that a given address is allocated before querying.
 * That is, for the cases when addr does not refer to an allocated memory
 * address belonging to static allocation the return value for this function is
 * unspecified. */
static uintptr_t static_info(uintptr_t addr, char type) {
  DVALIDATE_STATIC_LOCATION(addr);
  unsigned char *prim_shadow = (unsigned char*)PRIMARY_SHADOW(addr);

  /* Unless the address belongs to tracked allocation 0 is returned */
  if (prim_shadow[0]) {
    unsigned int code = (prim_shadow[0] >> 2);
    unsigned int long_block = (code >= LONG_BLOCK_INDEX_START);
    if (long_block) {
      unsigned int offset = code - LONG_BLOCK_INDEX_START;
      unsigned int *sec_shadow =
        (unsigned int*)SECONDARY_SHADOW(addr - offset) ;
      switch(type) {
        case 'B': /* Base address */
          return addr - offset - sec_shadow[1];
        case 'O': /* Offset */
          return sec_shadow[1] + offset;
        case 'L': /* Length */
          return sec_shadow[0];
        default:
          DASSERT(0 && "Unknown static query type");
      }
    } else {
      switch(type) {
        case 'B': /* Base address */
          return addr - short_offsets[code];
        case 'O': /* Offset */
          return short_offsets[code];
        case 'L': /* Length */
          return short_lengths[code];
        default:
          DASSERT(0 && "Unknown static query type");
      }
    }
  }
  return 0;
}

#ifdef E_ACSL_TEMPORAL /*{{{*/
/*! Return either an origin (if `origin` is non-zero) or referent timestamp
 *  associated with a static address `addr` */
static uint32_t static_temporal_info(uintptr_t addr, int origin) {
  /* NOTE: No checking for allocated blocks, since an invalid
   timestamp is zero and ununsed memory is nullified then an invalid
   timestamp is also returned for allocated memory */
  if (origin) {
    int allocated = static_allocated_one(addr);
    if (allocated && !IS_ON_GLOBAL(addr)) {
      uintptr_t base = static_info(addr, 'B');
      return *((uint32_t*)TEMPORAL_PRIMARY_STATIC_SHADOW(base));
    } else if (allocated && IS_ON_GLOBAL(addr)) {
      return GLOBAL_TEMPORAL_TIMESTAMP;
    } else {
      return INVALID_TEMPORAL_TIMESTAMP;
    }
  } else {
    return *((uint32_t*)TEMPORAL_SECONDARY_STATIC_SHADOW(addr));
  }
}

#define static_origin_timestamp(_ptr) static_temporal_info((uintptr_t)(_ptr),1)
#define static_referent_timestamp(_ptr) static_temporal_info((uintptr_t)(_ptr),0)

/*! Store a referent time stamp associated with a static pointer.
 *  Origin timestamps are generated via `shadow_alloca` */
static void static_store_temporal_referent(uintptr_t addr, uint32_t ref) {
  DVALIDATE_STATIC_ACCESS(addr, PTR_SZ);
  *((uint32_t*)TEMPORAL_SECONDARY_STATIC_SHADOW(addr)) = ref;
}
#endif/*}}} E_ACSL_TEMPORAL*/
/* }}} */

/* Static initialization {{{ */
/*! \brief The following function marks n bytes starting from the address
 * given by addr as initialized. `size` equating to zero indicates that the
 * whole block should be marked as initialized.  */
static void initialize_static_region(uintptr_t addr, long size) {
  DVALIDATE_STATIC_ACCESS(addr, size);
  DVASSERT(!(addr - _base_addr(addr) + size > _block_length(addr)),
    "Attempt to initialize %lu bytes past block boundaries\n"
    "starting at %a with block length %lu at base address %a\n",
    size, addr, _block_length(addr), _base_addr(addr));

  /* Below code marks `size` bytes following `addr` in the stack shadow as
   * initialized. That is, least significant bits of all 9 bytes following
   * `addr` should be flipped to ones. While this is a common pattern in this
   * program, here are some explanations.
   *
   * Here we grab a shadow region and initialize 8 (::ULONG_SIZE) bits at a
   * time using masks stored in ::static_init_masks. This few lines below are
   * better explained using an example. Let's say we need to mark 9 bytes as
   * initialized starting from some address `addr`.
   *
   * In order to do that we first grab a shadow region storing it in `shadow`.
   * For the first 8 bytes we grab a mask stored at ::static_init_masks[8]:
   *   `10000000 10000000 10000000 10000000 10000000 10000000 10000000 10000000`
   * That is, `*shadow |= static_init_masks[8]` sets 8 lowest significant bits
   * of the 8 bytes following *shadow to ones.
   *
   * After that we need to mark the remaining 1 bite as initialized. For that
   * we grab mask ::static_init_masks[1]:
   *  `10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000`
   * That is, `*shadow |= static_init_masks[1]` will set only the least
   * significant bit in *shadow. */

  uint64_t *shadow = (uint64_t*)PRIMARY_SHADOW(addr);
  while (size > 0) {
    int rem = (size >= ULONG_BYTES) ? ULONG_BYTES : size;
    size -= ULONG_BYTES;
    *shadow |= static_init_masks[rem];
    shadow++;
  }
}
/* }}} */

/* Read-only {{{ */
/*! \brief Mark n bytes starting from the address given by `ptr` as initialized.
 * NOTE: This function has many similarities with ::initialize_static_region
 * The functionality, however is preferred to be kept separate
 * because the ::mark_readonly should operate only on the global shadow. */
static void mark_readonly_region (uintptr_t addr, long size) {
  /* Since read-only blocks can only be stored in the globals  segments (e.g.,
   * TEXT), this function required ptr carry a global address. */
  DASSERT(IS_ON_GLOBAL(addr));
  DASSERT(static_allocated_one(addr));
  DVASSERT(!(addr - _base_addr(addr) + size > _block_length(addr)),
    "Attempt to mark read-only %lu bytes past block boundaries\n"
    "starting at %a with block length %lu at base address %a\n",
    size, addr, _block_length(addr), _base_addr(addr));

  /* See comments in ::initialize_static_region for details */
  uint64_t *shadow = (uint64_t*)PRIMARY_GLOBAL_SHADOW(addr);
  while (size > 0) {
    int rem = (size >= ULONG_BYTES) ? ULONG_BYTES : size;
    size -= ULONG_BYTES;
    *shadow |= static_readonly_masks[rem];
    shadow++;
  }
}
/* }}} */

/* Heap allocation {{{ (malloc/calloc) */

/*! \brief Create a heap shadow for an allocated memory block starting at `ptr`
 * and of length `size`. Optionally mark it as initialized if `init`
 * evaluates to a non-zero value.
 * \b NOTE: This function assumes that `ptr` is a base address of a
 * heap-allocated memory block, such that HEAP_SEGMENT bytes preceding `ptr`
 * correspond to `unusable space`.
 * \b WARNING: Current implementation assumes that the size of a heap segment
 * does not exceed 64 bytes. */
static void set_heap_segment(void *ptr, size_t size, size_t alloc_size,
    size_t init, const char *function) {

  /* Make sure that heap memspace has not been moved. This is likely if
     a really large chunk has been requested to be allocated. */
  vassert(mem_spaces.heap_mspace_least ==
    (uintptr_t)mspace_least_addr(mem_spaces.heap_mspace),
    "Exceeded heap allocation limit of %luMB -- heap memory space moved. \n",
    E_ACSL_HEAP_SIZE);

  /* Similar check, make sure that allocated space does not exceed given
     allocation limit for mspace */
  uintptr_t max_addr = (uintptr_t)ptr + alloc_size;
  vassert(mem_spaces.heap_end > max_addr,
    "Exceeded heap allocation limit of %luMB\n", E_ACSL_HEAP_SIZE);

  DVALIDATE_MEMORY_INIT;
  /* Ensure the shadowed block in on the tracked heap portion */
  DVALIDATE_IS_ON_HEAP(((uintptr_t)ptr) - HEAP_SEGMENT, size);
  DVALIDATE_ALIGNMENT(ptr); /* Make sure alignment is right */
  update_heap_allocation(size); /* Adjust tracked allocation size */

  /* Get aligned size of the block, i.e., an actual size of the
   * allocated block */
  unsigned char *shadow = (unsigned char*)HEAP_SHADOW(ptr);

  /* Make sure shadow is nullified before setting it */
  DVALIDATE_NULLIFIED(shadow, alloc_size);

  /* The overall number of block segments in a tracked memory block  */
  size_t segments = alloc_size/HEAP_SEGMENT;
  uintptr_t *segment = (uintptr_t*)(shadow);
  segment[1] = size;

#ifdef E_ACSL_TEMPORAL /*{{{*/
  /* 4 bytes following a block's length store an origin timestamp */
  segment[2] = NEW_TEMPORAL_TIMESTAMP();
#endif /*}}} E_ACSL_TEMPORAL*/

  int i;
  /* Write the offsets per segment */
  for (i = 0; i < segments; i++) {
    segment = (uintptr_t*)(shadow + i*HEAP_SEGMENT);
    *segment = (uintptr_t)ptr;
  }

  /* If init is a non-zero value then mark all allocated bytes as initialized */
  if (init) {
    memset((void*)HEAP_INIT_SHADOW(ptr), (unsigned int)ONE, alloc_size/8);
  }
}

/*! \brief Replacement for a malloc function that additionally tracks the
 * allocated memory block.
 *
 * NOTE: This malloc returns a `NULL` pointer if the requested size is `0`.
 * Such behaviour is compliant with the C99 standard, however it differs from
 * the behaviour of the GLIBC malloc, which returns a zero-size block instead.
 * The standard indicates that a return value for a zero-sized allocation
 * is implementation specific:
 *    "If the size of the space requested is zero, the behaviour is
 *    implementation-defined: either a null pointer is returned, or the
 *    behaviour is as if the size were some non-zero value, except that the
 *    returned pointer shall not be used to access an object." */
void* malloc(size_t size) {
  size_t alloc_size = ALLOC_SIZE(size);

  /* Return NULL if the size is too large to be aligned */
  char* res = alloc_size ? (char*)public_malloc(alloc_size) : NULL;

  if (res) {
    /* Make sure there is sufficient room in shadow */
    set_heap_segment(res, size, alloc_size, 0, "malloc");
  }

  return res;
}

/*! \brief  Replacement for `calloc` that enables memory tracking */
void* calloc(size_t nmemb, size_t size) {
  /* Since both `nmemb` and `size` are both of size `size_t` the multiplication
   * of the arguments (which gives the actual allocation size) might lead to an
   * integer overflow. The below code checks for an overflow and sets the
   * `alloc_size` (argument a memory allocation function) to zero. */
  size = (size && nmemb > SIZE_MAX/size) ? 0 : nmemb*size;

  size_t alloc_size = ALLOC_SIZE(size);

  /* Since aligned size is required by the model do the allocation through
   * `malloc` and nullify the memory space by hand */
  char* res =
    size ? (char*)public_malloc(alloc_size) : NULL;

  if (res) {
    /* Make sure there is sufficient room in shadow */
    memset(res, 0, size);
    set_heap_segment(res, size, alloc_size, 1, "calloc");
  }
  return res;
}

/** \brief Return shadowed copy of a memory chunk on a program's heap using.
 * If `init` parameter is set to a non-zero value the memory occupied by the
 * resulting block is set to be initialized and uninitialized otherwise. */
static void *shadow_copy(const void *ptr, size_t size, int init) {
  char *ret = (init) ?	calloc(1, size) : malloc(size);
  vassert(ret != NULL, "Shadow copy failed\n", NULL);
  /* Shadow copy is internal, therefore heap status should not be updated.
     Since it is set via `set_heap_segment`, it needs to be reverted back. */
  update_heap_allocation(-size);
  return memcpy(ret, ptr, size);
}
/* }}} */

/* Heap deallocation (free) {{{ */

/*! \brief Remove a memory block with base address given by `ptr` from tracking.
 * This function effectively nullifies block shadow tracking an application
 * block.
 *
 * NOTE: ::unset_heap_segment assumes that `ptr` is a base address of an
 * allocated heap memory block, i.e., `freeable(ptr)` evaluates to true.
 *
 * \param ptr - base address of the memory block to be removed from tracking
 * \param init - if evaluated to a non-zero value then initialization shadow
 *  of the memory block with base address `ptr` is nullified as well.
 * \param function - name of the de-allocation function (e.g., `free` or `cfree`)
*/
static void unset_heap_segment(void *ptr, int init, const char *function) {
  DVALIDATE_MEMORY_INIT;
  DVALIDATE_FREEABLE(((uintptr_t)ptr));
  /* Base address of shadow block */
  uintptr_t *base_shadow = (uintptr_t*)HEAP_SHADOW(ptr);
  /* Physical allocation size */
  size_t alloc_size = ALLOC_SIZE(base_shadow[1]);
  /* Actual block length */
  size_t length = base_shadow[1];
  /* Nullify shadow block */
  memset(base_shadow, ZERO, alloc_size);
  /* Adjust tracked allocation size */
  heap_allocation_size -= length;
#ifdef E_ACSL_TEMPORAL /*{{{*/
  /* Nullify temporal shadow */
  uintptr_t *t_base_shadow = (uintptr_t*)TEMPORAL_HEAP_SHADOW(ptr);
  memset(t_base_shadow, ZERO, alloc_size);
#endif /*}}} E_ACSL_TEMPORAL*/
  /* Nullify init shadow */
  if (init) {
    memset((void*)HEAP_INIT_SHADOW(ptr), 0, alloc_size/8);
  }
}

/*! \brief Replacement for `free` with memory tracking  */
void free(void *ptr) {
  if (ptr == NULL) {
/* Fail if instructed to treat NULL input to free as invalid. */
#ifdef E_ACSL_FREE_VALID_ADDRESS
    vabort("NULL pointer in free\n");
#endif
    return;
  }

  if (ptr != NULL) { /* NULL is a valid behaviour */
    if (freeable(ptr)) {
      unset_heap_segment(ptr, 1, "free");
      public_free(ptr);
    } else {
      vabort("Not a start of block (%a) in free\n", ptr);
    }
  }
}
/* }}} */

/* Heap reallocation (realloc) {{{ */
void* realloc(void *ptr, size_t size) {
  char *res = NULL; /* Resulting pointer */
  /* If the pointer is NULL then realloc is equivalent to malloc(size) */
  if (ptr == NULL)
    return malloc(size);
  /* If the pointer is not NULL and the size is zero then realloc is
   * equivalent to free(ptr) */
  else if (ptr != NULL && size == 0) {
    free(ptr);
  } else {
    if (freeable(ptr)) { /* ... and can be used as an input to `free` */
      size_t alloc_size = ALLOC_SIZE(size);
      res = public_realloc(ptr, alloc_size);
      DVALIDATE_ALIGNMENT(res);

      /* realloc succeeds, otherwise nothing needs to be done */
      if (res != NULL) {
        size_t alloc_size = ALLOC_SIZE(size);
        size_t old_size = _block_length(ptr);
        size_t old_alloc_size = ALLOC_SIZE(old_size);

        /* Nullify old representation */
        unset_heap_segment(ptr, 0, "realloc");

        /* Set up new block shadow */
        set_heap_segment(res, size, alloc_size, 0, "realloc");

        /* Move init shadow */
        unsigned char* old_init_shadow  = (unsigned char*)HEAP_INIT_SHADOW(ptr);
        unsigned char* new_init_shadow  = (unsigned char*)HEAP_INIT_SHADOW(res);

        /* If realloc truncates allocation in the old init shadow it is first
         * needed to clear the old init shadow from the boundary of the old
         * shadow block to the size of the new allocation */
        if (old_size > size) {
          clearbits_right(
              old_alloc_size - size,
              old_init_shadow + old_alloc_size/8);
        }

        /* Now init shadow can be moved (if needed), keep in mind that
         * segment base addresses are aligned at a boundary of something
         * divisible by 8, so instead of moving actual bits here the
         * segments are moved to avoid dealing with bit-level operations
         * on incomplete bytes. */
        if (res != ptr) {
          size_t copy_size = (old_size > size) ? alloc_size : old_alloc_size;
          memcpy(new_init_shadow, old_init_shadow, copy_size);
          memset(old_init_shadow, 0, copy_size);
        }
      }
    } else {
      vabort("Not a start of block (%a) in realloc\n", ptr);
    }
  }
  return res;
}
/* }}} */

/* Heap aligned allocation (aligned_alloc) {{{ */
/*! \brief Replacement for `aligned_alloc` with memory tracking */
void *aligned_alloc(size_t alignment, size_t size) {
  /* Check if:
   *  - size and alignment are greater than zero
   *  - alignment is a power of 2
   *  - size is a multiple of alignment */
  if (!size || !alignment || !powof2(alignment) || (size%alignment))
    return NULL;

  char *res = public_aligned_alloc(alignment, size);

  if (res) {
    set_heap_segment(res, size, ALLOC_SIZE(size), 0, "aligned_alloc");
  }

  return (void*)res;
}
/* }}} */

/* Heap aligned allocation (posix_memalign) {{{ */
/*! \brief Replacement for `posix_memalign` with memory tracking */
int posix_memalign(void **memptr, size_t alignment, size_t size) {
 /* Check if:
   *  - size and alignment are greater than zero
   *  - alignment is a power of 2 and a multiple of sizeof(void*) */
  if (!size || !alignment || !powof2(alignment) || alignment%sizeof(void*))
    return -1;

  /* Make sure that the first argument to posix memalign is indeed allocated */
  vassert(allocated((uintptr_t)memptr, sizeof(void*), (uintptr_t)memptr),
      "\\invalid memptr in  posix_memalign", NULL);

  int res = public_posix_memalign(memptr, alignment, size);
  if (!res) {
    set_heap_segment(*memptr, size, ALLOC_SIZE(size), 0, "posix_memalign");
  }
  return res;
}
/* }}} */

/* Heap querying {{{ */
/*! \brief Return a non-zero value if a memory region of length `size`
 * starting at address `addr` belongs to an allocated (tracked) heap memory
 * block and a 0 otherwise. Note, this function is only safe if applied to a
 * heap address.
 *
 * Note the third argument `base_ptr` that represents the base of a pointer, i.e.,
 * `addr` of the form `base_ptr + i`, where `i` is some integer index.
 * ::heap_allocated also returns zero if `base_ptr` and `addr` belong to different
 * memory blocks, or if `base_ptr` lies within unallocated region. The intention
 * here is to be able to detect dereferencing of an allocated memory block through
 * a pointer to a different block. Consider, for instance, some pointer `p` that
 * points to a memory block `B`, and an index `i`, such that `p+i` references a
 * memory location belonging to a different memory block (say `C`). From a
 * low-level viewpoint, dereferencing `p+i` is safe (since it belongs to a properly
 * allocated block). From our perspective, however, dereference of `p+i` is
 * only legal if both `p` and `p+i` point to the same block. */
static int heap_allocated(uintptr_t addr, size_t size, uintptr_t base_ptr) {
  /* Base address of the shadow segment the address belongs to */
  uintptr_t *shadow = (uintptr_t*)HEAP_SHADOW(addr - addr%HEAP_SEGMENT);

  /* Non-zero if the segment belongs to heap allocation */
  if (shadow[0]) {
    uintptr_t *base_shadow =
      (uintptr_t*)HEAP_SHADOW(base_ptr - base_ptr%HEAP_SEGMENT);
    uintptr_t *first_segment = (uintptr_t*)HEAP_SHADOW(shadow[0]);
    /* shadow[0] - base address of the tracked block
     * fist_segment[1] - length (i.e., location in the first segment
     *  after base address)
     * offset is the difference between the address and base address (shadow[0])
     * Then an address belongs to heap allocation if
     *  offset + size <= length
     *
     * Additionally, if strong validity is enforced
     * (i.e., E_ACSL_WEAK_VALIDITY macro undefined) make sure that both
     * `addr` and `base_ptr` belong to the same block. */
#ifndef E_ACSL_WEAK_VALIDITY
    return base_shadow[0] == shadow[0] &&
      (addr - shadow[0]) + size <= first_segment[1];
#else
    return (addr - shadow[0]) + size <= first_segment[1];
#endif
  }
  return 0;
}

/*! \brief  Return a non-zero value if a given address is a base address of a
 * heap-allocated memory block that `addr` belongs to.
 *
 * As some of the other functions, \b \\freeable can be expressed using
 * ::IS_ON_HEAP, ::heap_allocated and ::_base_addr. Here direct
 * implementation is preferred for performance reasons. */
int freeable(void *ptr) { /* + */
  uintptr_t addr = (uintptr_t)ptr;
  /* Address is not on the program's heap, so cannot be freed */
  if (!IS_ON_HEAP(addr))
    return 0;

  /* Address of the shadow segment the address belongs to */
  uintptr_t *shadow = (uintptr_t*)ALIGNED_HEAP_SHADOW(addr);
  /* Non-zero if the segment belongs to heap allocation with *shadow
   * capturing the base address of the tracked block */
  if (*shadow) {
    /* Block is freeable if `addr` is the base address of its block  */
    return (uintptr_t)*shadow == addr;
  }
  return 0;
}

/*! \brief Querying information about a specific heap memory address.
 * This function is similar to ::static_info except it returns data
 * associated with dynamically allocated memory.
 * See in-line documentation for ::static_info for further details. */
static uintptr_t heap_info(uintptr_t addr, char type) {
  /* Ensure that `addr` is an allocated location on a program's heap */
  DVALIDATE_HEAP_ACCESS(addr, 1);
  /* Base address of the shadow segment the address belongs to.
   * First `sizeof(uintptr_t)` bytes of each segment store application-level
   * base address of the tracked block */
  uintptr_t *aligned_shadow = (uintptr_t*)ALIGNED_HEAP_SHADOW(addr);

  switch(type) {
    case 'B': /* Base address */
      return *aligned_shadow;
    case 'L': { /* Block length */
      /* Pointer to the first-segment in the shadow block */
      uintptr_t *base_segment = (uintptr_t*)HEAP_SHADOW(*aligned_shadow);
      /* Length of the stored block is captured in `sizeof(uintptr_t)` bytes
       * past `sizeof(uintptr_t)` tracking the base address */
      return base_segment[1];
    }
    case 'O':
      /* Offset of a given address within its block. This is the difference
       * between the input address and the base address of the block. */
      return addr - *aligned_shadow;
    default:
      DASSERT(0 && "Unknown heap query type");
  }
  return 0;
}

/*! \brief Implementation of the \b \\initialized predicate for heap-allocated
 * memory. NB: If `addr` does not belong to an allocated heap block this
 * function returns 0. */
static int heap_initialized(uintptr_t addr, long len) {
  /* Base address of a shadow segment addr belongs to */
  unsigned char *shadow = (unsigned char*)(HEAP_INIT_SHADOW(addr));

  /* See comments in the `initialize_heap_region` function for more details */
  unsigned skip = (addr - HEAP_START)%8;
  unsigned set;
  if (skip) {
    set = 8 - skip;
    set = (len > set) ? set : len;
    len -= set;
    unsigned char mask = 0;
    setbits64_skip(set,mask,skip);

    if (*shadow != mask)
      return 0;
  }
  if (len > 0)
    return checkbits(len, shadow);
  return 1;
}

/* }}} */

/* Heap temporal querying {{{*/
#ifdef E_ACSL_TEMPORAL
static uint32_t heap_temporal_info(uintptr_t addr, int origin) {
  /* NOTE: No checking for allocated blocks, since an invalid
     timestamp is zero and unused memory is nullified then an invalid
     timestamp is also returned for allocated memory */
  if (origin) {
    uintptr_t *aligned_shadow = (uintptr_t*)ALIGNED_HEAP_SHADOW(addr);
    uintptr_t *base_shadow = (uintptr_t*)HEAP_SHADOW(*aligned_shadow);
    return (uint32_t)base_shadow[2];
  } else {
    return *((uint32_t*)TEMPORAL_HEAP_SHADOW(addr));
  }
}

#define heap_origin_timestamp(_ptr)   heap_temporal_info((uintptr_t)(_ptr),1)
#define heap_referent_timestamp(_ptr) heap_temporal_info((uintptr_t)(_ptr),0)

static void heap_store_temporal_referent(uintptr_t addr, uint32_t ref) {
  DVALIDATE_HEAP_ACCESS(addr, PTR_SZ);
  uint32_t *temporal_shadow = (uint32_t*)TEMPORAL_HEAP_SHADOW(addr);
  *temporal_shadow = ref;
}
#endif/*}}} E_ACSL_TEMPORAL*/

/* Heap initialization {{{ */
/*! \brief Mark n bytes on the heap starting from address addr as initialized */
static void initialize_heap_region(uintptr_t addr, long len) {
  DVALIDATE_HEAP_ACCESS(addr, len);
  DVASSERT(!(addr - _base_addr(addr) + len > _block_length(addr)),
    "Attempt to initialize %lu bytes past block boundaries\n"
    "starting at %a with block length %lu at base address %a\n",
    len, addr, _block_length(addr), _base_addr(addr));

  /* Address within init shadow tracking initialization  */
  unsigned char *shadow = (unsigned char*)(HEAP_INIT_SHADOW(addr));

  /* First check whether the address in the init shadow is divisible by 8
   * (i.e., located on a byte boundary) */
  /* Leading bits in `*shadow` byte which do not need to be set
   * (i.e., skipped) */
  short skip = (addr - HEAP_START)%8;
  if (skip) {
    /* The remaining bits in the shadow byte */
    short set = 8 - skip;
    /* The length of initialized region can be short (shorter then the
     * above remainder). Adjust the number of bits to set accordingly. */
    set = (len > set) ? set : len;
    len -= set;
    setbits64_skip(set, *shadow, skip);
    /* Move to the next location if there are more bits to set */
    shadow++;
  }

  if (len > 0) {
    /* Set the remaining bits. Note `shadow` is now aligned at a byte
     * boundary, thus one can set `len` bits starting with address given by
     * `shadow` */
    setbits(len, shadow);
  }
}
/* }}} */

/* Any allocation {{{ */
/*! \brief Amalgamation of ::heap_allocated and ::static_allocated */
static int allocated(uintptr_t addr, long size, uintptr_t base) {
  TRY_SEGMENT_WEAK(addr,
    return heap_allocated(addr, size, base),
    return static_allocated(addr, size, base));
  if (!IS_ON_VALID(addr))
    return 0;
  return 0;
}
/* }}} */

/* Internal state print (debug mode) {{{ */
#ifdef E_ACSL_DEBUG
/* ! \brief Print human-readable representation of a byte in a primary
 * shadow */
static void printbyte(unsigned char c, char buf[]) {
  if (c >> 2 < LONG_BLOCK_INDEX_START) {
    rtl_sprintf(buf, "PRIMARY: I{%u} RO{%u} OF{%2u} => %u[%u]",
      checkbit(INIT_BIT,c), checkbit(READONLY_BIT,c), c >> 2,
      short_lengths[c >> 2], short_offsets[c >> 2]);
  } else {
    rtl_sprintf(buf, "SECONDARY:  I{%u} RO{%u} OF{%u} => %4u",
      checkbit(INIT_BIT,c), checkbit(READONLY_BIT,c),
      (c >> 2), (c >> 2) - LONG_BLOCK_INDEX_START);
  }
}

/*! \brief Print human-readable (well, ish) representation of a memory block
 * using primary and secondary shadows. */
static void print_static_shadows(uintptr_t addr, size_t size) {
  char prim_buf[256];
  char sec_buf[256];

  unsigned char *prim_shadow = (unsigned char*)PRIMARY_SHADOW(addr);
  unsigned int *sec_shadow = (unsigned int*)SECONDARY_SHADOW(addr);

  int i, j = 0;
  for (i = 0; i < size; i++) {
    sec_buf[0] = '\0';
    printbyte(prim_shadow[i], prim_buf);
    if (IS_LONG_BLOCK(size) && (i%LONG_BLOCK) == 0) {
      j += 2;
      if (i < LONG_BLOCK_BOUNDARY(size)) {
        rtl_sprintf(sec_buf, " %a  SZ{%u} OF{%u}",
          &sec_shadow[j], sec_shadow[j-2], sec_shadow[j-1]);
      }
      if (i) {
        DLOG("---------------------------------------------\n");
      }
    }
    DLOG("| [%2d] %a | %s || %s\n", i, &prim_shadow[i], prim_buf, sec_buf);
  }
#ifdef E_ACSL_TEMPORAL /* {{{ */
  uint32_t* origin_shadow = (uint32_t*)TEMPORAL_PRIMARY_STATIC_SHADOW(addr);
  uint32_t* ref_shadow = (uint32_t*)TEMPORAL_SECONDARY_STATIC_SHADOW(addr);
  DLOG(" | > Blk ID: %u\n", i, *origin_shadow);
  for (i = 0; i < size; i+=PTR_SZ)
    DLOG(" | >   Ref ID[%u]: %u\n", i/8, *(ref_shadow + 1));
#endif /*}}} E_ACSL_TEMPORAL*/
}

/*! \brief Print human-readable representation of a heap shadow region for a
 * memory block of length `size` starting at address `addr`. This function
 * assumes that `addr` is the base address of the memory block. */
static void print_heap_shadows(uintptr_t addr) {
  unsigned char *block_shadow = (unsigned char*)HEAP_SHADOW(addr);
  unsigned char *init_shadow =  (unsigned char*)HEAP_INIT_SHADOW(addr);

  size_t length = (size_t)((uintptr_t*)(block_shadow))[1];
  size_t alloc_size = ALLOC_SIZE(length);
  size_t segments = alloc_size/HEAP_SEGMENT;
  uintptr_t *segment = (uintptr_t*)(block_shadow);

  DLOG(" | === Block Shadow ======================================\n");
  DLOG(" | Access addr:    %a\n",  addr);
  DLOG(" | Block Shadow:   %a\n",	 block_shadow);
  DLOG(" | Init	 Shadow:   %a\n",	 init_shadow);
  DLOG(" | Segments:       %lu\n", segments);
  DLOG(" | Actual size:    %lu bytes\n", alloc_size);
  DLOG(" | Tracked Length: %lu bytes\n", length);

  if (zeroed_out(block_shadow, alloc_size))
    DLOG(" | << Nullified >>  \n");

#ifdef E_ACSL_TEMPORAL /*{{{*/
  DLOG(" | Origin TS:       %u\n", (uint32_t)segment[2]);
#endif	/*}}}*/

  size_t i;
  for (i = 0; i < segments; i++) {
    segment = (uintptr_t*)(block_shadow + i*HEAP_SEGMENT);
    DLOG(" |   Segment: %lu, Base: %a \n", i, *segment);
  }

  DLOG(" | Initialization: \n |   ");
  for (i = 0; i < alloc_size/8; i++) {
    if (i > 0 && (i*8)%HEAP_SEGMENT == 0)
      DLOG("\n |   ");
    DLOG("%8b ", init_shadow[i], init_shadow[i]);
  }
  DLOG("\n");
}

static void print_shadows(uintptr_t addr, size_t size) {
  if (IS_ON_STATIC(addr))
    print_static_shadows(addr, size);
  else if (IS_ON_HEAP(addr))
    print_heap_shadows(addr);
}

static void print_memory_segment(struct memory_segment *p, char *lab, int off) {
  DLOG("   %s: %lu MB [%lu, %lu]", lab, MB_SZ(p->size), p->start, p->end);
  if (off)
    DLOG("{ Offset: %ld }", p->shadow_offset);
  DLOG("\n");
}

static void print_memory_partition(struct memory_partition *p) {
  print_memory_segment(&p->application, "Application", 0);
  print_memory_segment(&p->primary, "Primary    ", 1);
  print_memory_segment(&p->secondary, "Secondary  ", 1);
#ifdef E_ACSL_TEMPORAL
  print_memory_segment(&p->temporal_primary, "Temporal Primary    ", 1);
  print_memory_segment(&p->temporal_secondary, "Temporal Secondary  ", 1);
#endif
}

static void print_shadow_layout() {
  DLOG(">>> HEAP ---------------------\n");
  print_memory_partition(&mem_layout.heap);
  DLOG(">>> STACK --------------------\n");
  print_memory_partition(&mem_layout.stack);
  DLOG(">>> GLOBAL -------------------\n");
  print_memory_partition(&mem_layout.global);
  DLOG(">>> TLS ----------------------\n");
  print_memory_partition(&mem_layout.tls);
  DLOG(">>> --------------------------\n");
}

/*! \brief Output the shadow segment the address belongs to */
static const char* which_segment(uintptr_t addr) {
  const char *loc = NULL;
  if (IS_ON_STACK(addr))
    loc = "stack";
  else if (IS_ON_HEAP(addr))
    loc = "heap";
  else if (IS_ON_GLOBAL(addr))
    loc = "global";
  else if (IS_ON_TLS(addr))
    loc = "TLS";
  else
    loc = "untracked";
  return loc;
}

/* NOTE: Above functions are designed to be used only through the following
 * macros or debug functions included/defined based on the value of the
 * E_ACSL_DEBUG macro. */

/*! \brief Print program layout. This function outputs start/end addresses of
 * various program segments, their shadow counterparts and sizes of shadow
 * regions used. */
#define DEBUG_PRINT_LAYOUT print_shadow_layout()
void ___e_acsl_debug_print_layout() { DEBUG_PRINT_LAYOUT; }

/*! \brief Print the shadow segment address addr belongs to */
#define DEBUG_PRINT_SEGMENT(_addr) which_segment(_addr)
void ___e_acsl_debug_print_segment(uintptr_t addr) { DEBUG_PRINT_SEGMENT(addr); }

/*! \brief Print human-readable representation of a shadow region corresponding
 * to a memory address addr. The second argument (size) if the size of the
 * shadow region to be printed. Normally addr argument is a base address of a
 * memory block and size is its length. */
#define DEBUG_PRINT_SHADOW(addr, size) \
  print_shadows((uintptr_t)addr, (size_t)size)
void ___e_acsl_debug_print_shadow(uintptr_t addr, size_t size) {
  DEBUG_PRINT_SHADOW(addr, size);
}

#else
/* \cond exclude from doxygen */
#define DEBUG_PRINT_SHADOW(addr, size)
#define DEBUG_PRINT_LAYOUT
#define DEBUG_PRINT_SEGMENT(addr)
/* \endcond */
#endif
/* }}} */
