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
 * \file e_acsl_bittree_mmodel.c
 * \brief Implementation of E-ACSL public API using a memory model based
 * on Patricia Trie. See e_acsl.h for details.
***************************************************************************/

# include "e_acsl_bittree_api.h"
# include "e_acsl_bittree.h"

/* Public API {{{ */
/* Debug */
#ifdef E_ACSL_DEBUG
# define bt_print_block     export_alias(bt_print_block)
# define bt_print_tree      export_alias(bt_print_tree)
# define block_info         export_alias(block_info)
# define store_block_debug  export_alias(store_block_debug)
# define delete_block_debug export_alias(delete_block_debug)
#endif
/* }}} */

#define E_ACSL_MMODEL_DESC "patricia trie"

/* Assertions in debug mode */
#define ALLOCATED(_ptr,_size, _base) \
  ((allocated(_ptr, _size, _base) == NULL) ? 0 : 1)

#ifdef E_ACSL_DEBUG
#define DVALIDATE_ALLOCATED(_ptr, _size, _base) \
  vassert(ALLOCATED(_ptr, _size, _base), \
  "Not allocated [%a, %a + %lu]", (uintptr_t)_ptr, _size)

#define DVALIDATE_WRITEABLE(_ptr, _size, _base) { \
  DVALIDATE_ALLOCATED(_ptr, _size, _base); \
  vassert(!readonly(_ptr), "Location %a is read-only", (uintptr_t)_ptr); \
}
#else
#define DVALIDATE_ALLOCATED(_ptr, _size, _base)
#define DVALIDATE_WRITEABLE(_ptr, _size, _base)
#endif

/**************************/
/* SUPPORT            {{{ */
/**************************/
static const int nbr_bits_to_1[256] = {
  0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,2,2,3,2,3,
  3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,1,2,2,3,2,3,3,4,2,3,3,4,
  3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,
  4,5,4,5,5,6,4,5,5,6,5,6,6,7,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,
  3,4,4,5,4,5,5,6,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,
  6,7,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,3,4,4,5,
  4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8
};

/* given the size of the memory block (_size) return (or rather evaluate to)
 * size in bytes required to represent its partial initialization */
#define needed_bytes(_size) \
  ((_size % 8) == 0 ? (_size/8) : (_size/8 + 1))
/* }}} */

/**************************/
/* DEBUG              {{{ */
/**************************/
#ifdef E_ACSL_DEBUG
/* Notion of current location for debugging purposes  */
static struct current_location {
  int line;
  char *file;
} cloc = { 0, "undefined" };

#define update_cloc(_file, _line) { cloc.line = _line; cloc.file = _file; }
#endif
/* }}} */

/**************************/
/* INITIALIZATION     {{{ */
/**************************/

/* mark the size bytes of ptr as initialized */
void initialize (void * ptr, size_t size) {
  bt_block * tmp;
  if(!ptr)
    return;

  tmp = bt_find(ptr);
  if(tmp == NULL)
    return;

  /* already fully initialized, do nothing */
  if(tmp->init_bytes == tmp->size)
    return;

  /* fully uninitialized */
  if(tmp->init_bytes == 0) {
    int nb = needed_bytes(tmp->size);
    tmp->init_ptr = private_malloc(nb);
    memset(tmp->init_ptr, 0, nb);
  }

  /* partial initialization is kept via a character array accessible via the
   * tmp->init_ptr. This is such that a N-th bit of tmp->init_ptr tracks
   * initialization of the N-th byte of the memory block tracked by tmp.
   *
   * The following sets individual bits in tmp->init_ptr that track
   * initialization of `size' bytes starting from `ptr'. */
  unsigned i;
  for(i = 0; i < size; i++) {
    /* byte-offset within the block, i.e., mark `offset' byte as initialized */
    size_t offset = (uintptr_t)ptr - tmp->ptr + i;
    /* byte offset within tmp->init_ptr, i.e., a byte containing the bit to
       be toggled */
    int byte = offset/8;
    /* bit-offset within the above byte, i.e., bit to be toggled */
    int bit = offset%8;

    if (!checkbit(bit, tmp->init_ptr[byte])) { /* if bit is unset ... */
      setbit(bit, tmp->init_ptr[byte]); /* ... set the bit ... */
      tmp->init_bytes++; /* ... and increment initialized bytes count */
    }
  }

  /* now fully initialized */
  if(tmp->init_bytes == tmp->size) {
    private_free(tmp->init_ptr);
    tmp->init_ptr = NULL;
  }
}

/* mark all bytes of ptr as initialized */
void full_init (void * ptr) {
  bt_block * tmp;
  if (ptr == NULL)
    return;

  tmp = bt_lookup(ptr);
  if (tmp == NULL)
    return;

  if (tmp->init_ptr != NULL) {
    private_free(tmp->init_ptr);
    tmp->init_ptr = NULL;
  }
  tmp->init_bytes = tmp->size;
}

/* mark a block as read-only */
void mark_readonly(void * ptr) {
  bt_block * tmp;
  if (ptr == NULL)
    return;
  tmp = bt_lookup(ptr);
  if (tmp == NULL)
    return;
  tmp->is_readonly = 1;
}
/* }}} */

/**************************/
/* PREDICATES        {{{  */
/**************************/

int freeable(void* ptr) {
  bt_block * tmp;
  if(ptr == NULL)
    return 0;
  tmp = bt_lookup(ptr);
  if(tmp == NULL)
    return 0;
  return tmp->is_freeable;
}

/* return whether the size bytes of ptr are initialized */
int initialized(void * ptr, size_t size) {
  unsigned i;
  bt_block * tmp = bt_find(ptr);
  if(tmp == NULL)
    return 0;

  /* fully uninitialized */
  if(tmp->init_bytes == 0)
    return 0;
  /* fully initialized */
  if(tmp->init_bytes == tmp->size)
    return 1;

  /* see implementation of function `initialize` for details */
  for(i = 0; i < size; i++) {
    size_t offset = (uintptr_t)ptr - tmp->ptr + i;
    int byte = offset/8;
    int bit = offset%8;
    if (!checkbit(bit, tmp->init_ptr[byte]))
      return 0;
  }
  return 1;
}

/* return the length (in bytes) of the block containing ptr */
size_t block_length(void* ptr) {
  bt_block * blk = bt_find(ptr);
  /* Hard failure when un-allocated memory is used */
  vassert(blk != NULL, "\\block_length of unallocated memory", NULL);
  return blk->size;
}

static bt_block* allocated(void* ptr, size_t size, void *ptr_base) {
  bt_block * blk = bt_find(ptr);
  if (blk == NULL)
    return NULL;
#ifndef E_ACSL_WEAK_VALIDITY
  bt_block * blk_base = bt_find(ptr_base);
  if (blk_base == NULL || blk->ptr != blk_base->ptr)
    return NULL;
#endif
  return (blk->size - ((size_t)ptr - blk->ptr) >= size) ? blk : NULL;
}

/** \brief Return 1 if a given memory location is read-only and 0 otherwise */
static int readonly (void *ptr) {
  bt_block * blk = bt_find(ptr);
  vassert(blk != NULL, "Readonly on unallocated memory", NULL);
  return blk->is_readonly;
}

/* return whether the size bytes of ptr are readable/writable */
int valid(void* ptr, size_t size, void *ptr_base, void *addrof_base) {
  bt_block * blk = allocated(ptr, size, ptr_base);
  return blk != NULL && !blk->is_readonly
#ifdef E_ACSL_TEMPORAL
    && temporal_valid(ptr_base, addrof_base)
#endif
  ;
}

/* return whether the size bytes of ptr are readable */
int valid_read(void* ptr, size_t size, void *ptr_base, void *addrof_base) {
  bt_block * blk = allocated(ptr, size, ptr_base);
  return blk != NULL
#ifdef E_ACSL_TEMPORAL
    && temporal_valid(ptr_base, addrof_base)
#endif
  ;
}

/* return the base address of the block containing ptr */
void* base_addr(void* ptr) {
  bt_block * tmp = bt_find(ptr);
  vassert(tmp != NULL, "\\base_addr of unallocated memory", NULL);
  return (void*)tmp->ptr;
}

/* return the offset of `ptr` within its block */
int offset(void* ptr) {
  bt_block * tmp = bt_find(ptr);
  vassert(tmp != NULL, "\\offset of unallocated memory", NULL);
  return ((uintptr_t)ptr - tmp->ptr);
}
/* }}} */

/**************************/
/* ALLOCATION         {{{ */
/**************************/

/* STACK ALLOCATION {{{ */
/* store the block of size bytes starting at ptr, the new block is returned.
 * Warning: the return type is implicitly (bt_block*). */
void* store_block(void* ptr, size_t size) {
#ifdef E_ACSL_DEBUG
  if (ptr == NULL)
    vabort("Attempt to record NULL block");
  else {
    char *check = (char*)ptr;
    bt_block * exitsing_block = bt_find(ptr);
    if (exitsing_block) {
      vabort("\nRecording %a [%lu] at %s:%d failed."
        " Overlapping block %a [%lu] found at %s:%d\n",
        ptr, size, cloc.file, cloc.line, base_addr(check),
        block_length(check), exitsing_block->file, exitsing_block->line);
    }
    check += size - 1;
    exitsing_block = bt_find(check);
    if (exitsing_block) {
      vabort("\nRecording %a [%lu] at %d failed."
        " Overlapping block %a [%lu] found at %s:%d\n",
        ptr, size, cloc.file, cloc.line, base_addr(check),
        block_length(check), exitsing_block->file, exitsing_block->line);
    }
  }
#endif
  bt_block * tmp = NULL;
  if (ptr) {
    tmp = private_malloc(sizeof(bt_block));
    tmp->ptr = (uintptr_t)ptr;
    tmp->size = size;
    tmp->init_ptr = NULL;
    tmp->init_bytes = 0;
    tmp->is_readonly = 0;
    tmp->is_freeable = 0;
    bt_insert(tmp);
#ifdef E_ACSL_DEBUG
    tmp->line = 0;
    tmp->file = "undefined";
#endif
#ifdef E_ACSL_TEMPORAL
    tmp->timestamp = NEW_TEMPORAL_TIMESTAMP();
    tmp->temporal_shadow = (size >= sizeof(void*)) ?
      private_malloc(size) : NULL;
#endif
  }
  return tmp;
}

/* Track a heap block. This is a wrapper for all memory allocation functions
  that create new bittree nodes. It applies to all memory allocating functions
  but realloc that modifies nodes rather than create them */
static void* store_freeable_block(void* ptr, size_t size, int init_bytes) {
  bt_block *blk = NULL;
  if (ptr) {
    blk = store_block(ptr, size);
    blk->is_freeable = 1;
    update_heap_allocation(size);
    if (init_bytes)
      blk->init_bytes = size;
  }
  return blk;
}

/* remove the block starting at ptr */
void delete_block(void* ptr) {
#ifdef E_ACSL_DEBUG
  /* Make sure the recorded block is not NULL */
  if (!ptr)
    vabort("Attempt to delete NULL block");
#endif
  if (ptr != NULL) {
    bt_block * tmp = bt_lookup(ptr);
#ifdef E_ACSL_DEBUG
    /* Make sure the removed block exists in the tracked allocation */
    if (!tmp)
      vabort("Attempt to delete untracked block");
#endif
    if (tmp) {
      bt_clean_block_init(tmp);
#ifdef E_ACSL_TEMPORAL
      private_free(tmp->temporal_shadow);
#endif
      bt_remove(tmp);
      private_free(tmp);
    }
  }
}

void* store_block_duplicate(void* ptr, size_t size) {
  bt_block * tmp = NULL;
  if (ptr != NULL) {
    bt_block * tmp = bt_lookup(ptr);
    if (tmp) {
#ifdef E_ACSL_DEBUG
    /* Make sure that duplicate block, if so is of the same length */
    if (tmp->size != size)
      vabort("Attempt to store duplicate block of different length");
#endif
      delete_block(ptr);
    }
    store_block(ptr, size);
  }
  return tmp;
}

/* }}} */

/* HEAP ALLOCATION {{{ */
/*! \brief Replacement for `malloc` with memory tracking */
void* malloc(size_t size) {
  if (size == 0)
    return NULL;

  void *res = public_malloc(size);
  store_freeable_block(res, size, 0);
  return res;
}

/*! \brief Replacement for `calloc` with memory tracking */
void* calloc(size_t nbr_block, size_t size_block) {
  /* FIXME: Need an integer overflow check here */
  size_t size = nbr_block * size_block;
  if (size == 0)
    return NULL;

  void *res = public_calloc(nbr_block, size_block);
  store_freeable_block(res, size, 1);
  return res;
}

/*! \brief Replacement for `aligned_alloc` with memory tracking */
void *aligned_alloc(size_t alignment, size_t size) {
  /* Check if:
   *  - size and alignment are greater than zero
   *  - alignment is a power of 2
   *  - size is a multiple of alignment */
  if (!size || !alignment || !powof2(alignment) || (size%alignment))
    return NULL;

  void *res = public_aligned_alloc(alignment, size);
  store_freeable_block(res, size, 0);
  return res;
}

/*! \brief Replacement for `posix_memalign` with memory tracking */
int posix_memalign(void **memptr, size_t alignment, size_t size) {
 /* Check if:
   *  - size and alignment are greater than zero
   *  - alignment is a power of 2 and a multiple of sizeof(void*) */
  if (!size || !alignment || !powof2(alignment) || alignment%sizeof(void*))
    return -1;

  /* Make sure that the first argument to posix memalign is indeed allocated */
  DVALIDATE_WRITEABLE((void*)memptr, sizeof(void*), (void*)memptr);

  int res = public_posix_memalign(memptr, alignment, size);
  if (!res)
    store_freeable_block(*memptr, size, 0);
  return res;
}

/*! \brief Replacement for `realloc` with memory tracking */
void* realloc(void* ptr, size_t size) {
  bt_block * tmp;
  void * new_ptr;
  /* ptr is NULL - malloc */
  if(ptr == NULL)
    return malloc(size);
  /* size is zero - free */
  if(size == 0) {
    free(ptr);
    return NULL;
  }
  tmp = bt_lookup(ptr);
  DASSERT(tmp != NULL);
  new_ptr = public_realloc((void*)tmp->ptr, size);
  if (new_ptr == NULL)
    return NULL;

  /* update the heap allocation size to `size - tmp->size` while keeping
     constant the number of allocated blocks */
  update_heap_allocation(size);
  update_heap_allocation(-tmp->size);
  /* realloc changes start address -- re-enter the element */
  if (tmp->ptr != (uintptr_t)new_ptr) {
    bt_remove(tmp);
    tmp->ptr = (uintptr_t)new_ptr;
    bt_insert(tmp);
  }
  /* uninitialized, do nothing */
  if(tmp->init_bytes == 0) ;
  /* already fully initialized block */
  else if (tmp->init_bytes == tmp->size) {
    /* realloc smaller block */
    if (size <= tmp->size) {
      /* adjust new size, allocation not necessary */
      tmp->init_bytes = size;
    /* realloc larger block */
    } else {
      /* size of tmp->init_ptr in the new block */
      int nb = needed_bytes(size);
      /* number of bits that need to be set in tmp->init_ptr */
      int nb_old = needed_bytes(tmp->size);
      /* allocate memory to store partial initialization */
      tmp->init_ptr = private_calloc(1, nb);
      /* carry out initialization of the old block */
      setbits(tmp->size, tmp->init_ptr);
    }
  } else { /* contains initialized and uninitialized parts */
    int nb = needed_bytes(size);
    int nb_old = needed_bytes(tmp->size);
    int i;
    /* increase container with init data */
    tmp->init_ptr = private_realloc(tmp->init_ptr, nb);
    for (i = nb_old; i < nb; i++)
      tmp->init_ptr[i] = 0;
    tmp->init_bytes = 0;
    for (i = 0; i < nb; i++)
      tmp->init_bytes += nbr_bits_to_1[tmp->init_ptr[i]];
    if (tmp->init_bytes == size || tmp->init_bytes == 0) {
      private_free(tmp->init_ptr);
      tmp->init_ptr = NULL;
    }
  }
  tmp->size = size;
  tmp->is_freeable = 1;
  return (void*)tmp->ptr;
}

/*! \brief Replacement for `free` with memory tracking */
void free(void* ptr) {
  if (ptr == NULL) {
/* Fail if instructed to treat NULL input to free as invalid. */
#ifdef E_ACSL_FREE_VALID_ADDRESS
    vabort("NULL pointer in free\n");
#endif
    return;
  }
  bt_block * res = bt_lookup(ptr);
  if (!res) {
    vabort("Not a start of block (%a) in free\n", ptr);
  } else {
    update_heap_allocation(-res->size);
    public_free(ptr);
    bt_clean_block_init(res);
    bt_remove(res);
  }
}
/* }}} */
/* }}} */

/******************************/
/* PROGRAM INITIALIZATION {{{ */
/******************************/

/* erase the content of the abstract structure */
void memory_clean() {
  bt_clean();
  report_heap_leaks();
}

/* POSIX-compliant array of character pointers to the environment strings. */
extern char **environ;

/* add `argv` to the memory model */
static void argv_alloca(int *argc_ref,  char *** argv_ref) {
  /* Track a top-level containers */
  store_block((void*)argc_ref, sizeof(int));
  store_block((void*)argv_ref, sizeof(char**));
  int argc = *argc_ref;
  char** argv = *argv_ref;
  /* Track argv */

  size_t argvlen = (argc + 1)*sizeof(char*);
  store_block(argv, argvlen);
  initialize(argv, (argc + 1)*sizeof(char*));

  while (*argv) {
    size_t arglen = strlen(*argv) + 1;
    store_block(*argv, arglen);
    initialize(*argv, arglen);
    argv++;
  }

#ifdef E_ACSL_TEMPORAL /* Fill temporal shadow */
  int i;
  argv = *argv_ref;
  temporal_store_nblock(argv_ref, *argv_ref);
  for (i = 0; i < argc; i++)
    temporal_store_nblock(argv + i, *(argv+i));
#endif

  while (*environ) {
    size_t envlen = strlen(*environ) + 1;
    store_block(*environ, envlen);
    initialize(*environ, envlen);
    environ++;
  }
}

void memory_init(int *argc_ref, char ***argv_ref, size_t ptr_size) {
  describe_run();
  /* Mspace sizes here are not that relevant as there is no shadowing and
     mspaces will grow automatically */
  make_memory_spaces(MB_SZ(64), MB_SZ(64));
  arch_assert(ptr_size);
  initialize_report_file(argc_ref, argv_ref);
  /* Tracking program arguments */
  if (argc_ref)
    argv_alloca(argc_ref, argv_ref);
  /* Tracking safe locations */
  collect_safe_locations();
  int i;
  for (i = 0; i < safe_location_counter; i++) {
    void *addr = (void*)safe_locations[i].address;
    uintptr_t len = safe_locations[i].length;
    store_block(addr, len);
    if (safe_locations[i].is_initialized)
      initialize(addr, len);
  }
  init_infinity_values();
}
/* }}} */

#ifdef E_ACSL_DEBUG

/* Debug version of store block with location tracking. This function is aimed
 * at manual debugging.  While there is no easy way of traking file/line numbers
 * recorded memory blocks with the use of the following macros placed after the
 * declaration of __e_acsl_store_block:
 *
 * #define __e_acsl_store_block(...) \
 *    __e_acsl_store_block_debug(__FILE__, __LINE__, __VA_ARGS__)
 *
 * The above macros with rewrite of instances of __e_acsl_store_block generating
 * origin information of tracked memory blocks.
*/
void* store_block_debug(char *file, int line, void* ptr, size_t size) {
  update_cloc(file, line);
  bt_block * res = store_block(ptr, size);
  if (res) {
    res->line = line;
    res->file = file;
  }
  return res;
}

void delete_block_debug(char *file, int line, void* ptr) {
  update_cloc(file, line);
  bt_block * tmp = bt_lookup(ptr);
  if (!tmp) {
    vabort("Block with base address %a not found in the memory model at %s:%d",
        ptr, file, line);
  }
  delete_block(ptr);
}

/* Debug print of block information */
void block_info(char *p) {
  bt_block * res = bt_find(p);
  if (res) {
    DLOG(" << %a >> %a [%lu] => %lu \n",
      p, base_addr(p), offset(p), block_length(p));
  } else {
    DLOG(" << %a >> not allocated\n", p);
  }
}
#endif

/* Local operations on temporal timestamps {{{ */
/* Remaining functionality (shared between all models) is located in e_acsl_temporal.h */
#ifdef E_ACSL_TEMPORAL
static uint32_t origin_timestamp(void *ptr) {
  bt_block * blk = bt_find(ptr);
  return blk != NULL ? blk->timestamp : INVALID_TEMPORAL_TIMESTAMP;
}

static uintptr_t temporal_referent_shadow(void *ptr) {
  bt_block * blk = bt_find(ptr);
  vassert(blk != NULL,
    "referent timestamp on unallocated memory address %a", (uintptr_t)ptr);
  vassert(blk->temporal_shadow != NULL,
    "no temporal shadow of block with base address", (uintptr_t)blk->ptr);
  return (uintptr_t)blk->temporal_shadow + offset(ptr);
}

static uint32_t referent_timestamp(void *ptr) {
  bt_block * blk = bt_find(ptr);
  if (blk != NULL)
    return *((uint32_t*)temporal_referent_shadow(ptr));
  else
    return INVALID_TEMPORAL_TIMESTAMP;
}

static void store_temporal_referent(void *ptr, uint32_t ref) {
  uint32_t *shadow = (uint32_t*)temporal_referent_shadow(ptr);
  *shadow = ref;
}
#endif
/* }}} */
