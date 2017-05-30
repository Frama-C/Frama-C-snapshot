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
 * on Patricia Trie. See e_acsl_mmodel_api.h for details.
***************************************************************************/

#include "e_acsl_bittree.h"

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
 * size in bytes requred to represent its partial initialization */
#define needed_bytes(_size) \
  ((_size % 8) == 0 ? (_size/8) : (_size/8 + 1))
/* }}} */

/**************************/
/* HEAP USAGE         {{{ */
/**************************/
static size_t heap_allocation_size = 0;

static size_t get_heap_allocation_size(void) {
  return heap_allocation_size;
}
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
static void initialize (void * ptr, size_t size) {
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
    tmp->init_ptr = native_malloc(nb);
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
    native_free(tmp->init_ptr);
    tmp->init_ptr = NULL;
  }
}

/* mark all bytes of ptr as initialized */
static void full_init (void * ptr) {
  bt_block * tmp;
  if (ptr == NULL)
    return;

  tmp = bt_lookup(ptr);
  if (tmp == NULL)
    return;

  if (tmp->init_ptr != NULL) {
    native_free(tmp->init_ptr);
    tmp->init_ptr = NULL;
  }
  tmp->init_bytes = tmp->size;
}

/* mark a block as read-only */
static void mark_readonly(void * ptr) {
  bt_block * tmp;
  if (ptr == NULL)
    return;
  tmp = bt_lookup(ptr);
  if (tmp == NULL)
    return;
  tmp->is_readonly = true;
}
/* }}} */

/**************************/
/* PREDICATES        {{{  */
/**************************/

static int freeable(void* ptr) {
  bt_block * tmp;
  if(ptr == NULL)
    return false;
  tmp = bt_lookup(ptr);
  if(tmp == NULL)
    return false;
  return tmp->freeable;
}

/* return whether the size bytes of ptr are initialized */
static int initialized(void * ptr, size_t size) {
  unsigned i;
  bt_block * tmp = bt_find(ptr);
  if(tmp == NULL)
    return false;

  /* fully uninitialized */
  if(tmp->init_bytes == 0)
    return false;
  /* fully initialized */
  if(tmp->init_bytes == tmp->size)
    return true;

  /* see implementation of function `initialize` for details */
  for(i = 0; i < size; i++) {
    size_t offset = (uintptr_t)ptr - tmp->ptr + i;
    int byte = offset/8;
    int bit = offset%8;
    if (!checkbit(bit, tmp->init_ptr[byte]))
      return false;
  }
  return true;
}

/* return the length (in bytes) of the block containing ptr */
static size_t block_length(void* ptr) {
  bt_block * tmp = bt_find(ptr);
  /* Hard failure when un-allocated memory is used */
  vassert(tmp != NULL, "\\block_length of unallocated memory", NULL);
  return tmp->size;
}

static int allocated(void* ptr, size_t size, void *ptr_base) {
  bt_block * blk = bt_find(ptr);
  bt_block * blk_base = bt_find(ptr_base);
  if (blk == NULL || blk_base == NULL || blk->ptr != blk_base->ptr)
    return false;
  return (blk->size - ((size_t)ptr - blk->ptr) >= size);
}

/* return whether the size bytes of ptr are readable/writable */
static int valid(void* ptr, size_t size, void *ptr_base, void *addr_of_base) {
  /* Many similarities with allocated (so far at least), but it is better
   * to use this tandalone definition, otherwise the block needs to be looked
   * up twice */
  bt_block * blk = bt_find(ptr);
  bt_block * blk_base = bt_find(ptr_base);
  if (blk == NULL || blk_base == NULL || blk->ptr != blk_base->ptr)
    return false;
  return (blk->size - ((size_t)ptr - blk->ptr) >= size && !blk->is_readonly);
}

/* return whether the size bytes of ptr are readable */
static int valid_read(void* ptr, size_t size, void *ptr_base, void *addr_of_base) {
  return allocated(ptr, size, ptr_base);
}

/* return the base address of the block containing ptr */
static void* base_addr(void* ptr) {
  bt_block * tmp = bt_find(ptr);
  vassert(tmp != NULL, "\\base_addr of unallocated memory", NULL);
  return (void*)tmp->ptr;
}

/* return the offset of `ptr` within its block */
static int offset(void* ptr) {
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
static void* store_block(void* ptr, size_t size) {
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
    tmp = native_malloc(sizeof(bt_block));
    tmp->ptr = (uintptr_t)ptr;
    tmp->size = size;
    tmp->init_ptr = NULL;
    tmp->init_bytes = 0;
    tmp->is_readonly = false;
    tmp->freeable = false;
    bt_insert(tmp);
#ifdef E_ACSL_DEBUG
    tmp->line = 0;
    tmp->file = "undefined";
#endif
  }
  return tmp;
}

/* remove the block starting at ptr */
static void delete_block(void* ptr) {
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
      bt_remove(tmp);
      native_free(tmp);
    }
  }
}

static void* store_block_duplicate(void* ptr, size_t size) {
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
static void* bittree_malloc(size_t size) {
  if(size == 0)
    return NULL;

  void *res = native_malloc(size);
  if (res) {
    bt_block * new_block = store_block(res, size);
    heap_allocation_size += size;
    new_block->freeable = true;
  }
  return res;
}

/*! \brief Replacement for `calloc` with memory tracking */
static void* bittree_calloc(size_t nbr_block, size_t size_block) {
  /* FIXME: Need an integer overflow check here */
  size_t size = nbr_block * size_block;
  if (size == 0)
    return NULL;

  void *res = native_calloc(nbr_block, size_block);
  if (res) {
    bt_block * new_block = store_block(res, size);
    heap_allocation_size += size;
    new_block->freeable = 1;
    /* Mark allocated block as freeable and initialized */
    new_block->init_bytes = size;
  }
  return res;
}

/*! \brief Replacement for `aligned_alloc` with memory tracking */
static void *bittree_aligned_alloc(size_t alignment, size_t size) {
  /* Check if:
   *  - size and alignment are greater than zero
   *  - alignment is a power of 2
   *  - size is a multiple of alignment */
  if (!size || !alignment || !powof2(alignment) || (size%alignment))
    return NULL;

  void *res = native_aligned_alloc(alignment, size);
  if (res) {
    bt_block * new_block = store_block(res, size);
    new_block->freeable = 1;
    heap_allocation_size += size;
  }
  return res;
}

/*! \brief Replacement for `posix_memalign` with memory tracking */
static int bittree_posix_memalign(void **memptr, size_t alignment, size_t size) {
 /* Check if:
   *  - size and alignment are greater than zero
   *  - alignment is a power of 2 and a multiple of sizeof(void*) */
  if (!size || !alignment || !powof2(alignment) || alignment%sizeof(void*))
    return -1;

  /* Make sure that the first argument to posix memalign is indeed allocated */
  vassert(allocated((void*)memptr, sizeof(void*), (void*)memptr),
      "\\invalid memptr in posix_memalign", NULL);

  int res = native_posix_memalign(memptr, alignment, size);
  if (!res) {
    bt_block * new_block = store_block(*memptr, size);
    new_block->freeable = 1;
    heap_allocation_size += size;
  }
  return res;
}

/*! \brief Replacement for `realloc` with memory tracking */
static void* bittree_realloc(void* ptr, size_t size) {
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
  new_ptr = native_realloc((void*)tmp->ptr, size);
  if(new_ptr == NULL)
    return NULL;
  heap_allocation_size -= tmp->size;
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
    /* realloc bigger larger block */
    } else {
      /* size of tmp->init_ptr in the new block */
      int nb = needed_bytes(size);
      /* number of bits that need to be set in tmp->init_ptr */
      int nb_old = needed_bytes(tmp->size);
      /* allocate memory to store partial initialization */
      tmp->init_ptr = native_calloc(1, nb);
      /* carry out initialization of the old block */
      setbits(tmp->size, tmp->init_ptr);
    }
  } else { /* contains initialized and uninitialized parts */
    int nb = needed_bytes(size);
    int nb_old = needed_bytes(tmp->size);
    int i;
    tmp->init_ptr = native_realloc(tmp->init_ptr, nb);
    for (i = nb_old; i < nb; i++)
      tmp->init_ptr[i] = 0;
    tmp->init_bytes = 0;
    for (i = 0; i < nb; i++)
      tmp->init_bytes += nbr_bits_to_1[tmp->init_ptr[i]];
    if (tmp->init_bytes == size || tmp->init_bytes == 0) {
      native_free(tmp->init_ptr);
      tmp->init_ptr = NULL;
    }
  }
  tmp->size = size;
  tmp->freeable = true;
  heap_allocation_size += size;
  return (void*)tmp->ptr;
}

/*! \brief Replacement for `free` with memory tracking */
static void bittree_free(void* ptr) {
  if (!ptr)
    return;
  bt_block * res = bt_lookup(ptr);
  if (!res) {
    vabort("Not a start of block (%a) in free\n", ptr);
  } else {
    heap_allocation_size -= res->size;
    native_free(ptr);
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
static void memory_clean() {
  bt_clean();
}

/* add `argv` to the memory model */
static void init_argv(int argc, char **argv) {
  int i;

  store_block(argv, (argc+1)*sizeof(char*));
  full_init(argv);

  for (i = 0; i < argc; i++) {
    store_block(argv[i], strlen(argv[i])+1);
    full_init(argv[i]);
  }
}

static void memory_init(int *argc_ref, char ***argv_ref, size_t ptr_size) {
  identify_run();
  arch_assert(ptr_size);
  /* Tracking program arguments */
  if (argc_ref)
    init_argv(*argc_ref, *argv_ref);
  /* Tracking safe locations */
  collect_safe_locations();
  int i;
  for (i = 0; i < safe_location_counter; i++) {
    void *addr = (void*)safe_locations[i].address;
    uintptr_t len = safe_locations[i].length;
    store_block(addr, len);
    if (safe_locations[i].initialized)
      initialize(addr, len);
  }
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
    printf(" << %a >> %a [%lu] => %lu \n",
      p, base_addr(p), offset(p), block_length(p));
  } else {
    printf(" << %a >> not allocated\n", p);
  }
}
#endif

/* API BINDINGS {{{ */
/* Heap allocation (native) */
strong_alias(bittree_malloc,	malloc)
strong_alias(bittree_calloc, calloc)
strong_alias(bittree_realloc, realloc)
strong_alias(bittree_free, free)
strong_alias(bittree_free, cfree)
strong_alias(bittree_posix_memalign, posix_memalign)
strong_alias(bittree_aligned_alloc, aligned_alloc)
/* Explicit tracking */
public_alias(delete_block)
public_alias(store_block)
public_alias(store_block_duplicate)
/* Predicates */
public_alias(offset)
public_alias(base_addr)
public_alias(valid_read)
public_alias(valid)
public_alias(block_length)
public_alias(initialized)
public_alias(freeable)
public_alias(mark_readonly)
/* Block initialization */
public_alias(initialize)
public_alias(full_init)
/* Memory state initialization */
public_alias(memory_clean)
public_alias(memory_init)
/* Heap size */
public_alias(get_heap_allocation_size)
public_alias(heap_allocation_size)
#ifdef E_ACSL_DEBUG /* Debug */
public_alias(bt_print_block)
public_alias(bt_print_tree)
public_alias(block_info)
public_alias(store_block_debug)
public_alias(delete_block_debug)
#endif
/* }}} */
