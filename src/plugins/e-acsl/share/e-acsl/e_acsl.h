/**************************************************************************/
/*                                                                        */
/*  This file is part of the Frama-C's E-ACSL plug-in.                    */
/*                                                                        */
/*  Copyright (C) 2012-2019                                               */
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
 * \file   e_acsl.h
 * \brief  Public C API of E-ACSL Runtime Library
 *
 * Functions and variables with non-static linkage used for instrumentation.
***************************************************************************/

#ifndef E_ACSL_MMODEL_H
#define E_ACSL_MMODEL_H

#include <stddef.h>
#include <stdio.h>
#include "e_acsl_alias.h"

/************************************************************************/
/*** API Prefixes {{{ ***/
/************************************************************************/

/* Assert */
#define sound_verdict         export_alias(sound_verdict)
#define assert                export_alias(assert)

/* Tracking */
#define delete_block          export_alias(delete_block)
#define store_block           export_alias(store_block)
#define store_block_duplicate export_alias(store_block_duplicate)

/* Predicates */
#define offset                export_alias(offset)
#define base_addr             export_alias(base_addr)
#define block_length          export_alias(block_length)
#define valid_read            export_alias(valid_read)
#define valid                 export_alias(valid)
#define initialized           export_alias(initialized)
#define freeable              export_alias(freeable)

/* Block initialization  */
#define mark_readonly         export_alias(mark_readonly)
#define initialize            export_alias(initialize)
#define full_init             export_alias(full_init)

/* Libc drop-in replacements */
#define builtin_strlen   export_alias(builtin_strlen)
#define builtin_strcpy   export_alias(builtin_strcpy)
#define builtin_strncpy  export_alias(builtin_strncpy)
#define builtin_strcat   export_alias(builtin_strcat)
#define builtin_strncat  export_alias(builtin_strncat)
#define builtin_strncat  export_alias(builtin_strncat)
#define builtin_strcmp   export_alias(builtin_strcmp)
#define builtin_strncmp  export_alias(builtin_strncmp)
#define builtin_memcpy   export_alias(builtin_memcpy)
#define builtin_memset   export_alias(builtin_memset)
#define builtin_memcmp   export_alias(builtin_memcmp)
#define builtin_memmove  export_alias(builtin_memmove)

/* stdio.h replacement functions */
#define builtin_printf   export_alias(builtin_printf)
#define builtin_fprintf  export_alias(builtin_fprintf)
#define builtin_dprintf  export_alias(builtin_dprintf)
#define builtin_sprintf  export_alias(builtin_sprintf)
#define builtin_snprintf export_alias(builtin_snprintf)
#define builtin_syslog   export_alias(builtin_syslog)

/* Memory state initialization */
#define memory_clean          export_alias(memory_clean)
#define mspaces_init          export_alias(mspaces_init)
#define memory_init           export_alias(memory_init)

/* Heap size */
#define heap_allocation_size      export_alias(heap_allocation_size)
#define get_heap_allocation_size  export_alias(get_heap_allocation_size)

/* Temporal analysis */
/* No need to encapsulate via ifdef: using these extra definitions does
   not hurt, otherwise need to pass additional parameters to frama-c */
#define temporal_store_nblock             export_alias(temporal_store_nblock)
#define temporal_store_nreferent          export_alias(temporal_store_nreferent)
#define temporal_save_nblock_parameter    export_alias(temporal_save_nblock_parameter)
#define temporal_save_nreferent_parameter export_alias(temporal_save_nreferent_parameter)
#define temporal_save_copy_parameter  export_alias(temporal_save_copy_parameter)
#define temporal_pull_parameter           export_alias(temporal_pull_parameter)
#define temporal_save_return              export_alias(temporal_save_return)
#define temporal_reset_parameters        export_alias(temporal_reset_parameters)
#define temporal_pull_return              export_alias(temporal_pull_return)
#define temporal_reset_return             export_alias(temporal_reset_return)
#define temporal_memcpy                   export_alias(temporal_memcpy)
#define temporal_memset                   export_alias(temporal_memset)

/* Infinity values for floating point types */
#define math_HUGE_VAL                   export_alias(math_HUGE_VAL)
#define math_HUGE_VALF                  export_alias(math_HUGE_VALF)
#define math_INFINITY                   export_alias(math_INFINITY)
#define floating_point_exception        export_alias(floating_point_exception)
/* }}} */

/************************************************************************/
/*** Assertion {{{ ***/
/************************************************************************/

/*! \brief Runtime assertion verifying a given predicate
 *  \param pred  integer code of a predicate
 *  \param kind  C string representing a kind an annotation (e.g., "Assertion")
 *  \param fct
 *  \param pred_txt  stringified predicate
 *  \param line line of predicate placement in the un-instrumented file */
/*@ requires pred != 0;
  @ assigns \nothing; */
void assert(int pred, char *kind, char *fct, char *pred_txt, int line)
  __attribute__((FC_BUILTIN));
/* }}} */

/************************************************************************/
/*** Dynamic memory allocation {{{ ***/
/************************************************************************/

/*! \brief Drop-in replacement for \p malloc with memory tracking enabled.
 * For further information, see \p malloc(3). */
void * malloc(size_t size)
  __attribute__((FC_BUILTIN));

/*! \brief Drop-in replacement for \p calloc with memory tracking enabled.
 * For further information, see \p calloc(3). */
void * calloc(size_t nbr_elt, size_t size_elt)
  __attribute__((FC_BUILTIN));

/*! \brief Drop-in replacement for \p realloc with memory tracking enabled.
 * For further information, see realloc(3) */
void * realloc(void * ptr, size_t size)
  __attribute__((FC_BUILTIN));

/*! \brief Drop-in replacement for \p free with memory tracking enabled.
 * For further information, see \p free(3). */
void free(void * ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Allocate `size` bytes of memory such that the allocation's base
 * address is an even multiple of alignment.
 *
 * \param alignment - should be the power of two
 * \param size - should be the multiple of alignment
 * \return - pointer to the allocated memory if the restrictions placed on size
 *   and alignment parameters hold. NULL is returned otherwise. */
void *aligned_alloc(size_t alignment, size_t size)
  __attribute__((FC_BUILTIN));

/*! \brief Allocate size bytes and place the address of the allocated memory in
 * `*memptr`.  The address of the allocated memory will be a multiple of
 * `alignment`, which must be a power of two and a multiple of `sizeof(void*)`.
 * If size  is  0, then the value placed in *memptr is NULL. */
int posix_memalign(void **memptr, size_t alignment, size_t size)
  __attribute__((FC_BUILTIN));
/* }}} */

/************************************************************************/
/*** Memory tracking {{{ ***/
/************************************************************************/

/*! \brief Initialize memory locations. */
void mspaces_init()
  __attribute__((FC_BUILTIN));

/*! \brief Initialize memory tracking state.
 * Called before any other statement in \p main */
/*@ assigns \nothing; */
void memory_init(int *argc_ref, char ***argv, size_t ptr_size)
  __attribute__((FC_BUILTIN));

/*! \brief Clean-up memory tracking state before a program's termination. */
/*@ assigns \nothing; */
void memory_clean(void)
  __attribute__((FC_BUILTIN));

/*! \brief Store stack or globally-allocated memory block
 * starting at an address given by \p ptr.
 *
 * \param ptr base address of the tracked memory block
 * \param size size of the tracked block in bytes */
/*@ ensures \result == ptr;
  @ assigns \result \from *(((char*)ptr)+(0..size-1)), ptr, size; */
void * store_block(void * ptr, size_t size)
  __attribute__((FC_BUILTIN));

/*! \brief Same as `store_block`, but first check
 * checks whether a block with a base address given by `ptr` exists in the
 * tracked allocation and remove it before storing a new block.
 *
 * \param ptr base address of the tracked memory block
 * \param size size of the tracked block in bytes */
/*@ ensures \result == ptr;
  @ assigns \result \from *(((char*)ptr)+(0..size-1)), ptr, size; */
void * store_block_duplicate(void * ptr, size_t size)
  __attribute__((FC_BUILTIN));

/*! \brief Remove a memory block which base address is \p ptr from tracking. */
/*@ assigns \nothing; */
void delete_block(void * ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Mark the \p size bytes starting at an address given by \p ptr as
 * initialized. */
/*@ assigns \nothing; */
void initialize(void * ptr, size_t size)
  __attribute__((FC_BUILTIN));

/*! \brief Mark all bytes belonging to a memory block which start address is
 * given by \p ptr as initialized. */
/*@ assigns \nothing; */
void full_init(void * ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Mark a memory block which start address is given by \p ptr as
 * read-only. */
/*@ assigns \nothing; */
void mark_readonly(void * ptr)
  __attribute__((FC_BUILTIN));

/*! \brief A variable holding a byte size of tracked heap allocation. */
extern size_t heap_allocation_size;

/*@ predicate diffSize{L1,L2}(integer i) =
  \at(heap_allocation_size, L1)
    - \at(heap_allocation_size, L2) == i; */

/*! \brief Return the cumulative size (in bytes) of tracked heap allocation. */
/*@ assigns \result \from heap_allocation_size; */
size_t get_heap_allocation_size(void)
  __attribute__((FC_BUILTIN));

/* }}} */

/************************************************************************/
/*** E-ACSL predicates {{{ ***/
/************************************************************************/

/*!\brief Implementation of the \b \\freeable predicate of E-ACSL.
 *
 * Evaluate to a non-zero value if \p ptr points to a start address of
 * a block allocated via \p malloc, \p calloc or \p realloc. */
/*@ assigns \result \from ptr; */
int freeable(void * ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Implementation of the \b \\valid predicate of E-ACSL.
 *
 * \\valid evaluates an expression of the form `p+i`, where `p` is a pointer
 * and `i` is an integer offset and returns `true` if both `p` and `p+i` belong
 * to the same allocated memory block.
 *
 * @param ptr - memory address under question
 * @param size - the byte-length (starting from `ptr`) of the memory area which
 *  needs to be valid
 * @param base - if `ptr` can be represented by the expression `p+i` then
 *  `base` refers to `p`
 * @param addrof_base - if `ptr` can be represented by the expression `p+i`
 * then `addrof_base` refers to `&p`. For the cases when the address of `p`
 * cannot be taken (e.g., address of a static array or a constant value
 * casted to a pointer) then `addrof_base` is zero.
 *
 * @returns
 *  `true` if regions `[ptr, ptr + size]` and `[base, base + size]` are
 *  writable and lie within the same memory block and `false` otherwise.
 *  If `weak validity` is used (see macro `E_ACSL_WEAK_VALIDITY`)
 *  then only region `[ptr, ptr + size]` should lie within the same block
 *  and be writable.
 */
/*@ assigns \result \from *(((char*)ptr)+(0..size-1)), ptr, size;
  @ behavior valid:
  @   assumes \valid(((char *)ptr)+(0..size-1));
  @   assumes
  @     size <= 0 ||
  @     ! \separated(((char *)ptr)+(0..size-1),
  @                  ((char *)\base_addr(base))+(0..\block_length(base)-1));
  @   ensures \result == 1;
  @ behavior invalid_ptr:
  @   assumes ! \valid(((char *)ptr)+(0..size-1));
  @   ensures \result == 0;
  @ behavior separated_ptr:
  @   assumes size > 0;
  @   assumes \separated(((char *)ptr)+(0..size-1),
  @                      ((char *)\base_addr(base))+(0..\block_length(base)-1));
  @   ensures \result == 0;
  @ complete behaviors;
  @ disjoint behaviors;
  @ */
int valid(void * ptr, size_t size, void *base, void *addrof_base)
  __attribute__((FC_BUILTIN));

/*! \brief Implementation of the \b \\valid_read predicate of E-ACSL.
 *
 * Same as ::valid except the checked memory locations are only
 * required to be allocated.  */
/*@ assigns \result \from *(((char*)ptr)+(0..size-1)), ptr, size;
  @ behavior valid:
  @   assumes \valid_read(((char *)ptr)+(0..size-1));
  @   assumes
  @     size <= 0 ||
  @     ! \separated(((char *)ptr)+(0..size-1),
  @                  ((char *)\base_addr(base))+(0..\block_length(base)-1));
  @   ensures \result == 1;
  @ behavior invalid_ptr:
  @   assumes ! \valid_read(((char *)ptr)+(0..size-1));
  @   ensures \result == 0;
  @ behavior separated_ptr:
  @   assumes size > 0;
  @   assumes \separated(((char *)ptr)+(0..size-1),
  @                      ((char *)\base_addr(base))+(0..\block_length(base)-1));
  @   ensures \result == 0;
  @ complete behaviors;
  @ disjoint behaviors;
  @ */
int valid_read(void * ptr, size_t size, void *base, void *addrof_base)
  __attribute__((FC_BUILTIN));

/*! \brief Implementation of the \b \\base_addr predicate of E-ACSL.
 * Return the base address of the memory block containing an address given
 * by \p ptr */
/*@ ensures \result == \base_addr(ptr);
  @ assigns \result \from ptr; */
void * base_addr(void * ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Implementation of the \b \\block_length predicate of E-ACSL.
 * Return the byte length of the memory block of the block containing a memory
 * address given by \p ptr */
/*@ ensures \result == \block_length(ptr);
  @ assigns \result \from ptr; */
size_t block_length(void * ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Implementation of the \b \\offset predicate of E-ACSL.
 * Return the byte offset of address given by \p ptr within a memory blocks
 * it belongs to */
/*@ ensures \result == \offset(ptr);
  @ assigns \result \from ptr; */
size_t offset(void * ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Implementation of the \b \\initialized predicate of E-ACSL.
 * Return a non-zero value if \p size bytes starting from an address given by
 * \p ptr are initialized and zero otherwise. */
/*@ assigns \result \from *(((char*)ptr)+(0..size-1)), ptr, size;
  @ behavior initialized:
  @   assumes \initialized(((char *)ptr)+(0..size-1));
  @   ensures \result == 1;
  @ behavior uninitialized:
  @   assumes ! \initialized(((char *)ptr)+(0..size-1));
  @   ensures \result == 0;
  @ complete behaviors;
  @ disjoint behaviors;
  @ */
int initialized(void * ptr, size_t size)
  __attribute__((FC_BUILTIN));
/* }}} */

/************************************************************************/
/*** Drop-in replacement functions {{{ ***/
/************************************************************************/

/*@ assigns dest[0..] \from src[0..];
  @ assigns \result \from dest;
  @ ensures \result == dest; */
char *builtin_strcpy(char *dest, const char *src)
  __attribute__((FC_BUILTIN));

/*@ assigns dest[0..n - 1] \from src[0..n-1];
  @ assigns \result \from dest;
  @ ensures \result == dest; */
char *builtin_strncpy(char *dest, const char *src, size_t n)
  __attribute__((FC_BUILTIN));

/*@ assigns \result \from s[0..]; */
size_t builtin_strlen(const char *s)
  __attribute__((FC_BUILTIN));

/*@ assigns dest[..] \from src[0..];
  @ assigns \result \from dest;
  @ ensures \result == dest; */
char *builtin_strcat(char *dest, const char *src)
  __attribute__((FC_BUILTIN));

/*@ assigns dest[..] \from src[0..n];
  @ assigns \result \from dest;
  @ ensures \result == dest; */
char *builtin_strncat(char *dest, const char *src, size_t n)
  __attribute__((FC_BUILTIN));

/*@ assigns \result \from s1[0..], s2[0..]; */
int builtin_strcmp(const char *s1, const char *s2)
  __attribute__((FC_BUILTIN));

/*@ assigns \result \from s1[0..n-1], s2[0..n-1]; */
int builtin_strncmp(const char *s1, const char *s2, size_t n)
  __attribute__((FC_BUILTIN));

/*@ assigns ((char*)dest)[0..n-1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest;
  @ ensures \result == dest; */
void *builtin_memcpy(void *dest, const void *src, size_t n)
  __attribute__((FC_BUILTIN));

/*@ assigns ((char*)s)[0..n-1] \from c;
  @ assigns \result \from s;
  @ ensures \result == s; */
void *builtin_memset(void *s, int c, size_t n)
  __attribute__((FC_BUILTIN));

/*@ assigns ((char*)dest)[0..n-1] \from ((char*)src)[0..n-1];
  @ assigns \result \from dest;
  @ ensures \result == dest; */
void *builtin_memmove(void *dest, const void *src, size_t n)
  __attribute__((FC_BUILTIN));

/*@ assigns \result \from ((char*)s1)[0..n-1], ((char*)s2)[0..n-1]; */
int builtin_memcmp(const void *s1, const void *s2, size_t n)
  __attribute__((FC_BUILTIN));

/* }}} */

/************************************************************************/
/*** Format functions {{{ ***/
/************************************************************************/

/** \brief `printf` with error checking. */
int builtin_printf(const char *fmtdesc, const char *fmt, ...)
  __attribute__((FC_BUILTIN));

/** \brief `fprintf` with error checking. */
int builtin_fprintf(const char *fmtdesc, FILE *stream, const char *fmt, ...)
 __attribute__((FC_BUILTIN));

/** \brief `dprintf` with error checking. */
int builtin_dprintf(const char *fmtdesc, int fd, const char *fmt, ...)
 __attribute__((FC_BUILTIN));

/** \brief `sprintf` with error checking. */
int builtin_sprintf(const char *fmtdesc, char *str, const char *fmt, ...)
 __attribute__((FC_BUILTIN));

/** \brief `snprintf` with error checking. */
int builtin_snprintf
  (const char *fmtdesc, char *str, size_t size, const char *fmt, ...)
 __attribute__((FC_BUILTIN));

/** \brief `syslog` with error checking. */
int builtin_syslog(const char *fmtdesc, int priority, const char *fmt, ...)
 __attribute__((FC_BUILTIN));
/* }}} */

/************************************************************************/
/*** Machine-dependent infinity values for flating points {{{ ***/
/************************************************************************/

/* Positive infinity for doubles: same as HUGE_VAL */
extern double math_HUGE_VAL
  __attribute__((FC_BUILTIN));

/* Positive infinity for floats: same as HUGE_VALF */
extern float  math_HUGE_VALF
  __attribute__((FC_BUILTIN));

/* Representation of infinity value for doubles: same as INFINITY */
extern double math_INFINITY
  __attribute__((FC_BUILTIN));

/* Check for floating point exception at a given execution point */
extern void floating_point_exception(const char *s)
  __attribute__((FC_BUILTIN));
/* }}} */

/************************************************************************/
/*** Temporal analysis {{{ ***/
/************************************************************************/

/*! \brief Take origin number of a memory block containing `block_addr` and
 * store it as a referent number of a pointer given by `ptr_addr`. */
/*@ assigns \nothing; */
void temporal_store_nblock(void *ptr_addr, void *block_addr)
  __attribute__((FC_BUILTIN));

/*! \brief Same as `temporal_store_nblock` but take a referent
 * number of `block_addr` instead */
/*@ assigns \nothing; */
void temporal_store_nreferent(void *ptr_addr, void *block_addr)
  __attribute__((FC_BUILTIN));

/*! \brief store struct { .ptr = ptr, .temporal_flow = TReferentN }
 *  in the global parameter array. */
/*@ assigns \nothing; */
void temporal_save_nreferent_parameter(void *ptr, unsigned int param)
  __attribute__((FC_BUILTIN));

/*! \brief store struct { .ptr = ptr, .temporal_flow = TBlockN }
 *  in the global parameter array. */
/*@ assigns \nothing; */
void temporal_save_nblock_parameter(void *ptr, unsigned int param)
  __attribute__((FC_BUILTIN));

/*! \brief store struct { .ptr = ptr, .temporal_flow = TCopy } in the global
 *  parameter array. */
/*@ assigns \nothing; */
void temporal_save_copy_parameter(void *ptr, unsigned int param)
  __attribute__((FC_BUILTIN));

/*! \brief Assign a referent number of `ptr` based on the record in the global
 * parameter array at index `param`. */
/*@ assigns \nothing; */
void temporal_pull_parameter(void *ptr, unsigned int param, size_t size)
  __attribute__((FC_BUILTIN));

/*! \brief Nullify global parameter array  */
/*@ assigns \nothing; */
void temporal_reset_parameters()
  __attribute__((FC_BUILTIN));

/*! \brief Save temporal referent number of `ptr` in a placeholder variable
 * tracking the referent number of a function's return. */
/*@ assigns \nothing; */
void temporal_save_return(void *ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Take a temporal referent stored in the placeholder tracking return
 * values  as a temporal referent number of `ptr`. */
/*@ assigns \nothing; */
void temporal_pull_return(void *ptr)
  __attribute__((FC_BUILTIN));

/*! \brief Nullify a placeholder variable tracking the referent number of a
 * function's return. */
/*@ assigns \nothing; */
void temporal_reset_return()
  __attribute__((FC_BUILTIN));

/*! \brief Copy temporal shadow data from [src, src + size] to
 * [dest, dest + size]. Counterpart of the memcpy function */
/*@ assigns \nothing; */
void temporal_memcpy(void *dest, void *src, size_t size)
  __attribute__((FC_BUILTIN));

/*! \brief Set temporal shadow data from [src, src + size] to 0.
 * Counterpart of memset the function */
/*@ assigns \nothing; */
void temporal_memset(void *dest, int n, size_t size)
  __attribute__((FC_BUILTIN));

/* }}} */
#endif
