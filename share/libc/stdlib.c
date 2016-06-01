/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2016                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  All rights reserved.                                                  */
/*  Contact CEA LIST for licensing.                                       */
/*                                                                        */
/**************************************************************************/

/* ISO C: 7.20 */
#include "stdlib.h"
#include "__fc_builtin.h"

/* This file is part of the Frama-C framework.
   It must be included in all files calling malloc of free as it defines macros.
   3 different implementation are available: you should define one of these:
   FRAMA_C_MALLOC_STACK
   FRAMA_C_MALLOC_INDIVIDUAL
   FRAMA_C_MALLOC_HEAP
   to select the proper one. */


#ifdef FRAMA_C_MALLOC_INDIVIDUAL

/* This malloc must not be used if the analyzer cannot determine that there is
   only a finite number of calls to malloc.
   It checks for out-of-bound accesses to malloc'ed blocks.
   Each call to malloc is separated from the others. This might create too many
   different bases.
*/

void *Frama_C_alloc_size(size_t size);

void *malloc(size_t size) {
  void *malloc_result = Frama_C_alloc_size(size);
  return malloc_result;
}

void Frama_C_free(void*base);

void free(void *p) {
  if (p) Frama_C_free(p);
}

#else

#ifdef FRAMA_C_MALLOC_HEAP

/* malloc is always safe and may return NULL.
   free() does not check anything.
   All calls to malloc return offsets inside the same memory zone.
   Drawback : malloc pointers are not separated.
*/

/* Size of mallocable memory in bytes. */
#ifndef MEMORY_SIZE
# define MEMORY_SIZE (1<<10)
#endif

char MEMORY[MEMORY_SIZE];

void *malloc(size_t size) {
  static int next_free = 0;
  next_free += size;
  if (next_free>=MEMORY_SIZE) return NULL;
  return (MEMORY+(next_free-size));
}

void free(void*p) {
  return;
}

#else

#ifdef FRAMA_C_MALLOC_STACK

void * Frama_C_alloc_by_stack(size_t size);
void Frama_C_free(void*base);

void *malloc(size_t size) {
  return Frama_C_alloc_by_stack(size);
}

void free(void *p) {
  if (p) Frama_C_free(p);
}

#else
#error Please define one of: \
  FRAMA_C_MALLOC_STACK FRAMA_C_MALLOC_INDIVIDUAL FRAMA_C_MALLOC_HEAP
#endif
#endif
#endif

void *calloc(size_t nmemb, size_t size)
{
  size_t l = nmemb * size;
  char *p = malloc(l);
  Frama_C_memset(p, 0, l);
  return p;
}
