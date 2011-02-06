/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2011                                               */
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

/* $Id: malloc.c,v 1.23 2008-10-03 11:09:19 uid562 Exp $ */

/* This file is part of the Frama-C framework.
   It must be included in all files calling malloc of free as it defines macros.
   4 different implementation are available: you should define one of these:
   FRAMA_C_MALLOC_HEAP
   FRAMA_C_MALLOC_CHUNKS
   FRAMA_C_MALLOC_INDIVIDUAL
   FRAMA_C_MALLOC_POSITION
   to select the proper one.
*/

#include "libc.h"
#include "builtin.h"

#ifndef NULL
#define NULL ((void*)0)
#endif

extern void*Frama_C_alloc_infinite(const char *);

/* Size of mallocable memory in bytes.
   Some implementations may not limit the memory size. */
#ifndef MEMORY_SIZE
# define MEMORY_SIZE 100000000
#endif

#ifdef FRAMA_C_MALLOC_POSITION
/*
   malloc is always safe and may return NULL.
   free() problems are checked heuristically.
   Two calls at different lines in a given file return separated zones.
   Drawback : successive malloc inside a loop are not separated.
*/
#define FRAMA_C_STRINGIFY(x) #x
#define FRAMA_C_XSTRINGIFY(x) FRAMA_C_STRINGIFY(x)
#define malloc(x) (Frama_C_malloc_at_pos(x,__FILE__ "_line_" FRAMA_C_XSTRINGIFY(__LINE__)))
#define FRAMA_C_LOCALIZE_WARNING(x) (x " file " __FILE__ " line " FRAMA_C_XSTRINGIFY(__LINE__))
#define FRAMA_C_VALID 1
#define FRAMA_C_FREED 2

static void *Frama_C_malloc_at_pos(size_t size,const char* file) {
  static int counter = 0;
  counter++;
  char *base = Frama_C_alloc_infinite(file);
  char *tag = Frama_C_alloc_infinite(base);
  size_t *next_free = Frama_C_alloc_infinite(tag);
  size_t index = *next_free;
  if (index+size>=MEMORY_SIZE) return NULL;
  *next_free += size;
  tag[index] = FRAMA_C_VALID;
  return base+index;
}

void free(void *ptr) {
  if (ptr==NULL) {
    Frama_C_show_each_warning(FRAMA_C_LOCALIZE_WARNING("potential free of NULL"));
    return;}
  char *tag = Frama_C_alloc_infinite(ptr);
  tag += Frama_C_offset(ptr);
  if (*tag != FRAMA_C_VALID)
    if (*tag == FRAMA_C_FREED)
      {
        Frama_C_show_each_warning (FRAMA_C_LOCALIZE_WARNING("potential double free"));
        Frama_C_abort();
        }
    else
      {
        Frama_C_show_each_warning (FRAMA_C_LOCALIZE_WARNING("potential invalid adress"));
        Frama_C_abort();
        }
  else { *tag = FRAMA_C_FREED;}
}
#else
#ifdef FRAMA_C_MALLOC_HEAP
/*
   malloc is always safe and may return NULL.
   free() does not check anything.
   All calls to malloc return offsets inside the same memory zone.
   Drawback : malloc pointers are not separated.
*/

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
#ifdef FRAMA_C_MALLOC_INDIVIDUAL
/*
   This malloc must not be used if the analyzer cannot determine that
   there is only a finite number of calls to malloc.
   It checks for out-of-bound accesses to malloc'ed blocks.
   It does not check free() problems.
   Each call to malloc is separated from the others. This might create too many
   different bases.
*/
void *Frama_C_alloc_size(size_t size);
void *malloc(size_t size) {
  return Frama_C_alloc_size(size);
}
void free(void*) {
  return;
}
#else
#ifdef FRAMA_C_MALLOC_CHUNKS
#define FRAMA_C_CHUNK_LENGTH 2000
/*
   This malloc must not be used if the analyzer cannot determine
   that there is only a finite number of calls to malloc.
   It does not check free() problems.
   Memory is allocated in separated chunks of memory of size CHUNK_LENGTH.
*/
void *malloc(size_t size) {
  static char* base= (char*)0;
  static int next_free = 0;
  void *addr;

  if (!base) base=Frama_C_alloc_infinite("MEMORY");

  if (next_free >= FRAMA_C_CHUNK_LENGTH)
    {
      next_free = 0;
      base = Frama_C_alloc_infinite(base);
    }
  addr = (void*) (base + next_free);
  next_free += size;

  return addr;
}

void free(void*) {
  return;
}
#else
#ifdef FRAMA_C_MALLOC_INFINITE
/*
   This malloc must not be used if the analyzer cannot determine that
   there is only a finite number of calls to malloc.
   It does not take into account the size and therefore does not checks
   for out-of-bound accesses to malloc'ed blocks.
   It does not check free() problems.
   Each call to malloc is separated from the others. This might create too many
   different zones.
*/
void *malloc(size_t size) {
  static char* const base="M";
#ifdef FRAMA_C_MALLOC_DEBUG
  CEA_F("Called malloc", base);
#endif
  return base = Frama_C_alloc_infinite(base);
}
void free(void*) {
  return;
}
#else
#error Please define one of: FRAMA_C_MALLOC_HEAP FRAMA_C_MALLOC_INFINITE\
 FRAMA_C_MALLOC_CHUNKS\
 FRAMA_C_MALLOC_INDIVIDUAL\
 FRAMA_C_MALLOC_POSITION.
#endif // FRAMA_C_MALLOC_INFINITE
#endif // FRAMA_C_MALLOC_CHUNKS
#endif // FRAMA_C_MALLOC_INDIVIDUAL
#endif // FRAMA_C_MALLOC_HEAP
#endif // FRAMA_C_MALLOC_POSITION


#if 0
void main0(void) {
  int *x = malloc((sizeof(int)));
  *x = 12;
  free(x);
  free(x);
}

void main1(void) {
  int *x;
  CEA_F(x);
  free(x);

}

void main2(void) {
  int x;
  CEA_F(x);
  free(&x);

}

void main3(void) {
  free((void*)0);
}

void main4(void) {
  int * x = Frama_C_alloc_infinite("toto");
  int * y = Frama_C_alloc_infinite("toto");
}

int G;
void main5(void) {
  int *x[5];
  for(int i=0; i<=4; i++) {
    x[i] = (int*)malloc(4);}

  free(x[2]);

  for(int i=0; i<=4; i++) {
    *x[i] = i;
    }
}
#endif
