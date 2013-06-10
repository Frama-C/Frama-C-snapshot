
/*****************************************************************************/
/* Attempt to define a running example for ACSL (Ansi C Specification        */
/* Language), much as the Purse example in JML description papers.           */
/* It is a memory allocator, whose main functions are [memory_alloc] and     */
/* [memory_free], to respectively allocate and deallocate memory.            */
/* The goal is to exercise as much as possible of ACSL.                      */
/* This file presents the basic version of the allocator.                    */
/*****************************************************************************/

//#include <stdlib.h>
//#include "../../share/libc.h"
#define FRAMA_C_MALLOC_POSITION
#include "share/libc/stdlib.c"

#define DEFAULT_BLOCK_SIZE 1000

typedef enum _bool { false = 0, true = 1 } bool;

typedef struct _memory_block {
  size_t size;
  bool         free;
  char*        data;
} memory_block;

/*@ type invariant inv_memory_block(memory_block mb) =
  @   0 < mb.size && \offset(mb.data) == 0 && \block_length{Here}(mb.data) == mb.size ;
  @*/

/*@ predicate used_memory_block{L}(memory_block mb) = 
  @   mb.free == false && inv_memory_block(mb) ;
  @*/

/*@ predicate freed_memory_block{L}(memory_block mb) =
  @   mb.free == true && inv_memory_block(mb) ;
  @*/

/*@ predicate valid_memory_block{L}(memory_block* mb) =
  @   \valid(mb) && inv_memory_block(*mb) ;
  @*/

/*@ predicate valid_used_memory_block{L}(memory_block* mb) =
  @   \valid(mb) && used_memory_block(*mb) ;
  @*/

/*@ predicate valid_freed_memory_block{L}(memory_block* mb) =
  @   \valid(mb) && freed_memory_block(*mb) ;
  @*/

typedef struct _memory_block_list {
  memory_block*              block;
  struct _memory_block_list* next;
} memory_block_list;

/*@ predicate valid_memory_block_list{L}(memory_block_list* mbl) =
  @   \valid(mbl) && valid_memory_block(mbl->block)
  @   && (mbl->next == \null || valid_memory_block_list(mbl->next)) ;
  @*/

typedef memory_block_list* memory_pool;

/*@ predicate valid_memory_pool{L}(memory_pool *mp) =
  @   \valid(mp) && valid_memory_block_list(*mp) ;
  @*/

/*@ requires valid_memory_pool(arena) && 0 < s;
  @ ensures valid_used_memory_block(\result);
  @ */
memory_block* memory_alloc(memory_pool* arena, size_t s) {
  memory_block_list *mbl = *arena;
  memory_block *mb;
  size_t mb_size;
  char *mb_data;
  // iterate through memory blocks
  while (mbl != 0) {
    mb = mbl->block;
    // is [mb] free and large enough?
    if (mb->free && s <= mb->size) {
      mb->free = false;
      return mb;
    }
    // try next block
    mbl = mbl->next;
  }
  // allocate a new block
  mb_size = (DEFAULT_BLOCK_SIZE < s) ? s : DEFAULT_BLOCK_SIZE;
  mb_data = (char*)malloc(mb_size);
  mb = (memory_block*)malloc(sizeof(memory_block));
  mb->size = mb_size;
  mb->free = false;
  mb->data = mb_data;
  // add the new block to the arena
  mbl = (memory_block_list*)malloc(sizeof(memory_block_list));
  mbl->block = mb;
  mbl->next = *arena;
  *arena = mbl;
  return mb;
}

/*@ requires valid_memory_pool(arena) && valid_used_memory_block(block);
  @ ensures valid_freed_memory_block(block);
  @ */
void memory_free(memory_pool* arena, memory_block* block) {
  block->free = true;
}
