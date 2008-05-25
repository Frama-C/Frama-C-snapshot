/* run.config
   DONTRUN: annotations grammar needs update.
*/
/*****************************************************************************/
/* Attempt to define a running example for ACSL (Ansi C Specification        */
/* Language), much as the Purse example in JML description papers.           */
/* It is a memory allocator, whose main functions are [memory_alloc] and     */
/* [memory_free], to respectively allocate and deallocate memory.            */
/* The goal is to exercise as much as possible of ACSL.                      */
/* This file presents the more complex version of the allocator.             */
/*****************************************************************************/


#include <stdlib.h>

#define DEFAULT_BLOCK_SIZE 1000

typedef enum _bool { false = 0, true = 1 } bool;

/*@ predicate finite_list<a>((a* -> a*) next_elem, a* ptr) {
  @   ptr == \null || (\valid(ptr) && finite_list(next_elem(ptr)))
  @ }
  @
  @ logic int list_length<a>((a* -> a*) next_elem, a* ptr) {
  @   (ptr == \null) ? 0 : 1 + list_length(next_elem(ptr))
  @ }
  @
  @ predicate lower_length<a>((a* -> a*) next_elem, a* ptr1, a* ptr2) {
  @   finite_list(next_elem, ptr1) && finite_list(next_elem, ptr2)
  @   && list_length(next_elem, ptr1) < list_length(next_elem, ptr2)
  @ } */

// forward reference
struct _memory_slice;

/* A memory block holds a pointer to a raw block of memory allocated by
 * calling [malloc]. It is sliced into chunks, which are maintained by
 * the [slice] structure. It maintains additional information such as
 * the [size] of the memory block, the number of bytes [used] and the [next]
 * index at which to put a chunk.
 */
typedef struct _memory_block {
  //@ ghost bool        packed;
    // ghost field [packed] is meant to be used as a guard that tells when
    // the invariant of a structure of type [memory_block] holds
  unsigned int          size;
    // size of the array [data]
  unsigned int          next;
    // next index in [data] at which to put a chunk
  unsigned int          used;
    // how many bytes are used in [data], not necessarily contiguous ones
  char*                 data;
    // raw memory block allocated by [malloc]
  struct _memory_slice* slice;
    // structure that describes the slicing of a block into chunks
} memory_block;

/*@ type invariant inv_memory_block(memory_block mb) {
  @   mb.packed ==>
  @     (0 < mb.size && mb.used <= mb.next <= mb.size
  @     && \offset(mb.data) == 0
  @     && \block_length(mb.data) == mb.size)
  @ }
  @
  @ predicate valid_memory_block(memory_block* mb) {
  @   \valid(mb) && mb->packed
  @ } */

/* A memory chunk holds a pointer [data] to some part of a memory block
 * [block]. It maintains the [offset] at which it points in the block, as well
 * as the [size] of the block it is allowed to access. A field [free] tells
 * whether the chunk is used or not.
 */
typedef struct _memory_chunk {
  //@ ghost bool packed;
    // ghost field [packed] is meant to be used as a guard that tells when
    // the invariant of a structure of type [memory_chunk] holds
  unsigned int   offset;
    // offset at which [data] points into [block->data]
  unsigned int   size;
    // size of the chunk
  bool           free;
    // true if the chunk is not used, false otherwise
  memory_block*  block;
    // block of memory into which the chunk points
  char*          data;
    // shortcut for [block->data + offset]
} memory_chunk;

/*@ type invariant inv_memory_chunk(memory_chunk mc) {
  @   mc.packed ==>
  @     (0 < mc.size && valid_memory_block(mc.block)
  @     && mc.offset + mc.size <= mc.block->next)
  @ }
  @
  @ predicate valid_memory_chunk(memory_chunk* mc, int s) {
  @   \valid(mc) && mc->packed && mc->size == s
  @ }
  @
  @ predicate used_memory_chunk(memory_chunk mc) {
  @   mc.free == false
  @ }
  @
  @ predicate freed_memory_chunk(memory_chunk mc) {
  @   mc.free == true
  @ } */

/* A memory chunk list links memory chunks in the same memory block.
 * Newly allocated chunks are put first, so that the offset of chunks
 * decreases when following the [next] pointer. Allocated chunks should
 * fill the memory block up to its own [next] index.
 */
typedef struct _memory_chunk_list {
  memory_chunk*              chunk;
    // current list element
  struct _memory_chunk_list* next;
    // tail of the list
} memory_chunk_list;

/*@ \let next_chunk = \lambda memory_chunk_list* ptr; ptr->next ;
  @
  @ predicate valid_memory_chunk_list
  @                  (memory_chunk_list* mcl, memory_block* mb) {
  @   \valid(mcl) && valid_memory_chunk(mcl->chunk,mcl->chunk->size)
  @   && mcl->chunk->block == mb
  @   && (mcl->next == \null || valid_memory_chunk_list(mcl->next, mb))
  @   && mcl->offset == mcl->chunk->offset
  @   && (
  @        // it is the last chunk in the list
  @        (mcl->next == \null && mcl->chunk->offset == 0)
  @      ||
  @        // it is a chunk in the middle of the list
  @        (mcl->next != \null
  @        && mcl->next->chunk->offset + mcl->next->chunk->size
  @           == mcl->chunk->offset)
  @      )
  @   && finite_list(next_chunk, mcl)
  @ }
  @
  @ predicate valid_complete_chunk_list
  @                  (memory_chunk_list* mcl, memory_block* mb) {
  @   valid_memory_chunk_list(mcl,mb)
  @   && mcl->next->chunk->offset + mcl->next->chunk->size == mb->next
  @ }
  @
  @ predicate chunk_lower_length(memory_chunk_list* ptr1,
  @                              memory_chunk_list* ptr2) {
  @   lower_length(next_chunk, ptr1, ptr2)
  @ } */

/* A memory slice holds together a memory block [block] and a list of chunks
 * [chunks] on this memory block.
 */
typedef struct _memory_slice {
  //@ ghost bool     packed;
    // ghost field [packed] is meant to be used as a guard that tells when
    // the invariant of a structure of type [memory_slice] holds
  memory_block*      block;
  memory_chunk_list* chunks;
} memory_slice;

/*@ type invariant inv_memory_slice(memory_slice* ms) {
  @   ms.packed ==>
  @     (valid_memory_block(ms->block) && ms->block->slice == ms
  @     && (ms->chunks == \null
  @         || valid_complete_chunk_list(ms->chunks, ms->block)))
  @ }
  @
  @ predicate valid_memory_slice(memory_slice* ms) {
  @   \valid(ms) && ms->packed
  @ } */

/* A memory slice list links memory slices, to form a memory pool.
 */
typedef struct _memory_slice_list {
  //@ ghost bool     packed;
    // ghost field [packed] is meant to be used as a guard that tells when
    // the invariant of a structure of type [memory_slice_list] holds
  memory_slice*              slice;
    // current list element
  struct _memory_slice_list* next;
    // tail of the list
} memory_slice_list;

/*@ \let next_slice = \lambda memory_slice_list* ptr; ptr->next ;
  @
  @ type invariant inv_memory_slice_list(memory_slice_list* msl) {
  @   msl.packed ==>
  @     (valid_memory_slice(msl->slice)
  @     && (msl->next == \null || valid_memory_slice_list(msl->next))
  @     && finite_list(next_slice, msl))
  @ }
  @
  @ predicate valid_memory_slice_list(memory_slice_list* msl) {
  @   \valid(msl) && msl->packed
  @ }
  @
  @ predicate slice_lower_length(memory_slice_list* ptr1,
  @                              memory_slice_list* ptr2) {
  @   \let next_slice = \lambda memory_slice_list* ptr; ptr->next ;
  @      lower_length(next_slice, ptr1, ptr2)
  @ } */

typedef memory_slice_list* memory_pool;

/*@ type invariant valid_memory_pool(memory_pool *mp) {
  @   \valid(mp) && valid_memory_slice_list(*mp)
  @ } */

/*@ behavior zero_size:
  @   assumes s == 0;
  @   assigns \empty;
  @   ensures \result == 0;
  @
  @ behavior positive_size:
  @   assumes s > 0;
  @   requires valid_memory_pool(arena);
  @   ensures \result == 0
  @     || (valid_memory_chunk(\result,s) && used_memory_chunk(*\result));
  @ */
memory_chunk* memory_alloc(memory_pool* arena, unsigned int s) {
  memory_slice_list *msl = *arena;
  memory_chunk_list *mcl;
  memory_slice *ms;
  memory_block *mb;
  memory_chunk *mc;
  unsigned int mb_size;
  //@ ghost unsigned int mcl_offset;
  char *mb_data;
  // guard condition
  if (s == 0) return 0;
  // iterate through memory blocks (or slices)
  /*@
    @ loop invariant valid_memory_slice_list(msl);
    @ loop variant msl for slice_lower_length;
    @ */
  while (msl != 0) {
    ms = msl->slice;
    mb = ms->block;
    mcl = ms->chunks;
    // does [mb] contain enough free space?
    if (s <= mb->size - mb->next) {
      //@ ghost ms->ghost = false;   // unpack the slice
      // allocate a new chunk
      mc = (memory_chunk*)malloc(sizeof(memory_chunk));
      if (mc == 0) return 0;
      mc->offset = mb->next;
      mc->size = s;
      mc->free = false;
      mc->block = mb;
      //@ ghost mc->ghost = true;   // pack the chunk
      // update block accordingly
      //@ ghost mb->ghost = false;   // unpack the block
      mb->next += s;
      mb->used += s;
      //@ ghost mb->ghost = true;   // pack the block
      // add the new chunk to the list
      mcl = (memory_chunk_list*)malloc(sizeof(memory_chunk_list));
      if (mcl == 0) return 0;
      mcl->chunk = mc;
      mcl->next = ms->chunks;
      ms->chunks = mcl;
      //@ ghost ms->ghost = true;   // pack the slice
      return mc;
    }
    // iterate through memory chunks
    /*@
      @ loop invariant valid_memory_chunk_list(mcl,mb);
      @ loop variant mcl for chunk_lower_length;
      @ */
    while (mcl != 0) {
      mc = mcl->chunk;
      // is [mc] free and large enough?
      if (mc->free && s <= mc->size) {
        mc->free = false;
        mb->used += mc->size;
        return mc;
      }
      // try next chunk
      mcl = mcl->next;
    }
    msl = msl->next;
  }
  // allocate a new block
  mb_size = (DEFAULT_BLOCK_SIZE < s) ? s : DEFAULT_BLOCK_SIZE;
  mb_data = (char*)malloc(mb_size);
  if (mb_data == 0) return 0;
  mb = (memory_block*)malloc(sizeof(memory_block));
  if (mb == 0) return 0;
  mb->size = mb_size;
  mb->next = s;
  mb->used = s;
  mb->data = mb_data;
  //@ ghost mb->ghost = true;   // pack the block
  // allocate a new chunk
  mc = (memory_chunk*)malloc(sizeof(memory_chunk));
  if (mc == 0) return 0;
  mc->offset = 0;
  mc->size = s;
  mc->free = false;
  mc->block = mb;
  //@ ghost mc->ghost = true;   // pack the chunk
  // allocate a new chunk list
  mcl = (memory_chunk_list*)malloc(sizeof(memory_chunk_list));
  if (mcl == 0) return 0;
  //@ ghost mcl->offset = 0;
  mcl->chunk = mc;
  mcl->next = 0;
  // allocate a new slice
  ms = (memory_slice*)malloc(sizeof(memory_slice));
  if (ms == 0) return 0;
  ms->block = mb;
  ms->chunks = mcl;
  //@ ghost ms->ghost = true;   // pack the slice
  // update the block accordingly
  mb->slice = ms;
  // add the new slice to the list
  msl = (memory_slice_list*)malloc(sizeof(memory_slice_list));
  if (msl == 0) return 0;
  msl->slice = ms;
  msl->next = *arena;
  //@ ghost msl->ghost = true;   // pack the slice list
  *arena = msl;
  return mc;
}

/*@ behavior null_chunk:
  @   assumes chunk == \null;
  @   assigns \empty;
  @
  @ behavior valid_chunk:
  @   assumes chunk != \null;
  @   requires valid_memory_pool(arena);
  @   requires valid_memory_chunk(chunk,chunk->size);
  @   requires used_memory_chunk(chunk);
  @   ensures
  @       // if it is not the last chunk in the block, mark it as free
  @       (valid_memory_chunk(chunk,chunk->size)
  @       && freed_memory_chunk(chunk))
  @     ||
  @       // if it is the last chunk in the block, deallocate the block
  @       ! \valid(chunk);
  @ */
void memory_free(memory_pool* arena, memory_chunk* chunk) {
  memory_slice_list *msl = *arena;
  memory_block *mb = chunk->block;
  memory_slice *ms = mb->slice;
  memory_chunk_list *mcl;
  memory_chunk *mc;
  // is it the last chunk in use in the block?
  if (mb->used == chunk->size) {
    // remove the corresponding slice from the memory pool
    // case it is the first slice
    if (msl->slice == ms) {
      *arena = msl->next;
      //@ ghost msl->ghost = false;    // unpack the slice list
      free(msl);
    }
    // case it is not the first slice
    while (msl != 0) {
      if (msl->next != 0 && msl->next->slice == ms) {
        memory_slice_list* msl_next = msl->next;
	msl->next = msl->next->next;
	// unpack the slice list
	//@ ghost msl_next->ghost = false;
	free(msl_next);
	break;
      }
      msl = msl->next;
    }
    //@ ghost ms->ghost = false;    // unpack the slice
    // deallocate all chunks in the block
    mcl = ms->chunks;
    // iterate through memory chunks
    /*@
      @ loop invariant valid_memory_chunk_list(mcl,mb);
      @ loop variant mcl for chunk_lower_length;
      @ */
    while (mcl != 0) {
      memory_chunk_list *mcl_next = mcl->next;
      mc = mcl->chunk;
      //@ ghost mc->ghost = false;    // unpack the chunk
      free(mc);
      free(mcl);
      mcl = mcl_next;
    }
    mb->next = 0;
    mb->used = 0;
    // deallocate the memory block and its data
    //@ ghost mb->ghost = false;     // unpack the block
    free(mb->data);
    free(mb);
    // deallocate the corresponding slice
    free(ms);
    return;
  }
  // mark the chunk as freed
  chunk->free = true;
  // update the block accordingly
  mb->used -= chunk->size;
  return;
}
