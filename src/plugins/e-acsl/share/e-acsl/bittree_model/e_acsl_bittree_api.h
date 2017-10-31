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
 * \file  e_acsl_bittree_api.h
 * \brief Patricia Trie API
***************************************************************************/

#ifndef E_ACSL_BITTREE_API
#define E_ACSL_BITTREE_API

/*! \brief Structure representing an allocated memory block */
struct bt_block {
  size_t ptr;  //!< Base address
  size_t size; //!< Block length (in bytes)
  unsigned char * init_ptr; //!< Per-bit initialization
  size_t init_bytes; //!< Number of initialized bytes within a block
  int is_readonly; //!< True if a block is marked read-only
  int is_freeable; //!< True if a block can be de-allocated using `free`
#ifdef E_ACSL_DEBUG
  size_t line; //!< Line number where this block was recorded
  char* file; //!< File name where this block was recorded
#endif
#ifdef E_ACSL_TEMPORAL
  uint32_t timestamp; //!< Temporal timestamp of a block's creation
  void *temporal_shadow; //!< Temporal shadow for storing referent numbers
#endif
};

typedef struct bt_block bt_block;

/*! \brief Structure representing a bittree node */
struct bt_node {
  int is_leaf;
  size_t addr, mask;
  struct bt_node * left, * right, * parent;
  bt_block * leaf;
};

typedef struct bt_node bt_node;

/*! \brief Remove a block from the structure */
static void bt_remove(bt_block *b);

/*! \brief Add a block to the structure */
static void bt_insert(bt_block *b);

/*! \brief Look-up a memory block by its base address
  NB: The function assumes that such a block exists. */
static bt_block * bt_lookup(void *ptr);

/*! \brief Find a memory block containing a given memory address
 *
 * Return block B such that:
 *  `\base_addr(B->ptr) <= ptr < (\base_addr(B->ptr) + size)`
 *  or NULL if such a block does not exist. */
static bt_block * bt_find(void *ptr);

/*! \brief Erase the contents of the structure */
static void bt_clean(void);

/*! \brief Erase information about a block's initialization */
static void bt_clean_block_init(bt_block *b);

/*! \brief Erase all information about a given block */
static void bt_clean_block(bt_block *b);

#ifdef E_ACSL_DEBUG
/*! \brief Print information about a given block */
static void bt_print_block(bt_block *b);

/*! \brief Recursively print the contents of the bittree starting from a
 * given node */
/*@ assigns \nothing; */
static void bt_print_node(bt_node * ptr, int depth);

/*! \brief Print the contents of the entire bittree */
/*@ assigns \nothing; */
static void bt_print();
#endif

#endif
