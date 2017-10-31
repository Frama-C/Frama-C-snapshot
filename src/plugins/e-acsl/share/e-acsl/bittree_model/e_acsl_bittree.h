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
 * \file e_acsl_bittree.h
 * \brief Patricia Trie API Implementation
***************************************************************************/

#ifndef E_ACSL_BITTREE
#define E_ACSL_BITTREE

#define WORDBITS __WORDSIZE

static size_t mask(size_t, size_t);

#if WORDBITS == 16

static const size_t Tmasks[] = {
0x0,0x8000,0xc000,0xe000,0xf000,0xf800,0xfc00,0xfe00,0xff00,0xff80,0xffc0,
0xffe0,0xfff0,0xfff8,0xfffc,0xfffe,0xffff};

static const int Teq[] = {0,-1,3,-3,6,-5,7,-7,12,-9,11,-11,14,-13,15,16,-16};
static const int Tneq[] = {0,0,1,-2,2,-4,5,-6,4,-8,9,-10,10,-12,13,-14,-15};

#elif WORDBITS == 32

static const size_t Tmasks[] = {
0x0,0x80000000,0xc0000000,0xe0000000,0xf0000000,0xf8000000,0xfc000000,
0xfe000000,0xff000000,0xff800000,0xffc00000,0xffe00000,0xfff00000,0xfff80000,
0xfffc0000,0xfffe0000,0xffff0000,0xffff8000,0xffffc000,0xffffe000,0xfffff000,
0xfffff800,0xfffffc00,0xfffffe00,0xffffff00,0xffffff80,0xffffffc0,0xffffffe0,
0xfffffff0,0xfffffff8,0xfffffffc,0xfffffffe,0xffffffff};

static const int Teq[] =
  { 0,-1,3,-3,6,-5,7,-7,12,-9,11,-11,14,-13,15,-15,24,-17,19,-19,22,
    -21,23,-23,28,-25,27,-27,30,-29,31,32,-32 };

static const int Tneq[] =
  { 0,0,1,-2,2,-4,5,-6,4,-8,9,-10,10,-12,13,-14,8,-16,17,-18,18,-20,21,-22,20,
    -24,25,-26,26,-28,29,-30,-31 };

#else /* WORDBITS == 64 */

static const size_t Tmasks[] = {
0x0,0x8000000000000000,0xc000000000000000,0xe000000000000000,0xf000000000000000,
0xf800000000000000,0xfc00000000000000,0xfe00000000000000,0xff00000000000000,
0xff80000000000000,0xffc0000000000000,0xffe0000000000000,0xfff0000000000000,
0xfff8000000000000,0xfffc000000000000,0xfffe000000000000,0xffff000000000000,
0xffff800000000000,0xffffc00000000000,0xffffe00000000000,0xfffff00000000000,
0xfffff80000000000,0xfffffc0000000000,0xfffffe0000000000,0xffffff0000000000,
0xffffff8000000000,0xffffffc000000000,0xffffffe000000000,0xfffffff000000000,
0xfffffff800000000,0xfffffffc00000000,0xfffffffe00000000,0xffffffff00000000,
0xffffffff80000000,0xffffffffc0000000,0xffffffffe0000000,0xfffffffff0000000,
0xfffffffff8000000,0xfffffffffc000000,0xfffffffffe000000,0xffffffffff000000,
0xffffffffff800000,0xffffffffffc00000,0xffffffffffe00000,0xfffffffffff00000,
0xfffffffffff80000,0xfffffffffffc0000,0xfffffffffffe0000,0xffffffffffff0000,
0xffffffffffff8000,0xffffffffffffc000,0xffffffffffffe000,0xfffffffffffff000,
0xfffffffffffff800,0xfffffffffffffc00,0xfffffffffffffe00,0xffffffffffffff00,
0xffffffffffffff80,0xffffffffffffffc0,0xffffffffffffffe0,0xfffffffffffffff0,
0xfffffffffffffff8,0xfffffffffffffffc,0xfffffffffffffffe,0xffffffffffffffff};

static const int Teq[] =
  { 0,-1,3,-3,6,-5,7,-7,12,-9,11,-11,14,-13,15,-15,24,-17,19,-19,22,-21,23,-23,
    28,-25,27,-27,30,-29,31,-31,48,-33,35,-35,38,-37,39,-39,44,-41,43,-43,46,
    -45,47,-47,56,-49,51,-51,54,-53,55,-55,60,-57,59,-59,62,-61,63,64,-64 };

static const int Tneq[] =
  { 0,0,1,-2,2,-4,5,-6,4,-8,9,-10,10,-12,13,-14,8,-16,17,-18,18,-20,21,-22,20,
    -24,25,-26,26,-28,29,-30,16,-32,33,-34,34,-36,37,-38,36,-40,41,-42,42,-44,
    45,-46,40,-48,49,-50,50,-52,53,-54,52,-56,57,-58,58,-60,61,-62,-63 };

#endif

/*! \brief Root node of the bitree */
static bt_node * bt_root = NULL;

/* common prefix of two addresses */
/*@ assigns \nothing;
  @ ensures \forall int i;
     0 <= i <= WORDBITS
     ==> (Tmasks[i] & a) == (Tmasks[i] & b)
     ==> \result >= Tmasks[i];
  @ ensures (a & \result) == (b & \result);
  @ ensures \exists int i; 0 <= i <= WORDBITS && \result == Tmasks[i];
  @*/
static size_t mask(size_t a, size_t b) {
  size_t nxor = ~(a ^ b), ret;
  int i = WORDBITS/2; /* dichotomic search, starting in the middle */
  /*cpt_mask++;*/

  /* if the current mask matches we use transition from Teq, else from Tneq
     we stop as soon as i is negative, meaning that we found the mask
     a negative element i from Teq or Tneq means stop and return Tmasks[-i] */
  /*@ loop invariant -WORDBITS <= i <= WORDBITS;
    @ loop assigns i;
    @*/
  while(i > 0) {
    //@ assert 0 < i <= WORDBITS;
    //@ assert \valid(Tmasks+i);
    if (nxor >= Tmasks[i])
      //@ assert \valid(Teq+i);
      i = Teq[i];
    else
      //@ assert \valid(Tneq+i);
      i = Tneq[i];
  }

  //@ assert -WORDBITS <= i <= 0;
  ret = Tmasks[-i];
  DASSERT ((a & ret) == (b & ret));
  return ret;
}


/* called from bt_remove */
/* the block we are looking for has to be in the tree */
/*@ requires \valid(ptr);
  @ requires \valid(bt_root);
  @ assigns \nothing;
  @ ensures \valid(\result);
  @ ensures \result->leaf == ptr;
  @*/
static bt_node * bt_get_leaf_from_block (bt_block * ptr) {
  bt_node * curr = bt_root;
  DASSERT(bt_root != NULL);
  DASSERT(ptr != NULL);

  /*@ loop assigns curr;
    @*/
  while(!curr->is_leaf) {
    // the prefix is consistent
    DASSERT((curr->addr & curr->mask) == (ptr->ptr & curr->mask));
    // two children
    DASSERT(curr->left != NULL && curr->right != NULL);
    // the prefix of one child is consistent
    if((curr->right->addr & curr->right->mask)
       == (ptr->ptr & curr->right->mask))
      curr = curr->right;
    else if((curr->left->addr & curr->left->mask)
	    == (ptr->ptr & curr->left->mask))
      curr = curr->left;
    else
      vassert(0, "Unreachable", NULL);
  }
  DASSERT(curr->is_leaf);
  DASSERT(curr->leaf == ptr);
  return curr;
}


/* remove the block from the structure */
/* the block we are looking for has to be in the tree */
/*@ requires \valid(ptr);
  @*/
static void bt_remove (bt_block * ptr) {
  bt_node * leaf_to_delete = bt_get_leaf_from_block (ptr);
  DASSERT(leaf_to_delete->leaf == ptr);

  if(leaf_to_delete->parent == NULL)
    // the leaf is the root
    bt_root = NULL;
  else {
    bt_node * sibling, * parent;
    parent = leaf_to_delete->parent;
    sibling = (leaf_to_delete == parent->left) ? parent->right : parent->left;
    DASSERT(sibling != NULL);
    // copying all sibling's fields into the parent's
    parent->is_leaf = sibling->is_leaf;
    parent->addr = sibling->addr;
    parent->mask = sibling->mask;
    parent->left = sibling->left;
    parent->right = sibling->right;
    parent->leaf = sibling->leaf;
    if(!sibling->is_leaf) {
      sibling->left->parent = parent;
      sibling->right->parent = parent;
    }
    private_free(sibling);
    /* necessary ? -- begin */
    if(parent->parent != NULL) {
      parent->parent->mask = mask(parent->parent->left->addr
				  & parent->parent->left->mask,
				  parent->parent->right->addr
				  & parent->parent->right->mask);
    }
    /* necessary ? -- end */
  }
  private_free(leaf_to_delete);
}


/* called from bt_insert */
/* the returned node will be the sibling of the soon to be added node */
/*@ requires \valid(ptr);
  @ requires \valid(bt_root);
  @ assigns \nothing;
  @ ensures \valid(\result);
  @*/
static bt_node * bt_most_similar_node (bt_block * ptr) {
  bt_node * curr = bt_root;
  size_t left_prefix, right_prefix;
  DASSERT(ptr != NULL);
  DASSERT(bt_root != NULL);

  while(1) {
    if(curr->is_leaf)
      return curr;
    DASSERT(curr->left != NULL && curr->right != NULL);
    left_prefix = mask(curr->left->addr & curr->left->mask, ptr->ptr);
    right_prefix = mask(curr->right->addr & curr->right->mask, ptr->ptr);
    if(left_prefix > right_prefix)
      curr = curr->left;
    else if(right_prefix > left_prefix)
      curr = curr->right;
    else
      return curr;
  }
}

/* add a block in the structure */
/*@ requires \valid(ptr);
  @*/
static void bt_insert (bt_block * ptr) {
  bt_node * new_leaf;
  DASSERT(ptr != NULL);

  new_leaf = private_malloc(sizeof(bt_node));
  DASSERT(new_leaf != NULL);
  new_leaf->is_leaf = 1;
  new_leaf->addr = ptr->ptr;
  new_leaf->mask = Tmasks[WORDBITS]; /* ~0ul */
  new_leaf->left = NULL;
  new_leaf->right = NULL;
  new_leaf->parent = NULL;
  new_leaf->leaf = ptr;

  if(bt_root == NULL)
    bt_root = new_leaf;
  else {
    bt_node * sibling = bt_most_similar_node (ptr), * parent, * aux;

    DASSERT(sibling != NULL);
    parent = private_malloc(sizeof(bt_node));
    DASSERT(parent != NULL);
    parent->is_leaf = 0;
    parent->addr = sibling->addr & new_leaf->addr;
    /*parent->mask = mask(sibling->addr & sibling->mask, ptr->ptr);*/
    parent->leaf = NULL;
    if(new_leaf->addr <= sibling->addr) {
      parent->left = new_leaf;
      parent->right = sibling;
    } else {
      parent->left = sibling;
      parent->right = new_leaf;
    }
    new_leaf->parent = parent;

    if(sibling == bt_root) {
      parent->parent = NULL;
      parent->mask = mask(sibling->addr & sibling->mask, ptr->ptr);
      bt_root = parent;
    } else {
      if (sibling->parent->left == sibling)
        sibling->parent->left = parent;
      else
        sibling->parent->right = parent;
      parent->parent = sibling->parent;

      /* necessary ? -- begin */
      aux = parent;
      aux->mask = mask(aux->left->addr & aux->left->mask,
		       aux->right->addr & aux->right->mask);
      /* necessary ? -- end */
    }
    sibling->parent = parent;
    if(!sibling->is_leaf)
      sibling->mask = mask(sibling->left->addr & sibling->left->mask,
      sibling->right->addr & sibling->right->mask);

    DASSERT((parent->left == sibling && parent->right == new_leaf)
     || (parent->left == new_leaf && parent->right == sibling));
  }
}

/* return the block B such as: begin addr of B == ptr if such a block exists,
   return NULL otherwise */
/*@ assigns \nothing;
  @ ensures \valid(\result);
  @ ensures \result == \null || \result->ptr == (size_t)ptr;
  @*/
static bt_block * bt_lookup (void * ptr) {
  bt_node * tmp = bt_root;
  DASSERT(bt_root != NULL);
  DASSERT(ptr != NULL);

  /*@ loop assigns tmp;
    @*/
  while(!tmp->is_leaf) {
    // if the ptr we are looking for does not share the prefix of tmp
    if((tmp->addr & tmp->mask) != ((size_t)ptr & tmp->mask))
      return NULL;

    // two children
    DASSERT(tmp->left != NULL && tmp->right != NULL);
    // the prefix of one child is consistent
    if((tmp->right->addr & tmp->right->mask)
       == ((size_t)ptr & tmp->right->mask))
      tmp = tmp->right;
    else if((tmp->left->addr & tmp->left->mask)
	    == ((size_t)ptr & tmp->left->mask))
      tmp = tmp->left;
    else
      return NULL;
  }

  if(tmp->leaf->ptr != (size_t)ptr)
    return NULL;
  return tmp->leaf;
}

/* return the block B containing ptr, such as :
   begin addr of B <= ptr < (begin addr + size) of B
   or NULL if such a block does not exist */
static bt_block * bt_find (void * ptr) {
  bt_node * tmp = bt_root;
  if(bt_root == NULL || ptr == NULL)
    return NULL;

  bt_node * other_choice = NULL;

  while(1) {
    if(tmp->is_leaf) {
      /* tmp cannot contain ptr because its begin addr is higher */
      if(tmp->addr > (size_t)ptr)
        return NULL;

      /* tmp->addr <= ptr, tmp may contain ptr
       ptr is contained if tmp is large enough (begin addr + size) */
      else if((size_t)ptr < tmp->leaf->size + tmp->addr
              || (tmp->leaf->size == 0 && (size_t)ptr == tmp->leaf->ptr))
        return tmp->leaf;
      /* tmp->addr <= ptr, but tmp->addr is not large enough */
      else
        return NULL;
    }

    DASSERT(tmp->left != NULL && tmp->right != NULL);

    /* the right child has the highest address, so we test it first */
    if(((size_t)tmp->right->addr & tmp->right->mask)
       <= ((size_t)ptr & tmp->right->mask)) {
      other_choice = tmp->left;
      tmp = tmp->right;
    }
    else if(((size_t)tmp->left->addr & tmp->left->mask)
	    <= ((size_t)ptr & tmp->left->mask))
      tmp = tmp->left;
    else {
      if(other_choice == NULL)
        return NULL;
      else {
        tmp = other_choice;
        other_choice = NULL;
      }
    }
  }
}

/*******************/
/* CLEAN           */
/*******************/
/* erase information about initialization of a block */
static void bt_clean_block_init (bt_block * ptr) {
  if(ptr->init_ptr != NULL) {
    private_free(ptr->init_ptr);
    ptr->init_ptr = NULL;
  }
  ptr->init_bytes = 0;
}

/* erase all information about a block */
static void bt_clean_block (bt_block * ptr) {
  if(ptr) {
    bt_clean_block_init(ptr);
    private_free(ptr);
  }
}

/* called from bt_clean */
/* recursively erase the content of the structure */
static void bt_clean_rec (bt_node * ptr) {
  if(ptr == NULL) return;
  else if(ptr->is_leaf) {
    bt_clean_block(ptr->leaf);
    ptr->leaf = NULL;
  }
  else {
    bt_clean_rec(ptr->left);
    bt_clean_rec(ptr->right);
    ptr->left = ptr->right = NULL;
  }
  private_free(ptr);
}

/* erase the content of the structure */
static void bt_clean () {
  bt_clean_rec(bt_root);
  bt_root = NULL;
}

/*********************/
/* DEBUG             */
/*********************/
#ifdef E_ACSL_DEBUG
static void bt_print_block(bt_block * ptr) {
  if (ptr != NULL) {
    DLOG("%a; %lu Bytes; %slitteral; [init] : %d ",
      (char*)ptr->ptr, ptr->size,
      ptr->is_readonly ? "" : "not ", ptr->init_bytes);
    if(ptr->init_ptr != NULL) {
      unsigned i;
      for(i = 0; i < ptr->size/8; i++)
        DLOG("%b ", ptr->init_ptr[i]);
    }
    DLOG("\n");
  }
}

static void bt_print_node(bt_node * ptr, int depth) {
  int i;
  if(ptr == NULL)
    return;
  for(i = 0; i < depth; i++)
    DLOG("  ");
  if(ptr->is_leaf)
    bt_print_block(ptr->leaf);
  else {
    DLOG("%p -- %p\n", (void*)ptr->mask, (void*)ptr->addr);
    bt_print_node(ptr->left, depth+1);
    bt_print_node(ptr->right, depth+1);
  }
}

static void bt_print_tree() {
  DLOG("------------DEBUG\n");
  bt_print_node(bt_root, 0);
  DLOG("-----------------\n");
}
#endif
#endif
