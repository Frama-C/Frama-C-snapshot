/**************************************************************************/
/*                                                                        */
/*  The Why platform for program certification                            */
/*  Copyright (C) 2002-2008                                               */
/*    Romain BARDOU                                                       */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
/*    Christine PAULIN                                                    */
/*    Yann RÉGIS-GIANAS                                                   */
/*    Nicolas ROUSSET                                                     */
/*    Xavier URBAIN                                                       */
/*                                                                        */
/*  This software is free software; you can redistribute it and/or        */
/*  modify it under the terms of the GNU General Public                   */
/*  License version 2, as published by the Free Software Foundation.      */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/*  See the GNU General Public License version 2 for more details         */
/*  (enclosed in the file GPL).                                           */
/*                                                                        */
/**************************************************************************/

/* Priority queues */

/**** misc ************************************************************/

//@ axiom div2_1: \forall int x; 0 <= x ==> 0 <= x/2 <= x;

/**** bags ************************************************************/

//@ type bag;

//@ logic bag empty_bag();

//@ logic bag singleton_bag(integer x);

//@ logic bag union_bag(bag b1, bag b2);

//@ logic bag add_bag(integer x, bag b) = union_bag(b, singleton_bag(x)) ;

//@ logic integer occ_bag(integer x, bag b);

/*@ predicate is_max_bag(bag b, integer m) =
  @   occ_bag(m, b) >= 1 &&
  @   \forall integer x; occ_bag(x,b) >= 1 ==> x <= m
  @ ;
  @*/

/**** trees ************************************************************/

//@ type tree;

//@ logic tree Empty();

//@ logic tree Node(tree l, integer x, tree r);

//@ logic bag bag_of_tree(tree t);

/*@ axiom bag_of_tree_def_1:
  @   bag_of_tree(Empty()) == empty_bag();
  @*/

/*@ axiom bag_of_tree_def_2:
  @   \forall tree l; \forall int x; \forall tree r;
  @     bag_of_tree(Node(l, x, r)) ==
  @     add_bag(x, union_bag(bag_of_tree(l), bag_of_tree(r)));
  @*/

/*** heap property *******************************************************/

//@ predicate is_heap(tree t);

//@ axiom is_heap_def_1: is_heap(Empty());

//@ axiom is_heap_def_2: \forall integer x; is_heap(Node(Empty(), x, Empty()));

/*@ axiom is_heap_def_3:
  @   \forall tree ll; \forall int lx; \forall tree lr; \forall int x;
  @     x >= lx ==> is_heap(Node(ll, lx, lr)) ==>
  @     is_heap(Node(Node(ll, lx, lr), x, Empty()));
  @*/

/*@ axiom is_heap_def_4:
  @   \forall tree rl; \forall int rx; \forall tree rr; \forall int x;
  @     x >= rx ==> is_heap(Node(rl, rx, rr)) ==>
  @     is_heap(Node(Empty(), x, Node(rl, rx, rr)));
  @*/

/*@ axiom is_heap_def_5:
  @   \forall tree ll; \forall int lx; \forall tree lr;
  @   \forall int x;
  @   \forall tree rl; \forall int rx; \forall tree rr;
  @     x >= lx ==> is_heap(Node(ll, lx, lr)) ==>
  @     x >= rx ==> is_heap(Node(rl, rx, rr)) ==>
  @     is_heap(Node(Node(ll, lx, lr), x, Node(rl, rx, rr)));
  @*/

/**** trees encoded in arrays *********************************************/

//@ logic tree tree_of_array{L}(int *t, integer root, integer bound) reads t[..];

/*@ axiom tree_of_array_def_1{L}:
  @   \forall int *t; \forall integer root; \forall integer bound;
  @     root >= bound ==> tree_of_array(t, root, bound) == Empty();
  @*/

/*@ axiom tree_of_array_def_2{L}:
  @   \forall int *t; \forall integer root; \forall integer bound;
  @     0 <= root < bound ==>
  @     tree_of_array(t, root, bound) ==
  @     Node(tree_of_array(t, 2*root+1, bound),
  @          t[root],
  @          tree_of_array(t, 2*root+2, bound));
  @*/

/**** the heap and its model **********************************************/

#define MAXSIZE 100

int heap[MAXSIZE];

int size = 0;

/*@ global invariant size_inv : 0 <= size < MAXSIZE; */

//@ global invariant is_heap: is_heap(tree_of_array(heap, 0, size));

//@ logic bag model{L}() = bag_of_tree(tree_of_array(heap, 0, size));

/**** the code ************************************************************/

/*@ assigns
  @   size;
  @ ensures
  @   model() == empty_bag();
  @*/
void clear() {
  size = 0;
}

/*@ requires
  @   size < MAXSIZE-1;
  @ assigns
  @    heap[..], size;
  @ ensures
  @   model() == add_bag(x, \old(model()));
  @*/
void push(int x) {
  int i = size;
  /*@ loop invariant
    @   0 <= i <= size &&
    @   (i == size ==>
    @      is_heap(tree_of_array(heap, 0, size)) &&
    @      model() == model{Pre}()) &&
    @   (i < size ==>
    @      is_heap(tree_of_array(heap, 0, size+1)) &&
    @      bag_of_tree(tree_of_array(heap, 0, size+1)) ==
    @      add_bag(heap[i], \at(model(),Pre)));
    @ loop assigns
    @   heap[..];
    @ loop variant
    @   i;
    @*/
  while (i > 0) {
    int parent = (i-1) / 2;
    int p = heap[parent];
    if (p >= x) break;
    heap[i] = p;
    i = parent;
  }
  heap[i] = x;
  size++;
}

/*@ requires
  @   size > 0;
  @ assigns
  @   \nothing;
  @ ensures
  @   is_max_bag(model(), \result);
  @*/
int max() {
  return heap[0];
}

/*@ requires
  @   size > 0;
  @ assigns
  @    heap[..], size;
  @ ensures
  @   size == \old(size) -  1 &&
  @   is_max_bag(\old(model()), \result) &&
  @   \old(model()) == add_bag(\result, model());
  @*/
int pop() {
  int res = heap[0];
  if (--size) {
    int v = heap[size]; // value to insert
    int i = 0;          // candidate position
    /*@ loop invariant
      @   0 <= i <= size; // TODO: complete invariant
      @ loop assigns
      @   heap[..];
      @ loop variant
      @   size - i;
      @*/
    while (i < size) {
      int j = 2*i+1;
      if (j < size-1 && heap[j] < heap[j+1]) j++;
      if (v >= heap[j]) break;
      heap[i] = heap[j];
      i = j;
    }
    heap[i] = v;
  }
  return res;
}

/*
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make heap"
End:
*/
