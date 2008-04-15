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
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2, with the special exception on linking              */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

/* Skip lists. */

#if 0
#include <stdlib.h>
#else
void* malloc(int);
float drand48();
#endif

#define MaxNbLevels 16
#define MaxLevel 15

typedef struct node_struct *node;

struct node_struct {
  int elt;
  node forward[];
};

typedef struct list_struct {
  int level;
  node header;
} *list;

node NIL;

/*@ assigns \nothing
  @ ensures \valid(\result) && \block_length(\result->forward) == lvl */
node make_node(int lvl) {
  return (node)malloc(sizeof(struct node_struct) + lvl * sizeof(node));
}

/*@ ensures \valid(NIL) && \block_length(NIL->forward) == MaxNbLevels */
void init() {
  NIL = make_node(MaxNbLevels);
  NIL->elt = 0x7fffffff;
}

//@ type int_list
/*@ logic int_list nil() */
/*@ logic int_list cons(int x, int_list l) */
/*@ predicate mem(int x, int_list l) */

/*@ predicate elements(list l, int_list il) 
      reads l->header,l->header->forward[..] */

/*@ ensures \valid(\result) && elements(\result,nil()) */
list empty() {
  list l;
  int i;
  l = (list)malloc(sizeof(struct list_struct));
  l->level = 0;
  l->header = make_node(MaxNbLevels);
  for (i = 0; i < MaxNbLevels; i++)
    l->header->forward[i] = NIL;
  return l;
}

/*@ assigns \nothing ensures 0.0 <= \result <= 1.0 */
float rand01();

/*@ assigns \nothing ensures 1 <= \result <= MaxNbLevels */
int random_level() {
  static float prob = 0.25;
  int lvl = 1;
  while (rand01() < prob && lvl < MaxNbLevels) 
    lvl++;
  return lvl;
}

/*@ requires \valid(l) 
  @  // ensures \result == 0 
  @*/
char search(list l, int v) {
  node p = l->header;
  node q;
  int k = l->level;
  do {
    while (q = p->forward[k], q->elt < v) p = q;
  } while (--k >= 0);
  if (p->elt != v) return 0;
  return 1;
}

/*@ requires \valid(l) 
  @ // ensures ???? 
  @*/
void insert(list l, int v) {
  node update[MaxNbLevels];
  node p = l->header;
  node q;
  int k = l->level;
  do {
    while (q = p->forward[k], q->elt < v) p = q;
    update[k] = p;
  } while (--k >= 0);
  if (p->elt == v) return;
  k = random_level();
  if (k > l->level) {
    k = ++l->level;
    update[k] = l->header;
  }
  q = make_node(k);
  q->elt = v;
  do {
    p = update[k];
    q->forward[k] = p->forward[k];
    p->forward[k] = q;
  } while (--k >= 0);
}

/* test */
#if 0
#include <stdio.h>

void print(list l) {
  node x = l->header->forward[0];
  while (x != NIL) {
    printf("%d :: ", x->elt);
    x = x->forward[0];
  }
  printf("nil\n");
}

float rand01() { return drand48(); }

int main() {
  list l;

  init ();
  l = empty();
  print(l);
  insert(l, 1);
  print(l);
  insert(l, 7);
  print(l);
  insert(l, 3);
  print(l);
  return 0;
}

#endif

