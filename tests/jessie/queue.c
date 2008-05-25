/**************************************************************************/
/*                                                                        */
/*  The Why/Caduceus/Krakatoa tool suite for program certification        */
/*  Copyright (C) 2002-2006                                               */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
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

typedef struct queue {
  char *contents;
  int length;
  int first, last;
  unsigned int empty, full :1;
} queue;

char t[] = { 0, 0, 0, 0, 0 } ;

queue q = { t, 5, 0, 0, 0, 1};

/*@ global invariant q_invariant :
  @   \valid_range(q.contents, 0, q.length-1) &&
  @   0 <= q.first < q.length &&
  @   0 <= q.last < q.length ;
  @   // && (q.full != 0 <=> q.last == q.first)
  @*/

/*@ requires !q.full;
  @ assigns  q.empty, q.full, q.last, q.contents[q.last];
  @ ensures  !q.empty && q.contents[\old(q.last)] == c;
  @*/
void push(char c) {
  q.contents[q.last++] = c;
  if (q.last == q.length) q.last = 0;
  q.empty = 0;
  q.full = q.first == q.last;
}

/* q.last = (q.last + 1) % q.length; BUG */

/*@ requires !q.empty;
  @ assigns q.empty, q.full, q.first;
  @ ensures !q.full && \result == q.contents[\old(q.first)];
  @*/
char pop() {
  char r = q.contents[q.first++];
  if (q.first == q.length) q.first = 0;
  q.full = 0;
  q.empty = q.first == q.last;
  return r;
}

/*@ requires \valid(q1) && !q.empty;
  @ ensures \result == \old(q1->empty) ;
  @*/
int test(struct queue *q1) {
  char c = pop();
  return q1->empty;
}

/*
Local Variables:
compile-command: "LC_ALL=C make queue"
End:
*/
