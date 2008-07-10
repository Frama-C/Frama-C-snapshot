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

typedef struct struct_list {
  int hd;
  struct struct_list * tl;
} *list;

#define NULL ((void*)0)

/*@ predicate is_list(list p) reads p->tl */

/*@ axiom is_list_def : 
     \forall list p; 
     is_list(p) <=> (p == NULL || (\valid(p) && is_list(p->tl))) */

/*@ logic int length(list p) reads p->tl */

/* @ axiom length_null : length((list)NULL) == 0 */

/*@ axiom length_nonneg : \forall list p; is_list(p) => length(p) >= 0 */

/*@ axiom length_cons : 
     \forall list p; 
     is_list(p) => p != NULL => length(p) == length(p->tl) + 1 */

/*@ requires is_list(p)
  @ ensures  \result != NULL => \result->hd == v
  @*/
list search(list p, int v) {
  /*@ invariant is_list(p)
      variant   length(p) */
  while (p != NULL && p->hd != v) p = p->tl;
  return p;
}
