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

typedef struct T {
  int x;
  int y;
} T;

/*@ requires \valid(t2) && t2->x == 0
  @ assigns t2->x
  @ ensures \result == 1 && t2->x == 2 && t2->y == \old(t2->y)
  @*/
int f(T* t2) {
  t2->x++; 
  return t2->x++;
}

struct S { int z; T t; } s;
struct S *ps;

struct S *pps[] = { (void *)0 };

/*@ requires \valid(ps)
  @ ensures \result == 1
  @*/
int g() {
  T *p;
  ps = &s;
  pps[0] = ps;
  p = &(s.t);
  ps->t.x = 1;
  return s.t.x;
}
