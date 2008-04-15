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

/**************************************************************************/
/*                                                                        */
/* Proof of the Bresenham line drawing algorithm.                         */
/* (see examples/bresenham/ for the Why proof)                            */
/*                                                                        */
/* Jean-Christophe Filliâtre (LRI, Université Paris Sud)                  */
/* June 2008                                                              */
/*                                                                        */
/**************************************************************************/

int x2, y2;

//@ invariant first_octant : 0 <= y2 <= x2

//@ logic int abs(int x)

/*@ axiom abs_def: 
  @   \forall int x; (x >= 0 && abs(x) == x) || (x <= 0 && abs(x) == -x)
  @*/

/*@ predicate best(int x, int y) {
  @   \forall int yp; abs(x2 * y - x * y2) <= abs (x2 * yp - x * y2)
  @ } */

/*@ predicate Invariant(int x, int y, int e) {
  @   e == 2 * (x + 1) * y2 - (2 * y + 1) * x2 &&
  @   2 * (y2 - x2) <= e <= 2 * y2
  @ } */

/*@ axiom invariant_is_ok : 
  @  \forall int x, int y, int e; Invariant(x,y,e) => best(x,y)
  @*/

//@ axiom z_ring_0 : \forall int a, int b, int c; a * (b+c) == a*b + a*c
//@ axiom z_ring_1 : \forall int a, int b, int c; (b+c) * a == b*a + c*a

void bresenham() {
  int x = 0;
  int y = 0;
  int e = 2 * y2 - x2;
  /*@ invariant 0 <= x <= x2 + 1 && Invariant(x,y,e)
    @ variant x2 - x
    @*/
  for (x = 0; x <= x2; x++) {
    // plot (x,y) at this point
    //@ assert best(x,y)
    if (e < 0)
      e += 2 * y2;
    else {
      y++;
      e += 2 * (y2 - x2);
    }
  }
}

/*
Local Variables: 
compile-command: "make bresenham.gui"
End: 
*/
