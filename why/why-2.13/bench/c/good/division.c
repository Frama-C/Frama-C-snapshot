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

//@ axiom distr_right: \forall int x, int y, int z; x*(y+z) == (x*y)+(x*z)
//@ axiom distr_left: \forall int x, int y, int z; (x+y)*z == (x*z)+(y*z)

/*@ requires x >= 0 && y > 0
  @ ensures  \exists int r; x == \result * y + r && 0 <= r < y
  @*/
int division(int x, int y) {
  int i = 0, j = x;
  /*@ invariant x == i * y + j && 0 <= j
    @ variant   j
    @*/
  while (j >= y) {
    i++;
    j -= y;
  }
  return i;
}

