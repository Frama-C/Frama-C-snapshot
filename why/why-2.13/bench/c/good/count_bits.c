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

// the number of bits 1 in x
//@ logic int nbits(int x)

//@ axiom nbits_nonneg : \forall int x; nbits(x) >= 0

//@ axiom nbits_zero : nbits(0) == 0

/*@ axiom lowest_bit_zero :
  @   \forall int x; (x&-x) == 0 <=> x == 0
  @*/

/*@ axiom remove_one_bit :
  @    \forall int x; 
  @       x != 0 => nbits(x - (x&-x)) == nbits(x) - 1
  @*/

/*@ ensures \result == nbits(x) */
int count_bits(int x) {
  int d, c;
  /*@ invariant c + nbits(x) == nbits(\at(x,init))
    @ variant   nbits(x)
    @*/
  for (c = 0; d = x&-x; x -= d) c++;
  return c;
}
