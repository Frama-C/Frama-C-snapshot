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

/*@ requires n >= 0 && \valid_range(t,0,n-1)
  @ assigns t[0..n-1]
  @ ensures \forall int k; 0 <= k < n => t[k] == -\old(t[k])
  @*/
void negate(int *t, int n) {
  int i = 0;
  /*@ invariant 
    @   0 <= i <= n && 
    @   (\forall int k; 0 <= k < i => t[k] == -\old(t[k])) 
    @ loop_assigns t[0..i-1]
    @ variant n-i */
  while (i < n) {
    t[i] = -t[i];
    i++;
  }
}
