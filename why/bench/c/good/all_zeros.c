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

/* check that t[0..n-1] only contains 0 */

/*@ requires n >= 0 && \valid_range(t,0,n) 
    ensures \result <=> \forall int i; 0<=i<n => t[i]==0 */
int all_zeros(int t[], int n) {
  /*@ invariant n <= \old(n) && \forall int i; n<=i<\old(n) => t[i]==0
      variant n */
  while (--n>=0 && !t[n]);
  return n < 0;
}

/*@ requires n >= 0 && \valid_range(t,0,n) 
    ensures \result <=> \forall int i; 0<=i<n => t[i]==0 */
int all_zeros_0(int t[], int n) {
  int k;
  /*@ invariant 0 <= k <= n && \forall int i; 0<=i<k => t[i]==0 variant n-k */
  for (k = 0; k < n; k++) if (t[k]) return 0;
  return 1;
}
