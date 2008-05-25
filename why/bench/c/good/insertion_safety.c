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

/* Insertion sort (safety only; for correctness proof, see insertion.c) */

/*@ requires 
  @   0 <= n && \valid_range(a, 0, n-1)
  @*/
void insertion_sort(int* a, unsigned int n) {
  unsigned int i;
  if (n <= 1) return;
  /*@ invariant
    @   0 < i <= n
    @ variant
    @   n - i
    @*/
  for (i = 1; i < n; i++) {
    int v = a[i];
    unsigned int j = i;
    /*@ invariant
      @   0 <= j <= i
      @ variant
      @   j
      @*/
    while (j > 0 && a[j-1] > v) { a[j] = a[j-1]; j--; }
    a[j] = v;
  }
}


/*
Local Variables: 
compile-command: "make insertion_safety.overflows"
End: 
*/
