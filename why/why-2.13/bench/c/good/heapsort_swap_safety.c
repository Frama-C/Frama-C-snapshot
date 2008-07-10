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

/* Heapsort (safety only) */

/*** arith *******************************************************************/

//@ axiom div2_1 : \forall unsigned int i; 0 <= i => 0 <= i/2 <= i

//@ axiom div2_2 : \forall unsigned int i; 0 < i => 0 <= i/2 < i

// @ axiom div2_3 : \forall int i; 0 <= i => 2*(i/2) <= i

/*****************************************************************************/

/*@ requires \valid(a+i) && \valid(a+j)
  @*/
void swap(int* a, unsigned int i, unsigned int j) {
  int tmp = a[i];
  a[i] = a[j];
  a[j] = tmp;
}

/*@ requires 
  @   0 <= low <= high && 2*high <= \max_range(unsigned int) 
  @   && \valid_range(a, low, high)
  @*/
void sift_down(int* a, unsigned int low, unsigned int high) {
  unsigned int i = low, child;
  /*@ invariant 
    @   low <= i <= high
    @ variant 
    @   high - i
    @*/
  while ((child = 2*i+1) <= high) {
    if (child+1 <= high && a[child+1] >= a[child]) child++;
    if (a[i] >= a[child]) break;
    swap(a, i, child);
    i = child;
  }
}

/*@ requires 
  @   0 <= n  && 2*(n-1) <= \max_range(unsigned int) && \valid_range(a, 0, n-1)
  @*/
void heapsort(int* a, unsigned int n) {
  unsigned int i;
  if (n <= 1) return;
  /*@ invariant 
    @   0 <= i < n
    @ variant 
    @   i
    @*/
  for (i = n/2; i >= 1; i--) sift_down(a, i-1, n-1);
  /*@ invariant 
    @   0 <= i < n
    @ variant 
    @   i
    @*/
  for (i = n-1; i >= 1; i--) { swap(a, 0, i); sift_down(a, 0, i-1); }
}

/*
Local Variables: 
compile-command: "make heapsort_swap_safety.overflows"
End: 
*/
