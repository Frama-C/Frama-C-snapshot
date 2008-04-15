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

/* checks that t[0..n-1] contain only zeros */

/*@ requires \valid_range(t,0,n) && n >= 0;
    ensures \result <==> \forall integer i; 0<=i<n ==> t[i]==0; */
int all_zeros(int t[], int n) {
  /*@ loop invariant 0 <= n <= \at(n,Pre) && \forall int i; n<=i<\at(n,Pre) ==> t[i]==0;
      loop variant n; */
  while (--n>=0 && !t[n]);
  return n < 0;
}

/*@ requires \valid_range(t,0,n) && n >= 0;
  ensures \result <==> \forall integer i; 0<=i<n ==> t[i]==0; */
int all_zeros_1(int t[], int n) {
  int k = 0;
  /*@ loop invariant 0 <= k <= n && \forall integer i; 0<=i<k ==> t[i]==0;
    @ loop variant n-k;
   */
  while (k<n && !t[k++]);
  return k == n;
}

/*@ requires \valid_range(t,0,n) && n >= 0;
    ensures \result <==> \forall integer i; 0<=i<n ==> t[i]==0; */
int all_zeros_0(int t[], int n) {
  int k;
  /*@ loop invariant 0 <= k <= n && \forall integer i; 0<=i<k ==> t[i]==0;
    @ loop variant n-k;
   */
  for (k = 0; k < n; k++) if (t[k]) return 0;
  return 1;
}

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make all_zeros"
End:
*/
