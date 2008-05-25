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

/*@ requires
  @   n >= 1 ;
  @ ensures
  @   \valid_range(\result,0,n-1) &&
  @   \forall int i; 0<=i<n ==> \valid(\result[i]); */
int** test(int n) {
  int** t = (int**)malloc(n * sizeof(int*));
  int i;
  /*@ loop invariant
    @   0 <= i <= n && \forall int k; 0<=k<i ==> \valid(t[k]); */
  for (i = 0; i < n; i++)
    t[i] = (int*)malloc(sizeof(int));
  return t;
}

//@ ensures \result == 0;
int main() {
  int** t = test(10);
  t[3][0] = 0;
  // t[4][0] = 1;
  return t[3][0];
}

/* 
Local Variables:
compile-command: "LC_ALL=C make malloc"
End:
*/
