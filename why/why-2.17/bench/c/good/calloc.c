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


/*@ requires 
  @   n >= 1 
  @ ensures 
  @   \valid_range(\result,0,n-1) && 
  @   \forall int i; 0<=i<n => \valid(\result[i]) */
int** test(int n) { 
  int** t = (int**)calloc(n,sizeof(int*));
  int i;
  /*@ invariant 
    @   0 <= i <= n && \forall int k; 0<=k<i => \valid(t[k]) */
  for (i = 0; i < n; i++)
    t[i] = (int*)calloc(1,sizeof(int));
  return t;
}

//@ ensures \result == 0
int main() {
  int** t = test(10);
  t[3][0] = 0;
  // t[4][0] = 1;
  return t[3][0];
}
