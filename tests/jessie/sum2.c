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

/*@ axiomatic Sum {
  @   // sum(t,i,j) denotes t[i]+...+t[j-1] 
  @   logic integer sum{L}(int t[], integer i, integer j);
  @        // reads i,j,t,t[..] ;
  @   axiom sum1{L} :
  @     \forall int t[], integer i; sum(t,i,i) == 0;
  @   axiom sum2{L} :
  @     \forall int t[], integer i, j; 
  @       sum(t,i,j+1) == sum(t,i,j) + t[j]; 
  @   axiom sum3{L} :
  @     \forall int t[], integer i, j, k;
  @       i <= j <= k ==>
  @         sum(t,i,k) == sum(t,i,j) + sum(t,j,k); 
  @ }
  @*/

/*@ requires n >= 1 && \valid_range(t,0,n-1) ;
  @ ensures \result == sum(t,0,n);
  @*/
int test1(int t[],int n) {
  int i,s = 0;

  /*@ loop invariant 0 <= i <= n && s == sum(t,0,i);
    @ loop variant n-i;
  */
  for(i=0; i < n; i++)
  {
    s += t[i];
  }
  return s;
}


/*@ requires \valid_range(t,0,n-1);
  @ assigns t[..];
  @ ensures sum(t,0,n) == \old(sum(t,0,n))+n;
  @*/
void test2(int t[],int n) {
  int i;
  /*@ loop invariant 0 <= i <= n &&
    @     sum(t,0,n) == \at(sum(t,0,n),Pre)+i;
    @ loop variant n-i;
    @*/
  for(i=0; i < n; i++)
  {
    t[i] += 1;
  }
}

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make sum2"
End:
*/

