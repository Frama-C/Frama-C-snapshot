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

/* ATTENTION ! on veut sum(t,i,j) = t[i]+...+t[j-1] */

/*@ logic int sum(int t[], int i, int j) 
  reads i,j,t,t[..] 
*/

/*@ axiom sum1 : 
      \forall int t[], int i; sum(t,i,i) == 0 */

/*@ axiom sum2 : 
      \forall int t[], int i, int j; sum(t,i,j+1) == sum(t,i,j) + t[j] */

/*@ axiom sum3 : 
      \forall int t[], int i, int j, int k; 
         i <= j && j <= k =>
          sum(t,i,k) == sum(t,i,j) + sum(t,j,k) */

/*@ requires n >= 1 && \valid_range(t,0,n-1) 
  @ ensures \result == sum(t,0,n)
*/  
int test1(int t[],int n) {
  int i,s = 0;

  /*@invariant 0 <= i <= n && s == sum(t,0,i)
    @ variant n-i
  */
  for(i=0; i < n; i++) 
  {
    s += t[i];
  }
  return s;
}


/*@ requires \valid_range(t,0,n-1)
  @ assigns t[..]
  @ ensures sum(t,0,n) == \old(sum(t,0,n))+n
  @*/
void test2(int t[],int n) {
  int i;
  //@ label L 
  /*@ invariant 0 <= i <= n && 
         sum(t,0,n) == \at(sum(t,0,n),L)+i
    @ variant n-i
    @*/
  for(i=0; i < n; i++) 
  {
    t[i] += 1;
  }
}
