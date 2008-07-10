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

//@ logic int num_of_pos(int i,int j,int t[]) reads t[i]

/*@ axiom num_of_pos_empty :
  @   \forall int i, int j, int t[];
  @       i > j => num_of_pos(i,j,t) == 0
  @*/
 
/*@ axiom num_of_pos_true_case :
  @   \forall int i, int j, int t[];
  @       i <= j && t[j] > 0 => 
  @         num_of_pos(i,j,t) == num_of_pos(i,j-1,t) + 1
  @*/

/*@ axiom num_of_pos_false_case :
  @   \forall int i, int j, int t[];
  @       i <= j && ! (t[j] > 0) => 
  @         num_of_pos(i,j,t) == num_of_pos(i,j-1,t)
  @*/

/*@ axiom num_of_pos_strictly_increasing:
  @   \forall int i, int j, int k, int l, int t[];
  @       j < k && k <= l && t[k] > 0 => num_of_pos(i,j,t) < num_of_pos(i,l,t)
  @*/

/*@ requires length >= 0 && \valid_range(t,0,length-1)
  @*/
void m(int t[], int length) {
  int count = 0;
  int i;
  int *u;

  /*@ invariant
    @    0 <= i && i <= length && 
    @    0 <= count && count <= i && 
    @    count == num_of_pos(0,i-1,t)  
    @ variant length - i
    @*/
  for (i=0 ; i < length; i++) {
    if (t[i] > 0) count++;
  }
  u = (int *)calloc(count,sizeof(int));
  count = 0;
  
  /*@ invariant
    @    0 <= i && i <= length && 
    @    0 <= count && count <= i && 
    @    count == num_of_pos(0,i-1,t)
    @ variant length - i
    @*/
  for (i=0 ; i < length; i++) {
    if (t[i] > 0) {
      u[count++] = t[i];
    }
  }
}

/*
Local Variables: 
compile-command: "make muller.nosep"
End: 
*/
