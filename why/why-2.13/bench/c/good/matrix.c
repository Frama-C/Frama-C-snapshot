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

/* matrices */

/* initialize a matrix a[n][m] with a[i][j] = i+j */

/*@ predicate separated(int *a1, int n1, int *a2, int n2) {
  @   \base_addr(a1) != \base_addr(a2) ||
  @   a1 + n1 <= a2 ||
  @   a2 + n2 <= a1
  @ }
  @*/

/*@ predicate is_matrix(int **a, int n, int m) { 
  @   \valid_range(a, 0, n-1) &&
  @   (\forall int i; 0 <= i < n => \valid_range(a[i], 0, m-1)) &&
  @   (\forall int i, int j; 0 <= i < n => 0 <= j < n => i != j =>
  @      separated(a[i], m, a[j], m))
  @ }
  @*/

/*@ requires 0 <= n && 0 <= m && is_matrix(a,n,m)
  @ ensures  \forall int i, int j; 0 <= i < n => 0 <= j < m => a[i][j] == i+j
  @*/
void initialize(int **a, int n, int m) {
  int i, j;
  /*@ invariant 
    @   0 <= i <= n &&
    @   \forall int i0; 0 <= i0 < i => 
    @     \forall int j; 0 <= j < m => a[i0][j] == i0+j
    @ loop_assigns a[..][..]
    @*/
  for (i = 0; i < n; i++)
    /*@ invariant 
      @  0 <= j <= m &&
      @  \forall int i0, int j0; 
      @    ((0 <= i0 < i && 0 <= j0 < m) || (i0 == i && 0 <= j0 < j)) => 
      @    a[i0][j0] == i0+j0
      @ loop_assigns a[i][..]
      @*/
    for (j = 0; j < m; j++)
      a[i][j] = i+j;
}

/*@ requires 0 <= n && 0 <= m && is_matrix(a,n,m)
  @ ensures  \forall int i, int j; 0 <= i < n => 0 <= j < m => a[i][j] == i+j
  @*/
void initialize2(int **a, int n, int m) {
  int i, j;
  /*@ invariant 
    @   0 <= j <= m &&
    @   \forall int j0; 0 <= j0 < j => 
    @     \forall int i; 0 <= i < n => a[i][j0] == i+j0
    @ // loop_assigns a[..][..]
    @*/
  for (j = 0; j < m; j++)
    /*@ invariant 
      @  0 <= i <= n &&
      @  \forall int i0, int j0; 
      @    ((0 <= j0 < j && 0 <= i0 < n) || (j0 == j && 0 <= i0 < i)) => 
      @    a[i0][j0] == i0+j0
      @ // loop_assigns a[..][j]
      @*/
    for (i = 0; i < n; i++)
      a[i][j] = i+j;
}


/*@ requires 
  @   0 <= n && 0 <= m
  @ ensures  
  @   is_matrix(\result, n, m) &&
  @   \fresh(\result) &&
  @   \forall int i; 0 <= i < n => \fresh(\result[i])
  @*/
int ** alloc_matrix(int n, int m);
  
//@ requires 0 <= n && 0 <= m
void main(int n, int m) {
  int ** a = alloc_matrix(n, m);
  initialize(a, n, m);
}
