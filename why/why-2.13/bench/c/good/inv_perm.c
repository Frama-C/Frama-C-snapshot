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

/* Program I: Inverse of a permutation, in place.
   The Art of Computer Programming, vol. 1, page 176.*/

/*@ predicate permutation(int *t, int n) {
  @   \forall int i; 1 <= i <= n => \exists int j; 1 <= j <= n && t[j] == i 
  @ } */

/*@ axiom permut_domain :
  @  \forall int *t, int n; permutation(t,n) =>
  @    \forall int k; 1 <= k <= n => 1 <= t[k] <= n
  @*/

/*@ axiom permut_involution : 
  @  \forall int *t, int n; permutation(t,n) =>
  @    \forall int k; 1 <= k <= n => t[t[k]] == k
  @*/

// cycle-free path from k1 to k2 in t

//@ predicate path(int *t, int k1, int k2) reads t[..]

//@ axiom path_nil : \forall int *t, int k; path(t, k, k)

/*@ axiom path_cons : 
  @   \forall int *t, int k0, int k; 
  @     path(t, k0, k) => t[k] != k0 => path(t, k0, t[k])
  @*/

/*@ predicate path3(int *t, int ml, int k, int m) {
  @   path(t, ml, k) && t[k] != m && path(t, k, m)
  @ }
  @*/

// largest element in the cycle of k

//@ logic int largest(int *t, int k) reads t[..]

/*@ axiom largest_domain : 
  @   \forall int *t, int n; permutation(t, n) => 
  @     \forall int k; 1 <= largest(t, k) <= n
  @*/

/*@ requires 
  @   n >= 1 && \valid_range(t,1,n) && permutation(t, n)
  @ ensures
  @   \forall int k; 1 <= k <= n => t[\old(t[k])] == k
  @*/
void inverse(int *t, int n) {
  int m = n, j = -1;
  //@ label init
  /*@ invariant 
    @   1 <= m <= n && j < 0 &&
    @   (\forall int k; 1 <= k <= n => 
    @      (\at(largest(t, k), init) > m &&
    @        // cycle done
    @        (\forall int tk; t[k] == tk =>
    @          (m <  k &&  1 <= tk <= n  && \at(t[tk],  init) == k) ||
    @          (k <= m && -n <= tk <= -1 && \at(t[-tk], init) == k)))
    @   ||
    @      (\at(largest(t, k), init) <= m && t[k] == \at(t[k], init)))
    @ variant 
    @   m
    @*/
  do {
    int i = t[m];
    //@ label L
    if (i > 0) {
      /*@ invariant 
	@   1 <= m <= n && i == t[m] && j < 0 && 
	@   (m < \at(m,L) => 
	@     \forall int cj; cj == -j => \at(t[cj], init) == m) &&
	@   (\forall int mc; mc == m => \at(path(t, \at(m, L), mc), init)) &&
	@   (\forall int k; 1 <= k <= n => 
	@      (\at(largest(t, k), init) > m &&
	@        // cycle done
	@        (\forall int tk; t[k] == tk =>
	@          (m <  k &&  1 <= tk <= n  && \at(t[tk],  init) == k) ||
	@          (k <= m && -n <= tk <= -1 && \at(t[-tk], init) == k)))
	@   ||
	@      (\at(largest(t, k), init) < m && t[k] == \at(t[k], init))
	@   ||
	@      (\at(largest(t, k), init) == m && 
	@         (  (\at(path3(t, \at(m,L), k, m), init) && 
	@             (\forall int tk; t[k] == tk => 
	@                -n <= tk <= -1 && \at(t[-tk], init) == k))
	@         || (\at(path3(t, m, k, \at(m,L)), init) && 
	@             t[k] == \at(t[k], init)))))
	@ // variant
	@ // todo  
	@*/
      do {
	t[m] = j;
	j = -m;
	m = i;
	i = t[m];
      } while (i > 0);
      //@ assert m == \at(m,L)
      i = j;
    }
    t[m] = -i;
    m--;
  } while (m > 0);
}

/*@ requires 
  @   n >= 1 && \valid_range(t,1,n) &&
  @   \forall int k; 1 <= k <= n => 1 <= t[k] <= n
  @*/
void safety(int *t, int n) {
  int m = n, j = -1;
  /*@ invariant 
    @   1 <= m <= n && -n <= j <= -1 &&
    @   \forall int k; 1 <= k <= n => (-n <= t[k] <= -1 || 1 <= t[k] <= n)
    @*/
  do {
    int i = t[m];
    if (i > 0) {
      /*@ invariant 
	@   1 <= m <= n && 1 <= i <= n && -n <= j <= -1 &&
	@   \forall int k; 1 <= k <= n => (-n <= t[k] <= -1 || 1 <= t[k] <= n)
	@*/
      do {
	t[m] = j;
	j = -m;
	m = i;
	i = t[m];
      } while (i > 0);
      i = j;
    }
    t[m] = -i;
    m--;
  } while (m > 0);
}


/* test */

#ifdef TEST

int n = 6;
int t[7] = { 0,2,3,1,6,5,4 };

void print(int *t, int n) {
  int i;
  for (i = 1; i <= n; i++) printf("%d ", t[i]);
  printf("\n");
}

int main() {
  print(t,n);
  inverse(t, n);
  print(t,n);
}
#endif

/*
Local Variables: 
compile-command: "make inv_perm.gui"
End: 
*/
