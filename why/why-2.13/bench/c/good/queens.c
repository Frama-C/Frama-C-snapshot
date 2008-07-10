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

/* Verification of the following 2 lines code for the N queens:

t(a,b,c){int d=0,e=a&~b&~c,f=1;if(a)for(f=0;d=(e-=d)&-e;f+=t(a-d,(b+d)*2,(
c+d)/2));return f;}main(q){scanf("%d",&q);printf("%d\n",t(~(~0<<q),0,0));}
*/

/****** abstract sets of integers *******************************************/

//@ type iset

//@ predicate in_(int x, iset s)

//@ predicate included(iset a, iset b) { \forall int i; in_(i,a) => in_(i,b) }

//@ logic int card(iset s)
//@ axiom card_nonneg : \forall iset s; card(s) >= 0

//@ logic iset empty()
//@ axiom empty_def : \forall int i; !in_(i,empty())
//@ axiom empty_card : \forall iset s; card(s)==0 <=> s==empty()

//@ logic iset diff(iset a, iset b)
/*@ axiom diff_def : 
  @   \forall iset a, iset b; \forall int i;
  @     in_(i,diff(a,b)) <=> (in_(i,a) && !in_(i,b))
  @*/

//@ logic iset add(int x, iset a)
/*@ axiom add_def : 
  @   \forall iset s; \forall int x; \forall int i;
  @     in_(i,add(x,s))  <=> (i==x || in_(i,s))
  @*/

//@ logic iset remove(int x, iset s)
/*@ axiom remove_def : 
  @   \forall iset s; \forall int x; \forall int i;
  @     in_(i,remove(x,s))  <=> (in_(i,s) && i!=x)
  @*/

/*@ axiom remove_card : 
  @   \forall iset s; \forall int i;
  @     in_(i,s) => card(remove(i,s)) == card(s) - 1
  @*/

//@ logic int min_elt(iset s)
/*@ axiom min_elt_def : 
  @   \forall iset s; card(s) > 0 => 
  @     (in_(min_elt(s), s) &&
  @     \forall int i; in_(i,s) => min_elt(s) <= i)
  @*/

//@ logic iset singleton(int x)
/*@ axiom singleton_def : \forall int i, int j; in_(j,singleton(i)) <=> j==i
  @*/

//@ logic iset succ(iset s)
/*@ axiom succ_def_1 :  
  @   \forall iset s; \forall int i; in_(i,s) => in_(i+1,succ(s))
  @*/
/*@ axiom succ_def_2 :  
  @   \forall iset s; \forall int i; in_(i,succ(s)) => i>=1 && in_(i-1,s)
  @*/

//@ logic iset pred(iset s)
/*@ axiom pred_def_1 : 
  @   \forall iset s; \forall int i; i>=1 => in_(i,s) => in_(i-1,pred(s))
  @*/
/*@ axiom pred_def_2 : 
  @   \forall iset s; \forall int i; in_(i,pred(s)) => in_(i+1,s)
  @*/

//@ logic iset below(int n)
//@ axiom below_def : \forall int n, int i; in_(i, below(n)) <=> 0<=i<n
//@ axiom below_card : \forall int n; card(below(n)) == n

/****** interpretation of C ints as abstract sets of integers ***************/

//@ logic iset iset(int x)

//@ axiom iset_c_zero : \forall int x; iset(x)==empty() <=> x==0

/*@ axiom iset_c_remove : 
  @  \forall int x, int a, int b; 
  @    iset(b)==singleton(x) => in_(x,iset(a)) => iset(a-b)==remove(x, iset(a))
  @*/

// lowest bit trick
/*@ axiom iset_c_min_elt :
  @   \forall int x; x != 0 => iset(x&-x) == singleton(min_elt(iset(x)))
  @*/
/*@ axiom iset_c_min_elt_help :
  @   \forall int x; x != 0 <=> x&-x != 0
  @*/

/*@ axiom iset_c_diff :
  @  \forall int a, int b; iset(a&~b) == diff(iset(a), iset(b))
  @*/

/*@ axiom iset_c_add :
  @  \forall int x, int a, int b; 
  @    iset(b)==singleton(x) => !in_(x,iset(a)) => iset(a+b) == add(x, iset(a))
  @*/

// @ axiom iset_c_succ : \forall int a; iset(a*2) == succ(iset(a))

// @ axiom iset_c_pred : \forall int a; iset(a/2) == pred(iset(a))

//@ axiom iset_c_below : \forall int n; iset(~(~0<<n)) == below(n)

/****** helper lemmas *******************************************************/

/* @ axiom included_trans : \forall iset a, iset b, iset c;
  @   included(a,b) => included(b,c) => included(a,c)
  @*/

/* @ axiom included_diff : \forall iset a, iset b; included(diff(a,b), a) */

/* @ axiom included_remove : \forall iset a, int x; included(remove(x,a), a) */

/***************************************************************************/

// t1: termination of the for loop
int t1(int a, int b, int c){
  int d, e=a&~b&~c, f=1;
  if (a)
    /*@ variant card(iset(e)) */
    for (f=0; d=e&-e; e-=d) {
      f+=t1(a-d,(b+d)*2,(c+d)/2);
    }
  return f;
}

/****************************************************************************/

// t2: termination of the recursive function: card(iset(a)) decreases
int t2(int a, int b, int c){
  int d, e=a&~b&~c, f=1;
  //@ label L
  if (a)
    /*@ invariant included(iset(e),\at(iset(e),L)) */
    for (f=0; d=e&-e; e-=d) {
      //@ assert \exists int x; iset(d) == singleton(x) && in_(x,iset(e)) 
      //@ assert card(iset(a-d)) < card(iset(a))
      f+=t2(a-d,(b+d)*2,(c+d)/2);
    }
  return f;
}

/****************************************************************************/

//@ logic int N() // N queens on a NxN chessboard 
//@ axiom N_positive : N() > 0

// t and u have the same prefix [0..i[
/*@ predicate eq_prefix(int *t, int *u, int i) {
  @   \forall int k; 0 <= k < i => t[k]==u[k]
  @ } 
  @*/

//@ predicate eq_sol(int *t, int *u) { eq_prefix(t, u, N()) } 

int** sol; // sol[i] is the i-th solution
/*@ axiom dont_bother_me_I_am_a_ghost_1 : 
  @   \forall int i; \valid(sol+i) */
/*@ axiom dont_bother_me_I_am_a_ghost_2 : 
  @   \forall int i, int j; \valid(sol[i]+j) */

int s = 0; // next empty slot in sol for a solution

int* col; // current solution being built
int k;    // current row in the current solution

// s stores a partial solution, for the rows 0..k-1
/*@ predicate partial_solution(int k, int* s) {
  @   \forall int i; 0 <= i < k => 
  @     0 <= s[i] < N() &&
  @     (\forall int j; 0 <= j < i => s[i] != s[j] &&
  @                                   s[i]-s[j] != i-j &&
  @                                   s[i]-s[j] != j-i)
  @ }
  @*/

//@ predicate solution(int* s) { partial_solution(N(), s) }

// lemma
/*@ axiom partial_solution_eq_prefix:
  @   \forall int *t, int *u; \forall int k;
  @     partial_solution(k,t) => eq_prefix(t,u,k) => partial_solution(k,u)
  @*/

/*@ predicate lt_sol(int *s1, int *s2) {
  @   \exists int i; 0 <= i < N() && eq_prefix(s1, s2, i) && s1[i] < s2[i]
  @ } 
  @*/

/* s[a..b[ is sorted for lt_sol */
/*@ predicate sorted(int **s, int a, int b) {
  @   \forall int i, int j; a <= i < j < b => lt_sol(s[i], s[j])
  @ } 
  @*/

/*@ requires x != 0
  @ ensures  \result == min_elt(iset(x))
  @*/
int min_elt(int x);

/*@ requires solution(col)
  @ assigns  s, sol[s][0..N()-1]
  @ ensures  s==\old(s)+1 && eq_sol(sol[\old(s)], col)
  @*/
void store_solution();

/*@ requires
  @   0 <= k && k + card(iset(a)) == N() && 0 <= s &&
  @   pre_a:: (\forall int i; in_(i,iset(a)) <=> 
  @            (0<=i<N() && \forall int j; 0<=j<k => i != col[j])) &&
  @   pre_b:: (\forall int i; i>=0 => (in_(i,iset(b)) <=> 
  @            (\exists int j; 0<=j<k && col[j] == i+j-k))) &&
  @   pre_c:: (\forall int i; i>=0 => (in_(i,iset(c)) <=> 
  @            (\exists int j; 0<=j<k && col[j] == i+k-j))) &&
  @   partial_solution(k, col)
  @ assigns
  @   col[k..], s, k, sol[s..][..]
  @ ensures  
  @   \result == s - \old(s) && \result >= 0 && k == \old(k) &&
  @   sorted(sol, \old(s), s) &&
  @   \forall int* t; ((solution(t) && eq_prefix(col,t,k)) <=>
  @                    (\exists int i; \old(s)<=i<s && eq_sol(t, sol[i])))
  @*/
int t3(int a, int b, int c){
  int d, e=a&~b&~c, f=1;
  //@ label L
  if (a)
    /*@ invariant 
      @   included(iset(e),\at(iset(e),L)) &&
      @   f == s - \at(s,L) && f >= 0 && k == \old(k) && 
      @   partial_solution(k, col) &&
      @   sorted(sol, \at(s,L), s) &&
      @   \forall int *t; 
      @     (solution(t) && 
      @      \exists int di; in_(di, diff(\at(iset(e),L), iset(e))) &&
      @          eq_prefix(col,t,k) && t[k]==di) <=>
      @     (\exists int i; \at(s,L)<=i<s && eq_sol(t, sol[i]))
      @ loop_assigns
      @   col[k..], s, k, sol[s..][..]
      @*/
    for (f=0; d=e&-e; e-=d) {
      //@ assert \exists int x; iset(d) == singleton(x) && in_(x,iset(a)) 
      //@ ghost col[k] = min_elt(d);            // ghost code 
      //@ ghost k++;                            // ghost code
      f += t3(a-d, (b+d)*2, (c+d)/2);
      //@ ghost k--;                            // ghost code
    }
  //@ ghost else 
  //@ ghost   store_solution(); // ghost code
  return f;
}

/*@ requires 
  @   n == N() && s == 0 && k == 0
  @ ensures 
  @   \result == s &&
  @   sorted(sol, 0, s) &&
  @   \forall int* t; 
  @      solution(t) <=> (\exists int i; 0<=i<\result && eq_sol(t,sol[i]))
  @*/
int queens(int n) {
  return t3(~(~0<<n),0,0);
}

