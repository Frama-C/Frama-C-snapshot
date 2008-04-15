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


//@ lemma mean1 : (\forall integer x y ; x <= y ==> x <= (x + y) / 2) ;
//@ lemma mean2 : (\forall integer x y ; x < y ==> (x + y) / 2 < y) ;

//@ type intset ;

//@ logic intset emptyset() ;

//@ logic intset singleton(int n) ;

/*@ logic intset union(intset s1, intset s2) {
  @  axiom union_empty_left :
  @   \forall intset s ; union(emptyset(),s) == s;
  @ }
  @*/

/*@ predicate In(int n, intset s) {
  @  axiom In_emptyset : 
  @   \forall int n ; ! In(n,emptyset()) ;
  @  axiom In_union : 
  @   \forall intset s1 s2, int n ; 
  @      In(n,union(s1,s2)) <==> In(n,s1) || In(n,s2) ;
  @  axiom In_singleton : 
  @   \forall int n k ; 
  @      In(n,singleton(k)) <==> n==k ;
  @  axiom intset_ext : 
  @   \forall intset s1 s2;
  @     (\forall int n ; In(n,s1) <==> In(n,s2)) ==> s1==s2 ;
  @ }
  @*/

/*@ predicate array_models(intset s, int t[], integer i, integer j) =
  @   t != null && 
  @   0 <= i && j < t.length &&
  @   (\forall int n; In(n,s) <==> 
  @      (\exists int k;  i <= k <= j && n==t[k])) ;
  @*/

/*@ predicate IntSetModelField_models(intset s, IntSetModelField i) =
  @   i != null && 
  @   array_models(s,i.t,0,i.size-1) ;
  @*/

public class IntSetModelField {

    int size;
    int t[];

    /*@ invariant private_inv:
      @   t != null && 0 <= size <= t.length &&
      @   (\forall integer i j; 0 <= i <= j < size ==> t[i] <= t[j]);
      @*/

    //@ model intset my_model;

    //@ invariant repr_inv: IntSetModelField_models(this.my_model,this);

    /*@ assigns my_model;
      @ ensures my_model == emptyset();
      @*/
    IntSetModelField () {
        t = new int[10]; 
	size = 0;
    }



    /*@ assigns \nothing;
      @ ensures
      @   0 <= \result <= size &&
      @   (\forall integer i; 0 <= i < \result ==> t[i] < n) && 
      @   (\forall integer i; \result <= i < size ==> t[i] >= n) ;
      @*/
    public int index(int n) {
	int a = 0, b = size, m;
	/*@ loop_invariant 
	  @   0 <= a <= b <= size &&
	  @   (\forall integer i; 0 <= i < a ==> t[i] < n) && 
	  @   (\forall integer i; b <= i < size ==> t[i] >= n) ; 
	  @ loop_variant b-a;
	  @*/
	while (a<b) {
	    m = (a+b)/2;
	    if (t[m] < n) a=m+1; else b=m;
	}
	return a;
    }

    /*@ assigns \nothing;
      @ ensures \result == true <==> In(n,my_model);
      @*/
    public boolean mem(int n) {
	int i = index(n);
	return (i < size && t[i] == n);
    }	
        
    /* add with intermediate annotations for automatic
     * proof with simplify 
     */
    /*@ assigns t,size, my_model; // t[..], syntax error ?
      @ ensures my_model == union(\old(my_model),singleton(n));
      @*/
    public void add(int n) {	
	int i = index(n);
	if (i < size && t[i] == n) return;
        /* @ // assigns t; // ,t[*]; syntax error ?
            ensures (\forall int j; 0 <= j && j < i; t[j]==\old(t[j])) 
              && (\forall int j; i+1 <= j && j < size + 1; t[j]==\old(t[j-1]))
              && (\forall int j; i <= j && j < size; \old(t[j])==t[j+1])
              && t !=null && t[i]==n && size < t.length && t instanceof int[];
        */
	{if (size < t.length) {
	    copy(t,t,i,size-1,i+1);
	}
	else {
	    int old[] = t;
	    t = new int[2*size+1];
	    copy(old,t,0,i-1,0);
	    copy(old,t,i,size-1,i+1);
	}
	t[i] = n;};
	size++; 
    }



    /*@ ensures \result == true;
      @*/
    public static boolean test() {
	IntSetModelField s = new IntSetModelField();
	s.add(1);
	s.add(2);
	//@ assert s.my_model == union(singleton(1),singleton(2));
	return s.mem(1);
    }

    /*
       copy(src,dest,a,b,c) copies src[a..b] to dest[c..c+b-a]
       if src and dest are the same, c is assumed greater than or equal to a.
     */
    /*@ requires src != null && dest != null && 
      @   0 <= a && a-1 <= b < src.length && 0 <= c && 
      @     c+b-a < dest.length && (dest == src ==> c >= a);
      @ assigns dest[c..c+b-a]; 
      @ ensures \forall integer i; c <= i <= c+b-a ==> dest[i] == \old(src[i+a-c]);
      @*/

    public static void copy(int src[], int dest[],int a, int b, int c) {
	/*@ loop_invariant 
	  @   a-1 <= j <= b &&
	  @   \forall integer i; j < i <= b ==> dest[i+c-a] == \old(src[i]) &&
	  @   \forall integer i; c <= i <= j+c-a ==> dest[i] == \old(dest[i]);
	  @ loop_variant j;
	  @*/
	for (int j = b; j >= a; j--) {
	    dest[j+c-a] = src[j];
	}
    }


}


