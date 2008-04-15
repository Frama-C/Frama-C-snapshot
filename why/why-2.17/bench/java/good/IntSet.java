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

/*@ predicate mem(IntSet s,int n) =
  @    \exists integer i; 0 <= i < s.taille && s.t[i] == n ;
  @*/

public class IntSet {

    int taille = 0;
    int t[] = new int[10];

    /*@ invariant sorted:
      @   t != null && 0 <= taille <= t.length &&
      @   (\forall integer i j; 0 <= i <= j < taille ==> t[i] <= t[j]);
      @*/

    /*@ assigns \nothing;
      @ ensures
      @   0 <= \result <= taille &&
      @   (\forall integer i; 0 <= i < \result ==> t[i] < n) && 
      @   (\forall integer i; \result <= i < taille ==> t[i] >= n) ;
      @*/
    public int index(int n) {
	int a = 0, b = taille, m;
	/*@ loop_invariant 
	  @   0 <= a <= b <= taille &&
	  @   (\forall integer i; 0 <= i < a ==> t[i] < n) && 
	  @   (\forall integer i; b <= i < taille ==> t[i] >= n) ; 
	  @ loop_variant b-a;
	  @*/
	while (a<b) {
	    m = (a+b)/2;
	    if (t[m] < n) a=m+1; else b=m;
	}
	return a;
    }

    /*@ assigns \nothing;
      @ ensures \result == true <==> 
      @   (\exists integer i; 0 <= i < taille && t[i] == n);
      @*/
    public boolean mem(int n) {
	int i = index(n);
	return (i < taille && t[i] == n);
    }	
        
    /*@ assigns t,taille; // ,t[..]; syntax error ??
      @ ensures (\forall int k; mem(this,k) <==> \old(mem(this,k)) || k==n);
      @*/
    public void add(int n) {	
	int i = index(n);
	if (i < taille && t[i] == n) return;
	/* ensures taille < t.length ....
	  */
	if (taille < t.length) {
	    copy(t,t,i,taille-1,i+1);
	}
	else {
	    int old[] = t;
	    t = new int[2*taille+1];
	    copy(old,t,0,i-1,0);
	    copy(old,t,i,taille-1,i+1);
	}
	t[i] = n;
	taille++; 
    }

    /* A version of add with intermediate annotation for automatic
     * proof with simplify 
     */
    /*@ assigns t,taille; // ,t[..]; syntax error ??
      @ ensures (\forall int k; mem(this,k) <==> \old(mem(this,k)) || k==n);
      @*/
    public void add2(int n) {	
	int i = index(n);
	if (i < taille && t[i] == n) return;
        /* @ assigns t,t[..];
          @ ensures (\forall int j; 0 <= j && j < i; t[j]==\old(t[j])) 
          @    && (\forall int j; i+1 <= j && j < taille + 1; t[j]==\old(t[j-1]))
              && (\forall int j; i <= j && j < taille; \old(t[j])==t[j+1])
              && t !=null && t[i]==n && taille < t.length && t instanceof int[];
        */
	{if (taille < t.length) {
	    copy(t,t,i,taille-1,i+1);
	}
	else {
	    int old[] = t;
	    t = new int[2*taille+1];
	    copy(old,t,0,i-1,0);
	    copy(old,t,i,taille-1,i+1);
	}
	t[i] = n;};
	taille++; 
    }


    /*
       copy(src,dest,a,b,c) copies src[a..b] to dest[c..c+b-a]
       if src and dest are the same, c is assumed greater than or equal to a.
     */
    /*@ requires src != null && dest != null && 
      @   0 <= a && a-1 <= b && b < src.length && 0 <= c && 
      @     c+b-a < dest.length && (dest == src ==> c >= a);
      @ assigns dest[c..c+b-a]; 
      @ ensures (\forall integer i; c <= i <= c+b-a ==> dest[i] == \old(src[i+a-c]));
      @*/

    public static void copy(int src[], int dest[],int a, int b, int c) {
	/*@ loop_invariant 
	  @   a-1 <= j <= b &&
	  @   (\forall integer i; j < i <= b ==> dest[i+c-a] == \at(src[i],Pre)) &&
	  @   (\forall integer i; c <= i <= j+c-a ==> dest[i] == \at(dest[i],Pre));
	  @ loop_variant j;
	  @*/
	for (int j = b; j >= a; j--) {
	    dest[j+c-a] = src[j];
	}
    }
}


