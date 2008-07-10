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


//@+ CheckArithOverflow = no

/* is_max(t,i,l) is true whenever t[i] is the maximum of t[0]..t[l-1]
 * l is an integer and not an int, because used as t.length which 
 * (in the logic) returns an integer and not an int 
 */
/*@ predicate is_max{L}(int[] t,int i,integer l) {
  @   t != null && 0 <= i < l <= t.length &&
  @   (\forall integer j; 0 <= j < l ==> t[j] <= t[i])
  @ }
  @*/

public class Arrays {

    /*@ requires t != null && 1 <= t.length <= 32767;
      @ behavior max_found:
      @   ensures 
      @      0 <= \result < t.length && 
      @      (\forall integer i; 
      @           0 <= i < t.length ==> t[i] <= t[\result]);
      @*/
    public static short findMax(int[] t) {
	int m = t[0];
	short r = 0;
        /*@ loop_invariant 
          @   1 <= i <= t.length && 0 <= r < t.length &&
          @   m == t[r] && (\forall integer j; 0 <= j < i ==> t[j] <= m);
          @ decreases t.length-i;
          @*/
	for (short i=1; i < t.length; i++) {
	    if (t[i] > m) {
		r = i; 
		m = t[i];
	    }
	}
	return r;
    }

    /*@ requires t != null && t.length >= 1;
      @ behavior max_found:
      @  ensures 
      @      0 <= \result < t.length && 
      @      is_max(t,\result,t.length) ;
      @*/
    public static int findMax2(int[] t) {
	int m = t[0];
	int r = 0; 
	/*@ loop_invariant 
	  @   1 <= i <= t.length && 0 <= r < t.length &&
          @   m == t[r] && is_max(t,r,i) ;
	  @ decreases t.length-i;
	  @*/
	for (int i=1; i < t.length; i++) {
	    if (t[i] > m) {
		r = i; 
		m = t[i];
	    }
	}
	return r;
    }


    /*@ requires t != null ;
      @ ensures 
      @   \forall integer i; 0 < i < t.length ==> t[i] == \old(t[i-1]);
      @*/
    public static void arrayShift(int[] t) {
	/*@ loop_invariant 
	  @   j < t.length &&
	  @   (t.length > 0 ==>
	  @     (0 <= j && 
          @     (\forall integer i; 0 <= i <= j ==> t[i] == \at(t[i],Pre)) &&
          @     (\forall integer i; j < i < t.length ==> t[i] == \at(t[i-1],Pre))));
	  @ decreases j;
	  @*/
      for (int j = t.length-1 ; j > 0 ; j--) {
	  t[j] = t[j-1];
	}
    }


}


/*
Local Variables: 
compile-command: "make Arrays"
End: 
*/

