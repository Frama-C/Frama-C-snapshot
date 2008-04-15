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

/* complements for non-linear integer arithmetic */

//@ lemma zero_right: \forall integer x; x*0 == 0;
//@ lemma zero_left: \forall integer x; 0*x == 0;
//@ lemma one_right: \forall integer x; x*1 == x;
//@ lemma one_left: \forall integer x; 1*x == x;
//@ lemma two_right: \forall integer x; x*2 == x+x;
//@ lemma two_left: \forall integer x; 2*x == x+x;

/*@ lemma distr_right: 
  @   \forall integer x y z; x*(y+z) == (x*y)+(x*z);
  @*/

/*@ lemma distr_left: 
  @   \forall integer x y z; (x+y)*z == (x*z)+(y*z);
  @*/

/*@ lemma sqr_short_elim: 
  @   \forall integer x; x*x <= 32760 ==> x <= 180;
  @*/

/*@ lemma sqr_short_intro: 
  @   \forall integer x; 0 <= x && x <= 181 ==> x*x <= 32761;
  @*/

/*@ lemma sqr_int_elim: 
  @   \forall integer x; x*x <= 2147395599 ==> x <= 46339;
  @*/

/*@ lemma sqr_int_intro: 
  @   \forall integer x; 0 <= x && x <= 46340 ==> x*x <= 2147395600;
  @*/


public class Lesson1 {

    /*@ behavior result_ge_x:
      @   ensures \result >= x; 
      @ behavior result_ge_y:
      @   ensures \result >= y; 
      @ behavior result_is_lub:
      @   ensures \forall integer z; z >= x && z >= y ==> z >= \result;
      @*/
    public static int max(int x, int y) {
	if (x>y) return x; else  return y; 
    }
    
    /*@ requires x >= 0 && x <= 32760;
      @ ensures \result >= 0 && \result * \result <= x 
      @    && x < (\result + 1) * (\result + 1);
      @*/
    public static short short_sqrt(short x) {
	short count = 0, sum = 1;
    	/*@ loop_invariant 
	  @   count >= 0 && x >= count*count &&
          @   sum == (count+1)*(count+1) &&
	  @   count <= 180 && sum <= 32761;
	  @ loop_variant x - sum;
	  @*/
	while (sum <= x) { 
	    count++;
	    //@ assert (count*count)+2*count+1 == (count+1)*(count+1);
	    sum += 2*count+1;
	}
	return count;
    }   
  
    /*@ requires x >= 0 && x <= 2147395599;
      @ behavior result_is_sqrt: 
      @   ensures \result >= 0 && \result * \result <= x 
      @      && x < (\result + 1) * (\result + 1) ;
      @*/
    public static int sqrt(int x) {
	int count = 0, sum = 1;
    	/*@ loop_invariant 
	  @   count >= 0 && x >= count*count &&
          @   sum == (count+1)*(count+1) &&
	  @   count <= 46339 && sum <= 2147395600;
	  @ loop_variant x - sum;
	  @*/
	while (sum <= x) {
	    count++;
	    //@ assert (count*count)+2*count+1 == (count+1)*(count+1);
	    sum += 2*count+1;
	}
	return count;
    }   
  
}

/*
Local Variables: 
compile-command: "make Lesson1"
End: 
*/
