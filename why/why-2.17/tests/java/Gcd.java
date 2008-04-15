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

/* complements for non-linear integer arithmetic */

/*@ lemma distr_right: 
  @   \forall integer x y z; x*(y+z) == (x*y)+(x*z); 
  @*/

/*@ lemma distr_left: 
  @   \forall integer x y z; (x+y)*z == (x*z)+(y*z);
  @*/

/*@ lemma distr_right_minus: 
  @   \forall integer x y z; x*(y-z) == (x*y)-(x*z); 
  @*/

/*@ lemma distr_left_minus: 
  @   \forall integer x y z; (x-y)*z == (x*z)-(y*z);
  @*/

/*@ lemma mul_comm: 
  @   \forall integer x y; x*y == y*x; 
  @*/

/*@ lemma mul_assoc: 
  @   \forall integer x y z; x*(y*z) == (x*y)*z; 
  @*/

/*@ predicate divides(integer x, integer y) =
  @   \exists integer q; y == q*x ;
  @*/

/*@ lemma div_mod_property:
  @  \forall integer x y; 
  @    x >=0 && y > 0 ==> x%y  == x - y*(x/y);  
  @*/

/*@ lemma mod_property:
  @  \forall integer x y; 
  @    x >=0 && y > 0 ==> 0 <= x%y && x%y < y; 
  @*/

/*@ predicate isGcd(integer a, integer b, integer d) =
  @   divides(d,a) && divides(d,b) && 
  @     \forall integer z;
  @     divides(z,a) && divides(z,b) ==> divides(z,d) ;
  @*/

/*@ lemma gcd_zero :
  @   \forall integer a; isGcd(a,0,a) ;
  @*/

/*@ lemma gcd_property :
  @   \forall integer a b d q;
  @      b > 0 && isGcd(b,a % b,d) ==> isGcd(a,b,d) ;
  @*/

class Gcd {

    /*@ requires x >= 0 && y >= 0;
      @ behavior resultIsGcd: 
      @   ensures isGcd(x,y,\result) ;
      @ behavior bezoutProperty:
      @   ensures \exists integer a b; a*x+b*y == \result;
      @*/
    static int gcd(int x, int y) {
        //@ ghost integer a = 1, b = 0, c = 0, d = 1;
        /*@ loop_invariant 
          @    x >= 0 && y >= 0 &&  
	  @    (\forall integer d ;  isGcd(x,y,d) ==> 
	  @        \at(isGcd(x,y,d),Pre)) && 
          @    a*\at(x,Pre)+b*\at(y,Pre) == x && 
          @    c*\at(x,Pre)+d*\at(y,Pre) == y ;
          @ loop_variant y;
          @*/
        while (y > 0) {
            int r = x % y;
            //@ ghost integer q = x / y;
            x = y;
            y = r;
            //@ ghost integer ta = a, tb = b;
            //@ ghost a = c; 
	    //@ ghost b = d;
            //@ ghost c = ta - c * q;
            //@ ghost d = tb - d * q;
        }
        return x;
    }

}




/*
Local Variables: 
compile-command: "make Gcd"
End: 
*/

