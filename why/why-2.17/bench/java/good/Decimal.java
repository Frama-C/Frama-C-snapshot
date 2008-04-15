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

//@+ CheckArithOverflow = no

/*@ lemma real_add_int : 
  @   (\forall integer x y; (real) (x + y) == (real) x + (real) y);
  @*/

/*@ lemma real_mul_int : 
  @   (\forall integer x y; (real) (x * y) == (real) x * (real) y);
  @*/

/*@ lemma mul_add_distr1 : 
  @   (\forall real x y z; x * (y + z) == (x * y) + (x * z));
  @*/

/*@ lemma add_comm : 
  @   (\forall real x y; x + y == y + x);
  @*/

/*@ lemma add_assoc : 
  @   (\forall real x y z; x + (y + z) == (x + y) + z);
  @*/

/* @ lemma PRECISION_property : 
  @   0.01 * (real) DecimalAbstractReal.PRECISION == 1.0;
  @*/

/* @ lemma PRECISION_property : 
  @   0.01 * (real) 100 == 1.0;
  @*/

/*@ lemma PRECISION_property_real : 
  @   0.01 * 100.0 == 1.0;
  @*/

/*@ logic real amount_real_value{L}(DecimalAbstractReal x) =
  @    (real) x.intPart + 0.01 * (real) x.decPart ;
  @*/

  
/*@ logic integer amount_cent_value{L}(DecimalAbstractReal x) =
  @    DecimalAbstractReal.PRECISION * x.intPart + x.decPart ;
  @*/

public class DecimalAbstractReal {
  
    /** minimal fraction of currency = 1/PRECISION */
    public static final short PRECISION = 100;

    short intPart;
    short decPart;
    //@ invariant decimal: 0 <= decPart && decPart < 100;
    // bug: should be PRECISION ;

    /*@ requires a != null;
      @ assigns intPart, decPart;
      @ behavior real_value:
      @   ensures 
      @     amount_real_value(this) == 
      @         \old(amount_real_value(this)) + \old(amount_real_value(a));
      @ behavior cent_value:
      @   ensures 
      @     amount_cent_value(this) ==
      @         \old(amount_cent_value(this)) + \old(amount_cent_value(a));
      @ 
      @*/
    public void add(DecimalAbstractReal a) {
	short d = (short)(decPart + a.decPart);
	short i = (short)(intPart + a.intPart);
	if (d >= PRECISION) {
	    d -= PRECISION;
	    i++;
	}
	intPart = i;
	decPart = d;
    }

    
    /*@ requires a != null && a != this;
      @ assigns intPart, decPart;
      @ behavior real_value:
      @   ensures 
      @     amount_real_value(this) ==
      @         \old(amount_real_value(this)) + amount_real_value(a);
      @ behavior cent_value:
      @   ensures 
      @     amount_cent_value(this) ==
      @         \old(amount_cent_value(this)) + amount_cent_value(a);
      @ 
      @*/

    public void add2(DecimalAbstractReal a) {
	short d = (short)(decPart + a.decPart);
	short i = (short)(intPart + a.intPart);
	if (d >= PRECISION) {
	    d -= PRECISION;
	    i++;
	}
	intPart = i;
	decPart = d;
    }

}
