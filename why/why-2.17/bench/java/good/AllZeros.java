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

class AllZeros {

    /*@ requires t != null;
      @ ensures \result <==> \forall integer i; 0 <= i < t.length ==> t[i] == 0; 
      @*/
    static boolean all_zeros(int t[]) {
	/*@ loop_invariant 
	  @  0 <= k <= t.length && 
	  @  \forall integer i; 0 <= i < k ==> t[i] == 0;
	  @ loop_variant t.length - k;
	  @*/
	for (int k = 0; k < t.length; k++) 
	    if (t[k] != 0) 
		return false;
	return true;
    }
}

