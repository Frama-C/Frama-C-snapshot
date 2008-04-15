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
//@+ SeparationPolicy = Regions

/*@ axiomatic NumOfPos {
  @  logic integer num_of_pos{L}(integer i,integer j,int t[]);
  @  axiom num_of_pos_empty{L} :
  @   \forall integer i j, int t[];
  @    i > j ==> num_of_pos(i,j,t) == 0;
  @  axiom num_of_pos_true_case{L} :
  @   \forall integer i j k, int t[];
  @       i <= j && t[j] > 0 ==> 
  @         num_of_pos(i,j,t) == num_of_pos(i,j-1,t) + 1;
  @  axiom num_of_pos_false_case{L} :
  @   \forall integer i j k, int t[];
  @       i <= j && ! (t[j] > 0) ==> 
  @         num_of_pos(i,j,t) == num_of_pos(i,j-1,t);
  @ }
  @*/

/*@ lemma num_of_pos_strictly_increasing{L} :
  @   \forall integer i j k l, int t[];
  @       j < k && k <= l && t[k] > 0 ==> 
  @       num_of_pos(i,j,t) < num_of_pos(i,l,t);
  @*/

public class Muller {

    /*@ requires t!=null;
      @*/
    public static int[] m(int t[]) {
	int count = 0;
	
	/*@ loop_invariant
	  @    0 <= i && i <= t.length && 
	  @    0 <= count && count <= i && 
	  @    count == num_of_pos(0,i-1,t) ; 
	  @ loop_variant t.length - i;
	  @*/
	for (int i=0 ; i < t.length; i++) if (t[i] > 0) count++;
	
	int u[] = new int[count];
	count = 0;
	
	/*@ loop_invariant
	  @    0 <= i && i <= t.length && 
	  @    0 <= count && count <= i && 
	  @    count == num_of_pos(0,i-1,t);
	  @ loop_variant t.length - i;
	  @*/
	for (int i=0 ; i < t.length; i++) {
	    if (t[i] > 0) u[count++] = t[i];
	}
	return u;
    }
    
}

/*
Local Variables: 
compile-command: "make Muller"
End: 
*/
