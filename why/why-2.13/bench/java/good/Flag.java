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

/* Dijkstra's dutch flag */

//@ predicate is_color(int c) { Flag.BLUE <= c && c <= Flag.RED }

/*@ predicate is_color_array{L}(int t[]) { 
  @   t != null && 
  @   \forall int i; 0 <= i && i < t.length ==> is_color(t[i])
  @ }
  @*/

/*@ predicate is_monochrome{L}(int t[],integer i, integer j, int c) {
  @   \forall int k; i <= k && k < j ==> t[k] == c
  @ }
  @*/



class Flag {
    
    public static final int BLUE = 1, WHITE = 2, RED = 3;
    
    int t[];
    //@ invariant t_non_null: t != null;
    // pb with recursive def of type and logic in Jessie
    // @ invariant is_color_array_inv: is_color_array(t);

    /*@ requires 0 <= i && i <= j && j <= t.length ;
      @ behavior decides_monochromatic:
      @   ensures \result <==> is_monochrome(t,i,j,c);
      @*/
    public boolean isMonochrome(int i, int j, int c) {
    	/*@ loop_invariant i <= k && 
	  @   (\forall int l; i <= l && l < k ==> t[l]==c);
    	  @ decreases j - k;
	  @*/
	for (int k = i; k < j; k++) if (t[k] != c) return false;
	return true;
    }

    /*@ requires 0 <= i && i < t.length && 0 <= j && j < t.length;
      @ behavior i_j_swapped:
      @   assigns t[i],t[j];
      @   ensures t[i] == \old(t[j]) && t[j] == \old(t[i]);
      @*/
    private void swap(int i, int j) {
	int z = t[i];
	t[i] = t[j];
	t[j] = z;
    }

    /*@ behavior sorts:
      @   assigns t[..];
      @   ensures 
      @     (\exists int b r; is_monochrome(t,0,b,BLUE) &&
      @                       is_monochrome(t,b,r,WHITE) &&
      @                       is_monochrome(t,r,t.length,RED));
      @*/
    public void flag() {
	int b = 0;
	int i = 0;
	int r = t.length;
	/*@ loop_invariant
	  @   is_color_array(t) &&
	  @   0 <= b && b <= i && i <= r && r <= t.length &&
	  @   is_monochrome(t,0,b,BLUE) &&
	  @   is_monochrome(t,b,i,WHITE) &&
          @   is_monochrome(t,r,t.length,RED);
	  @ decreases r - i; 
	  @*/
	while (i < r) {
	    switch (t[i]) {
	    case BLUE:  
		swap(b++, i++);
		break;	    
	    case WHITE: 
		i++; 
		break;
	    case RED: 
		swap(--r, i);
		break;
	    }
	}
    }
}



/*
Local Variables: 
compile-command: "make Flag"
End: 
*/
