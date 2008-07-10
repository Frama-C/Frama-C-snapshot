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

//////////////////////////////////////////////////////////////////////

class C1 {

    int[] t;
    //@ invariant C1_t_inv: t != null && t.length >= 1;

    void m(int[] p) {
	p[0] = 1;
   }

    void main() {
	m(t);
    }

}

//////////////////////////////////////////////////////////////////////

class C2 {

    static int[] t;
    //@ static invariant C2_t_inv: t != null && t.length >= 1;

    void m(int[] p) {
	p[0] = 1;
   }

    void main() {
	m(t);
    }

}

//////////////////////////////////////////////////////////////////////

class C3 {

    static Object t;
    //@ static invariant C3_t_inv: t != null;

    void m() {
	t = new Object();
	//@ assert t != null; 
   }

}

//////////////////////////////////////////////////////////////////////

class C4 {
    static int[] t; 
    //@ static invariant C4_t_inv: t != null && t.length == 10;
}

class C5 {
    void m() {
	int i = C4.t[0]; 
    }
}



/*
Local Variables: 
compile-command: "make Invariants"
End: 
*/


