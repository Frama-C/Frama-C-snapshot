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

class VA {

    VA();

    int n;
}

class VB {

    int m(VA x) {
	return x.n;
    }

    void m1(int[] t) {
	for (int i = 0; i < t.length; i++)
	    t[i] = 0;
    }

    int main() {
	VA y = new VA();
	int[] t = new int[3];
	m1(t);
	return m(y);
    }

}

class T {

    int p[];

    //@ requires t != null && t.length >= 1;
    static int m(int t[]) {
	return t[0];
    }

    //@ ensures \result == 0;
    public int test() {
	p = new int [1];
	return m(p);
    }
	
}



/*
Local Variables: 
compile-command: "make Validity.io"
End: 
*/

