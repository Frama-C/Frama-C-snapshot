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
//@+ InvariantPolicy = Arguments
//@+ NonNullByDefault = yes

class C { int x; }

class NonNullByDefault {
    
    int[] t;
    C c;
    static C sc;

    int[] m1() {
	return t;
    }
    
    int[] /*@ nullable @*/ m1bis() {
	return null;
    }
    
    C m2() {
	return c;
    }
    
    C /*@ nullable @*/ m2bis() {
	return null;
    }
    
    C m2ter() {
	return new C();
    }

    void m3(int[] pt, C pc) {
	int n = pt.length;
	this.t = pt;
	int m = pc.x;
	this.c = c;
    }
    
    void m4() {
	int n = sc.x;
    }

}
