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

//@+ NonNullByDefault = alllocal

class C { 
    public int x;
}

class StaticTyping {

    C a;
    int[] t;

    void m1(C c) {
	a = null; // bug jessie : OP non generee
	C b = c; // b pas marque "non null"...
	b.x = 0; // ..du coup : PO inutile
	int[] tmp = t;
    }


    void m2(C c) {
	m1(c);
    }

    byte[] buffer;

    byte[] getBuffer() { return buffer; }

    void m3() {
	byte[] cmd_apdu = getBuffer();
	
	if (cmd_apdu.length >= 3) cmd_apdu[9] = 0;
    }

}
    


    
