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

public class Switch {

    /*@ behavior normal:
      @   assigns \nothing;
      @   ensures (n==0 || n==1) <==> \result == 0;
      @*/
    public static int test1 (int n) {
	int r;
	switch(n) {
	case 0:
	case 1:
	    r = 0;
	    break;
	default:
	    r = 2;
	}
	return r;
    }

    /*@ behavior normal:
      @   assigns \nothing;
      @   ensures ((n==4 || n==7) <==> \result == 1) && 
      @            ((n==0 || n==1) <==> \result == 0);
      @*/
    public static int test2 (int n) {
	int r;
	switch(n) {
	case 0:
	case 1:
	    r = 0;
	    break;
	case 4:
	case 7:
	    r = 1;
	    break;
	case 12:
	default:
	case 26:
	    r = 2;
	}
	return r;
    }

}

/*
Local Variables: 
compile-command: "make Switch.io"
End: 
*/

