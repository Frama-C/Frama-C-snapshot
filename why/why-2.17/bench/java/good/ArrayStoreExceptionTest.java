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

/* JLS 2.0, page 216 */

class Point { int x,y; }

class ColoredPoint extends Point { int color; }

class ArrayStoreExceptionTest {

    public static void main(String[] args) {
	ColoredPoint[] cpa = new ColoredPoint[10];
	Point[] pa = cpa;
	System.out.println(pa[1] == null);
	try {
	    pa[0] = new Point();
	}
	catch (ArrayStoreException e) {
	    System.out.println(""+e);
	}

	int[][] t = new int[10][];
	Object[] u = t;
	u[0] = new int[5]; // OK
	u[1] = new Object(); // ArrayStoreException
    }
}
