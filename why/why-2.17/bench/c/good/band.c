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

short ValueA;
/*@ invariant tev: 0 <= ValueA <= 10*/
short ValueB;
/*@ invariant ev: 0 <= ValueB <= 10*/
short returnValue;
/*@ invariant eev: 0 <= returnValue <= 10*/


/*@ 
  @ assigns returnValue
  @ ensures \result >= 0
  */
short test1(void)
{	
	returnValue = ValueA & ValueB;

	return returnValue;
}

/*@ 
  @ assigns returnValue
  @ ensures \result >= 0
  */
short test2(void)
{	
	returnValue = ValueA & ValueB;

	if((returnValue < 0) || (returnValue > 10))
	{
		returnValue = 0;
	}
	
	return returnValue;
}

/*@ 
  @ assigns returnValue
  @ ensures \result >= 0
  */
short test3(void)
{	
	returnValue = returnValue;

	return returnValue;
}

