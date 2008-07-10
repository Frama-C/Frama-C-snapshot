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


//@ ensures \result >= 0
int any();

//@ type intpair // = (int,int)

//@ logic intpair pair(int x, int y) 

//@ predicate lexico(intpair p1, intpair p2)

/* @ predicate lexico(intpair p1, intpair p2) = 
   @ \let (px1,py1) = p1 in ...
   @*/

/*@ axiom lexico_1 : \forall int x1, int x2, int y1, int y2; 
  @    x1 < y1 => lexico(pair(x1,x2),pair(y1,y2))
  @*/

/*@ axiom lexico_2 : \forall int x1, int x2, int y1, int y2; 
  @    x1 == y1 && x2 < y2 => lexico(pair(x1,x2),pair(y1,y2))
  @*/


//@ requires x >= 0 && y >= 0
int f(int x,int y) {

  /*@ invariant x >= 0 && y >= 0
    @ variant pair(x,y) for lexico
    @*/
  while (x > 0 && y > 0) {
    
    if (any()) {
      x--; y = any();
    }
    else y--;
    
  }

}
  
