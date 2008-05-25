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


/*@ ensures \result == 0
  @*/
int f1() {
  int x=0;
  goto l1;
  x=1;
 l1: return x;
}

int f2() {
  goto l1;
 l2: goto l3;
 l1: goto l2;
 l3: return 0;
}

int f3(int x) {
  if (x==0) { 
    goto l2; 
    l1 : x = 1;  
  } else {
    goto l1;
    l2 : x = 2;
  }
  return x;
}

  
int f4() {
  int x;
  //@ label L
  /*@ invariant x <= \at(x,L)
    @ variant x
    @*/
  while(x > 0) x--;
}

int f5() {
  int x;
 L:
  /* TODO @ invariant x <= \at(x,L)
    @ variant x
    @*/
  while(x > 0) x--;
}
