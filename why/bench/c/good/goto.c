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


/*@ ensures x == 0 => \result == 0
  @*/
int f2(int x) {
  if (x==0) goto l1;
  x=1;
 l1: return x;
}


/*@ ensures x == 9 <=> \result == 10
  @*/
int f3(int x) {
  if (x==0) goto l1;
  x++;
  if (x==10) goto l1;
  x=1;
 l1: return x;
}

  
/*@ ensures x == 0 => \result == 0
  @*/
int f4(int x) {
  if (x==0) goto l1;
 l2:  x++;
 l1: return x;
}
  
