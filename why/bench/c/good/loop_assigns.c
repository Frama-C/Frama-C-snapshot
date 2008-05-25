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

int t[10];

/*@ assigns t[..]
    ensures \forall int k; 0 <= k < 10 => t[k] == 0 */
void f1() {
  int i;
  /*@ invariant 0 <= i <= 10 && \forall int k; 0 <= k < i => t[k] == 0
    @ loop_assigns t[..]
    @ variant 10-i
    @*/
  for (i = 0; i < 10; i++) t[i] = 0;
}

/*@ assigns t[0 .. 9]
    ensures \forall int k; 0 <= k < 10 => t[k] == 0 */
void f2() {
  int i;
  /*@ invariant 0 <= i <= 10 && \forall int k; 0 <= k < i => t[k] == 0
    @ loop_assigns t[0 .. 9]
    @ variant 10-i
    @*/
  for (i = 0; i < 10; i++) t[i] = 0;
}

/*@ assigns t[0 .. 9]
    ensures \forall int k; 0 <= k < 10 => t[k] == 0 */
void f3() {
  int i;
  /*@ invariant 0 <= i <= 10 && \forall int k; 0 <= k < i => t[k] == 0
    @ loop_assigns t[0 .. i-1]
    @ variant 10-i
    @*/
  for (i = 0; i < 10; i++) t[i] = 0;
}

