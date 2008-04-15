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

void loop1(int n) {
  //@ variant n
  while (n > 0) n--;
}

void loop2(int n) {
  //@ variant 100-n
  while (n < 100) n++;
}

//@ ensures \result == 0
int loop3() {
  int i = 100;
  //@ invariant 0 <= i variant i
  while (i > 0) i--;
  return i;
}

//@ ensures \result == 100
int sum() {
  int i = 100, s;
  //@ invariant 0 <= i <= 100 && s == 100-i variant i
  for (s = 0; i > 0; i--) s++;
  return s;
} 


//@ requires 0 <= n
void loop(int n) {
  int i;
  //@ invariant 0 <= i <= n variant n-i
  for (i = 0; i < n; i++);
}

