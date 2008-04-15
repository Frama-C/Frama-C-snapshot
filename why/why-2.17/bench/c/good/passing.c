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

/*

C test file

*/

/*@ requires \valid(x) assigns *x ensures *x == 0 */
void g(int* x) { *x = 0; }

int * r;

/*@ requires \valid(r) ensures \result == 0 */
int g2() { g(r); return *r; }

#if 0
/*@ ensures \result == 0 */
int g3() { int i = 1; g(&i); return i; }
#endif

/*@ requires \valid_index(x,0)  assigns x[0]  ensures x[0] == 1 */ 
void f(int x[]) { 
  x[0] = 1;
}

int t[2];

/*@ ensures t[0] == 1 */ 
void main() {
  f(t);
} 



  
