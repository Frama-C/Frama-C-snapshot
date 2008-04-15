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


/*@ ensures \result == 3 */
int f() {
  int t[3] = {1,2,3};
  return t[2];
}

//@ ensures \result == 3
int g() {
  int t[] = {1,2,3};
  return t[2];
}

int u[4];

/*@ requires u[2] == 12
  @ ensures  \result == 12 */
int h() {
  int t[4] = {1,2,3,4};
  return u[2];
}

//@ ensures \result == 3
int two_local_arrays() {
  int t[4] = {1,2,3,4};
  int u[5] = {0,0,t[2],0};
  return u[2];
}

//@ ensures \result == 3
int two_local_arrays_not_alias() {
  int t[5];
  int v[6];
  t[4] = 3;
  v[4] = 1;
  return t[4];
}

struct S { int a; int b[4]; struct S *next; };

/*
//@ ensures \result == 3
int local_struct() {
  struct S s = { 1, {1,2,3,4}, (void*)0 };
  return s.b[2];
}
*/
