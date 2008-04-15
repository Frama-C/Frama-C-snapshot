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

char c;
signed char sc;
char c1;
short s;

enum E1 { A1, B1 };
enum E2 { A2, B2, C2 };

enum E1 e1;
enum E2 e2;

/*@ requires e2 != C2 */
void f() {
  e1 = e2;
}

/*@ requires 0 <= s <= 255 */
void g() {
  c = s;
}

int i;
long int li;
long long int lli;

void h() {
  c = 10;
  sc = c;
  s = c1;
  i = s;
  li = i;
  lli = li;
}

void hh() {
  i = 7;
  s = i;
  c = s;
}
