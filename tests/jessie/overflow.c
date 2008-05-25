/**************************************************************************/
/*                                                                        */
/*  The Why/Caduceus/Krakatoa tool suite for program certification        */
/*  Copyright (C) 2002-2006                                               */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
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

unsigned char c;
signed char sc;
char c1;
short s;

enum E1 { A1, B1 };
enum E2 { A2, B2, C2 };

enum E1 e1;
enum E2 e2;

/*@ requires e2 != C2; */
void f() {
  e1 = e2;
}

/*@ requires 0 <= s <= 255; */
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

/* 
Local Variables:
compile-command: "LC_ALL=C make overflow"
End:
*/
