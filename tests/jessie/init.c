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

int x = 45;

int t[3] = {1,2,3};
/*@ global invariant t_const : t[1] == 2; */

struct S {
  int a;
  int b[3];
};

struct S s = {1,{1,3,4}};
/*@ global invariant s_const : s.b[0] == 1 && s.b[2] == 4; */

/*@ ensures \result == 7; */
int f() {
  int y =x;
  return t[1] + s.b[0] + s.b[2];
}



/*@ ensures \result == 4; */
int g() {
  int t[] = {4,5};
  int x = 45;
  return t[0];
}


/*@ ensures \result == 12; */
int h() {
  int u[3] = { 3,4,5 };
  return u[0] + u[1] + u[2];
}

/*
Local Variables:
compile-command: "LC_ALL=C make init"
End:
*/
