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

struct s {
  int x;
  int y;
} s;
/*@ global invariant i: 0 <= s.x && s.x <= s.y && s.y <= 100; */

const int c[] = {12 , 14 };
/*@ global invariant const_c : c[0]==12; */

/*@ requires n>=0; @*/
void f(int n) {
  int t = s.x+n;
  if (t <= s.y - 20) s.x = t + c[0] ;
}


/*
Local Variables:
compile-command: "LC_ALL=C make invariants"
End:
*/
