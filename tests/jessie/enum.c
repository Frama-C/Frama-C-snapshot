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

enum E { A = 1 , y = A + 3 };

/*@ ensures y == 4; */
void f() { }

/*@ ensures 1 <= \result <= 4; */
int g(enum E e) { return e; }

typedef enum { BLUE, WHITE, RED } color;

/*@ requires \valid_range(t,0,9);
  @ ensures t[2] == BLUE || t[2] == WHITE || t[2] == RED ;
  @*/
void h(color *t) {
  t[2] = t[0];
}

// enum used as array index

enum I { U, V, W };

/*@ requires \valid_range(t, U, W);
  @ ensures  t[V] == 0;
  @*/
void enum_as_array_index(int *t) {
  t[V] = 0;
}

/*
Local Variables:
compile-command: "make enum"
End:
*/
