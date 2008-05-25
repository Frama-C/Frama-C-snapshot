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

/* These programs _are_ incorrect i.e. the corresponding proof obligations
   should _not_ be provable.
   They are here to test the consistence of the theory. */

struct u{int xxx;};

struct v{struct u xx; int yy[5];};

struct v  *z;
struct u zz;

int x[4];
int y[5];

/*@ ensures \false; */
void false0() {
}

void false1() {
  z->yy[5] = 3;
}

void false2() {
  x[-1] = 1;
}

void false3() {
  y[5] = 2;
  }

/* 
Local Variables:
compile-command: "LC_ALL=C make false"
End:
*/
