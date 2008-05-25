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

/*

C test file

*/

int i;
int j;

/*@ ensures i == \old(j) + k && j == 3 * \old(j) + 11 * k + 12; */
void test(int k)
{
  int l = 1;
  int m = 12;
  i = j + k;
  l *= j ;
  j += l + 10 * k + i + m;
}

// lemmas to help automatic provers 
/*@ lemma dist1: \forall int x, int y, int z; x*(y+z) == x*y + x*z; */
/*@ lemma dist2: \forall int x, int y, int z; (x+y)*z == x*z + y*z; */
/*@ lemma id1: \forall int x; x*1 == x; */
/*@ lemma id2: \forall int x; 1*x == x; */

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make arith"
End:
*/
