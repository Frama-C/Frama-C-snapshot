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

/* continue tests */

/*@ ensures \result == 0; */
int f1()
{
  int n = 10;
  /*@ loop invariant 0 <= n; loop variant n; */
  while (n > 0) {
    if (n == 5) { n = 0; continue; }
    n--;
  }
  return n;
}

/*@ ensures \result == 10; */
int f2()
{
  int i = 17;
  /*@ loop invariant i <= 10; loop variant 10 - i; */
  for (i = 0; i < 10; i++) {
    if (i == 5) { i = 6; continue; }
  }
  return i;
}

/*@ ensures \result == 7; */
int f3()
{
  int i;
  /*@ loop invariant i <= 7 && i != 6; loop variant 7 - i; */
  for (i = 0; i < 6; i++) {
    if (i == 5)
      { i = 6; continue; }
  }
  return i;
}

/*
int main(void) {
  printf("%d\n",f3());
  return 0;
}
*/

/* 
Local Variables:
compile-command: "LC_ALL=C make continue"
End:
*/
