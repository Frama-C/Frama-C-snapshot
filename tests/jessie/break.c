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

/* break tests */


/*@ ensures \result == 12; */
int f1()
{
  /*@ loop invariant \true; loop variant 1; */ while (1) break;
  return 12;
}


/*@ ensures \result == 1; */
int f2()
{
  int n = 10;

  /*@ loop invariant 0 <= n; loop variant n; */
  while (n >= 0) {
    if (n == 0) { n++; break; }
    n--;
  }
  return n;
}


/*@ ensures \result == 2; */
int f3()
{
  int n = 10;
  /*@ loop invariant 1 <= n; loop variant n; */
  while (n >= 0) {
    if (n == 1) { n++; break; }
    n--;
  }
  return n;
}

/*@ ensures \result == 3; */
int f4(int x)
{
  int i = 0;
  /*@ loop invariant i <= 3; loop variant 10 - i; */
  for (i = 0; i < 10; i++) {
    if (i == 3) break;
  }
  return i;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make break"
End:
*/
