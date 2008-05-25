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

typedef struct las3
{
  int  c;
} las3;

typedef struct las2
{
  las3    b[3];
  
} las2;

typedef struct las1
{
  las2  d;
  
} las1;

typedef struct las
{
  las1    a;
} las;

las x;
las2 y;

/*@ ensures \result == x.a.d.b[0].c + y.b[1].c;
  @ */
int f() {
  return x.a.d.b[0].c + y.b[1].c;
}


/* 
Local Variables:
compile-command: "LC_ALL=C make dassault_1"
End:
*/
