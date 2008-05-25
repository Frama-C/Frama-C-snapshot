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

/* array copy */

/*@ requires \valid_range(t1,0,n) && \valid_range(t2,0,n);
  @ ensures \forall int k; 0 <= k < n ==> t2[k] == t1[k];
  @*/
void copy(int t1[], int t2[], int n) {
  int i = n;
  /*@ loop invariant i <= n && \forall int k; i <= k < n ==> t2[k] == t1[k];
      loop variant i; */
  while (i-- > 0) {
    t2[i] = t1[i];
  }
}

/* 
Local Variables:
compile-command: "LC_ALL=C make copy"
End:
*/
