/**************************************************************************/
/*                                                                        */
/*  The Why platform for program certification                            */
/*  Copyright (C) 2002-2008                                               */
/*    Romain BARDOU                                                       */
/*    Jean-François COUCHOT                                               */
/*    Mehdi DOGGUY                                                        */
/*    Jean-Christophe FILLIÂTRE                                           */
/*    Thierry HUBERT                                                      */
/*    Claude MARCHÉ                                                       */
/*    Yannick MOY                                                         */
/*    Christine PAULIN                                                    */
/*    Yann RÉGIS-GIANAS                                                   */
/*    Nicolas ROUSSET                                                     */
/*    Xavier URBAIN                                                       */
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

/* Dijkstra's dutch flag */

typedef enum { BLUE, WHITE, RED } color;

/*@ predicate isMonochrome(color t[], int i, int j, color c)
  @   { \valid_range(t,i,j) && \forall int k; i <= k && k <= j => t[k] == c } 
  @*/

/*@ requires \valid_index(t,i) && \valid_index(t,j)
  @ assigns t[i],t[j]
  @ ensures t[i] == \old(t[j]) && t[j] == \old(t[i])
  @*/
void swap(color t[], int i, int j) {
  color tmp = t[i];
  t[i] = t[j];
  t[j] = tmp;
}

/*@ requires 
  @   n >= 0 && \valid_range(t,0,n-1) 
  @ assigns t[0 .. n-1]
  @ ensures 
  @   (\exists int b, int r; 
  @            isMonochrome(t,0,b-1,BLUE) &&
  @            isMonochrome(t,b,r-1,WHITE) &&
  @            isMonochrome(t,r,n-1,RED))
  @*/
void flag(color t[], int n) {
  int b = 0;
  int i = 0;
  int r = n;
  /*@ invariant
    @   0 <= b && b <= i && i <= r && r <= n &&
    @   isMonochrome(t,0,b-1,BLUE) &&
    @   isMonochrome(t,b,i-1,WHITE) &&
    @   isMonochrome(t,r,n-1,RED)
    @ loop_assigns b,i,r,t[0 .. n-1]
    @ variant r - i
    @*/
  while (i < r) {
    switch (t[i]) {
    case BLUE:  
      swap(t, b++, i++);
      break;	    
    case WHITE: 
      i++; 
      break;
    case RED: 
      swap(t, --r, i);
      break;
    }
  }
}

/*
Local Variables: 
compile-command: "make flag_checkenum.enum"
End: 
*/
