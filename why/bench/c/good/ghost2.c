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

int v; 
/*@ ghost int pre_v */ 
/*@ ghost int pre2_v */ 

/*@ 
assigns v 
ensures v==-\old(v) 
*/ 
void g(){ v = -v; } 


/*@ 
assigns v,pre_v,pre2_v 
*/ 
void f() 
{ 
  int n; 
  
  n=100; 
  /*@ set pre2_v = 5 */ 
  /*@ set pre_v = -5 */ 
  v=5; 
  
  
  /*@ 
    invariant 0<=n<=100 && (pre2_v > pre_v => pre_v < v) && (pre_v == -v) 
    loop_assigns v,pre_v,pre2_v 
    variant n 
  */ 
  while(n--) 
    { 
      /*@ set pre2_v = pre_v */ 
      /*@ set pre_v = v */ 
      g(); 
    } 
}
