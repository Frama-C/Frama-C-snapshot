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
/*  modify it under the terms of the GNU Library General Public           */
/*  License version 2, with the special exception on linking              */
/*  described in file LICENSE.                                            */
/*                                                                        */
/*  This software is distributed in the hope that it will be useful,      */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  */
/*                                                                        */
/**************************************************************************/

//@ logic int fib(int x)

//@ axiom fib_0 : fib(0) == 0
//@ axiom fib_1 : fib(1) == 1
//@ axiom fin_n : \forall int n; 2 <= n => fib(n) == fib(n-1) + fib(n-2)


/*@ requires 1 < n && \valid_range(t,0,n-1)
  @*/
void compute_fib(int *t, int n) {
  int i;
  t[0] = 0;
  t[1] = 1;
  //@ invariant 2 <= i <= n && t[i-1]==fib(i-1) && t[i-2]==fib(i-2)
  for (i = 2; i < n; i++)
    t[i] = t[i-1] + t[i-2];
}

/*
Local Variables: 
compile-command: "make fib.gui"
End: 
*/
