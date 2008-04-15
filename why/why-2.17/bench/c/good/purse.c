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

typedef struct purse {
  int balance;
} purse;

//@ predicate purse_inv(purse *p) { \valid(p) && p->balance >= 0 }

/*@ requires purse_inv(p) && s >= 0
  @ assigns p->balance
  @ ensures purse_inv(p) && p->balance == \old(p->balance) + s 
  @*/
void credit(purse *p,int s) {
  p->balance = p->balance + s;
}



/*@ requires purse_inv(p) && 0 <= s <= p->balance
  @ assigns p->balance
  @ ensures purse_inv(p) && p->balance == \old(p->balance) - s
  @*/
void withdraw(purse *p,int s) {
  p->balance = p->balance - s;
}


/*@ requires purse_inv(p1) && purse_inv(p2) && p1 != p2
  @ assigns p1->balance, p2->balance
  @ ensures \result == 0
  @*/
int test1(purse *p1, purse *p2) {
    p1->balance = 0;
    credit(p2,100);
    return p1->balance;
}

/*@ assigns \nothing
  @ ensures \fresh(\result) && purse_inv(\result) && \result->balance == 0
  @*/
purse *new_purse() { 
  purse* p = (purse*) malloc(1 * sizeof(purse));
  p->balance = 0;
  return p;
}

/*@ ensures \result == 150
  @*/
int test2() {
  purse *p1 = new_purse();
  purse *p2 = new_purse();
  credit(p1,100);
  credit(p2,200);
  withdraw(p1,50);
  withdraw(p2,100);
  return p1->balance + p2->balance;
}


/*
void main() {
  purse *p = new_purse();
  test1(p,p);
}
*/
