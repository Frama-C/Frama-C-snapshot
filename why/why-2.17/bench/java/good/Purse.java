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

//@+ CheckArithOverflow = no


class NoCreditException extends Exception {

    public NoCreditException() { }

}

public class Purse {
    
    private int balance;
    //@ invariant balance_non_negative: balance >= 0;

    /*@ requires true;
      @ assigns balance;
      @ ensures balance == 0;
      @*/
    public Purse() {
        balance = 0;
    }

    /*@ requires s >= 0;
      @ assigns balance;
      @ ensures balance == \old(balance) + s;
      @*/
    public void credit(int s) {
        balance += s;
    }

    /*@ requires s >= 0;
      @ assigns balance;
      @ ensures s <= \old(balance) && balance == \old(balance) - s;
      @ behavior amount_too_large:
      @   assigns \nothing;
      @   signals (NoCreditException) s > \old(balance) ;
      @*/
    public void withdraw(int s) throws NoCreditException {
        if (balance >= s)
            balance = balance - s;
        else
            throw new NoCreditException();
    }

    //@ ensures \result == balance;
    public int getBalance() {
        return balance;
    }

}

