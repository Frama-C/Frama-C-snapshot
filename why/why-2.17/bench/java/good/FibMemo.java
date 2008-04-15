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

//@+ CheckArithOverflow = yes
// @+ MinimalClassHierarchy = yes

import java.util.HashMapIntegerInteger;
import java.util.HashMapIntegerLong;


//@ type mappings;

//@ logic Integer access_int(mappings m, Integer key);
/*@ logic mappings update_int(mappings m, Integer key, Integer value) {
  @  axiom access_update_eq_int:
  @     \forall mappings m, Integer key, Integer value ;
  @       access_int(update_int(m,key,value),key) == value ;
  @  axiom access_update_neq_int:
  @   \forall mappings m, Integer key1 key2, Integer value ;
  @       key1 != key2 ==>
  @       access_int(update_int(m,key1,value),key2) == access_int(m,key2) ;
  @ }
  @*/ 



//@ logic Long access(mappings m, Integer key);
/*@ logic mappings update(mappings m, Integer key, Long value) {
  @  axiom access_update_eq:
  @   \forall mappings m, Integer key, Long value ;
  @       access(update(m,key,value),key) == value ;
  @  axiom access_update_neq:
  @   \forall mappings m, Integer key1 key2, Long value ;
  @       key1 != key2 ==>
  @       access(update(m,key1,value),key2) == access(m,key2) ;
  @ }
  @*/


//@ logic mappings empty_mappings();

/*@ predicate containsKey(mappings m, Integer key) {
  @  axiom containsKey_empty:
  @    \forall Integer key; ! containsKey(empty_mappings(),key);
  @  axiom containsKey_update_any_int:
  @   \forall mappings m, Integer key1 key2, Integer value; 
  @      containsKey(m,key1) ==> containsKey(update_int(m,key2,value),key1);
  @ axiom containsKey_update_any:
  @   \forall mappings m, Integer key1 key2, Long value; 
  @      containsKey(m,key1) ==> containsKey(update(m,key2,value),key1);
  @ axiom containsKey_update_eq_int:
  @   \forall mappings m, Integer key, Integer value; 
  @      containsKey(update_int(m,key,value),key);
  @ axiom containsKey_update_eq:
  @   \forall mappings m, Integer key, Long value; 
  @      containsKey(update(m,key,value),key);
  @ }
  @*/

/* @ axiom containsKey_update_discr:
  @   \forall mappings m, Integer key, Integer value; 
  @      containsKey(update(m,key1,value),key2) ==> key1==key2 || containsKey(m,key2);
  @*/

/*@ logic integer math_fib(integer n) {
  @  axiom fib0 : math_fib(0) == 1;
  @  axiom fib1 : math_fib(1) == 1;
  @  axiom fibn : \forall integer n; n >= 2 ==> 
  @    math_fib(n) == math_fib(n-1) + math_fib(n-2);
  @ }
  @*/

class FibMemo {

    private HashMapIntegerInteger memo_int;
    private HashMapIntegerLong memo;

    /*@ invariant fib_safety: memo != null;
      @*/

    /*@ invariant fib_inv: \forall Integer x; 
      @     containsKey(memo.hashmap_model,x) ==>
      @        access(memo.hashmap_model,x) != null &&
      @        access(memo.hashmap_model,x).value == math_fib(x.value);
      @*/


    FibMemo () {
	memo_int = new HashMapIntegerInteger();
	memo = new HashMapIntegerLong();
    }



    /*@ requires n >=0 ;
      @ assigns \nothing;
      @ ensures \result == math_fib(n);
      @*/
    int fib_int(int n) {       
	if (n <= 1) return 1; 
	Integer m = new Integer(n);
	Integer x = memo_int.get(m);
	if (! (x == null)) return x.intValue();
	x = new Integer(fib_int(n-1) + fib_int(n-2));
	memo_int.put(m,x); return x.intValue();
    }

    /*@ requires n >=0 ;
      @ ensures \result == math_fib(n);
      @*/
    long fib(int n) {       
	if (n <= 1) return 1; 
	Integer m = new Integer(n);
	Long x = memo.get(m);
	if (x != null) return x.longValue();
	x = new Long(fib(n-1) + fib(n-2));
	memo.put(m,x); return x.longValue();
    }

    /*@ requires n >=0 ;
      @ ensures \result == math_fib(n);
      @*/
    long fib_slow(int n) {       
	if (n <= 1) return 1; 
	return fib_slow(n-1)+fib_slow(n-2);

    }
}