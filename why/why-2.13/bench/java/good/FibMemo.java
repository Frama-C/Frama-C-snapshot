//@+ CheckArithOverflow = no

import java.util.HashMapIntegerInteger;


//@ type mappings;

//@ logic Integer access(mappings m, Integer key);

//@ logic mappings update(mappings m, Integer key, Integer value);

//@ logic mappings empty_mappings();

/*@ axiom access_update_eq:
  @   \forall mappings m, Integer key, Integer value ;
  @       access(update(m,key,value),key) == value ;
  @*/

/*@ axiom access_update_neq:
  @   \forall mappings m, Integer key1 key2, Integer value ;
  @       key1 != key2 ==>
  @       access(update(m,key1,value),key2) == access(m,key2) ;
  @*/

//@ predicate containsKey(mappings m, Integer key);

/*@ axiom containsKey_empty:
  @   \forall Integer key; ! containsKey(empty_mappings(),key);
  @*/

/*@ axiom containsKey_update_any:
  @   \forall mappings m, Integer key1 key2, Integer value; 
  @      containsKey(m,key1) ==> containsKey(update(m,key2,value),key1);
  @*/

/*@ axiom containsKey_update_eq:
  @   \forall mappings m, Integer key, Integer value; 
  @      containsKey(update(m,key,value),key);
  @*/

/* @ axiom containsKey_update_discr:
  @   \forall mappings m, Integer key, Integer value; 
  @      containsKey(update(m,key1,value),key2) ==> key1==key2 || containsKey(m,key2);
  @*/

//@ logic integer math_fib(integer n);

//@ axiom fib0 : math_fib(0) == 1;
//@ axiom fib1 : math_fib(1) == 1;
/*@ axiom fibn : \forall integer n; n >= 2 ==> 
  @    math_fib(n) == math_fib(n-1) + math_fib(n-2);
  @*/

class FibMemo {

    private HashMapIntegerInteger memo;

    /*@ invariant fib_safety: memo != null;
      @*/

    /*@ invariant fib_inv: \forall Integer x; 
      @     containsKey(memo.hashmap_model,x) ==>
      @        access(memo.hashmap_model,x) != null &&
      @        access(memo.hashmap_model,x).value == math_fib(x.value);
      @*/


    FibMemo () {
	memo = new HashMapIntegerInteger();
    }



    /*@ requires n >=0 ;
      @ assigns \nothing;
      @ ensures \result == math_fib(n);
      @*/
    int fib(int n) {       
	if (n <= 1) return 1; 
	Integer m = new Integer(n);
	Integer x = memo.get(m);
	if (! (x == null)) return x.intValue();
	x = new Integer(fib(n-1) + fib(n-2));
	memo.put(m,x); return x.intValue();
    }

    /* @ requires n >=0 ;
      @ ensures \result == math_fib(n);
      @* /
    long fib_long(int n) {       
	if (n <= 1) return 1; 
	Integer m = new Integer(n);
	Long x = memo.get(m);
	if (x != null) return x.longValue();
	x = new Long(fib(n-1) + fib(n-2));
	memo.put(m,x); return x.longValue();
    }
    */

    /*@ requires n >=0 ;
      @ ensures \result == math_fib(n);
      @*/
    long fib_slow(int n) {       
	if (n <= 1) return 1; 
	return fib_slow(n-1)+fib_slow(n-2);

    }
}