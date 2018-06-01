typedef unsigned T;
T incr_value;

//@ axiomatic A { predicate Incr(T x, T r) = r == x + incr_value; }

/*@ assigns \nothing;
  @ ensures Incr(x, \result);
  @ */
T incr(T x);

// Was KO before the fix of #453
void f1(T i) {
  /*@ loop assigns i;
    @*/
  while (i<10) {
    /*@ assigns i ;
       ensures Sincr: Incr(\old(i), i);
      */
    i = incr(i);
  }
}

// Was OK before the fix of #453
void f2(T i) {
  /*@ loop assigns i;
    @*/
  while (i<10) {
    //@ ghost A: ;
    /*@ assigns i ;
       ensures Sincr: Incr(\old(i), i);
      */
    i = incr(i);
    //@ ghost B: ;
  }
}
