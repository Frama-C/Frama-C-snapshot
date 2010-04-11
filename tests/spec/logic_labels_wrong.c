int x;

// wrong: multiply defined label
//@ predicate p{L,L}(integer n) = n > 0 ;

// label missing, but automatically inferred
//@ predicate p(int t[]) = t[0];

/*@ axiomatic Q {
  @   predicate q(int t[]);
  @   //label missing, but automatically inferred
  @   axiom q_ax: \forall int t[]; t[0] == 0 ==> q(t);
  @ }
  @*/

void f() {

  // wrong: \old forbidden in loop invariants
  //@ loop invariant x == \old(x);
  for (;;) ;

  // wrong: label undefined
  //@ assert \at(x,L0) == 0;

  // wrong: label defined later
  //@ assert \at(x,L1) == 0;


    for(;;) { L2: x = 0; }
  // wrong: label defined in inner block
  //@ assert \at(x,L2) == 0;

 L1: x = 0;
}

/*
Local Variables:
compile-command: "../../bin/toplevel.opt  -pp-annot -print  logic_labels_wrong.c"
End:
*/
