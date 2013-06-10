/* run.config
OPT: -load-script tests/syntax/reorder.ml
*/

int x;

void f() { x++; }

/*@ axiomatic Ax {
  @   predicate Q (integer v);
  @   }
  @*/

//@ requires Q: \let v = Q(255); !(!v||v) ;
void g (void);

