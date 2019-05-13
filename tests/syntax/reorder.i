/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/

int x;

void f() { x++; }

/*@ axiomatic Ax {
  @   predicate Q (integer v);
  @   }
  @*/

//@ requires Q: \let v = Q(255); !(!v||v) ;
void g (void);

