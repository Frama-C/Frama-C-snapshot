/* run.config
   OPT: -wp-invariants
*/

/* run.config_qualif
   DONTRUN: (config_qualif) useless
*/

//@ predicate I(integer x) ;
//@ predicate A(integer x) ;
//@ predicate R(integer x) ;
//@ predicate E(integer x, integer y) ;

int i;
/*@ requires R:R(i) ;
  @ assigns Loc:i;
  @ ensures E:E(i,\old(i));
*/
void g(void);

void f1 (void) {
  //@ loop assigns loc:i;
   while (i < 10){
    //@ invariant I:I(i);
    g() ;
  }
}
void f2 (void) {
  //@ loop assigns loc:i;
   while (i < 10){
    g() ;
    //@ invariant T:I(i);
  }
}


void f3 (void) {
  //@ loop assigns loc:i;
  while (i < 10) {
    //@ invariant I:I(i);
    //@ assert A:A(i);
    i = i+1 ;
  }
}

void f4 (void) {
  //@ loop assigns loc:i;
  while (i < 10) {
    //@ assert A:A(i);
    i = i+1 ;
    //@ invariant I:I(i);
  }
}
