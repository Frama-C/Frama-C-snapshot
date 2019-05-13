/*@ axiomatic Foo {

  logic boolean f(integer x);
  axiom f_def: \forall integer x; f(x) == \true <==> (\forall integer y; y < x ==> y < 1) ;

  lemma f_1: f(1);

  }*/


_Bool boolean_casts(int x, _Bool y) {
  //@ check C0: 0 == (integer) \false;
  //@ check C1: 1 == (integer) \true ;
  //@ check c0: \false == (boolean) 0;
  //@ check c1: \true  == (boolean) 1;
  //@ check c2: \true  == (boolean) 2;
  int x0 = 0, x1=1, x2=2;
  //@ check X0: x0 == (int) \false;
  //@ check X1: x1 == (int) \true ;
  //@ check x0: \false == (boolean) x0;
  //@ check x1: \true  == (boolean) x1;
  //@ check x2: \true  == (boolean) x2;
  _Bool b0=0, b1=1;
  //@ check B0: b0 == (_Bool) \false;
  //@ check B1: b1 == (_Bool) \true ;
  //@ check b0: \false == (boolean) b0;
  //@ check b1: \true  == (boolean) b1;
  return 0;
}
