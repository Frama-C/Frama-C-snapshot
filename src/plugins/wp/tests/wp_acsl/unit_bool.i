/*@ axiomatic Foo {

  logic boolean f(integer x);
  axiom f_def: \forall integer x; f(x) == \true <==> (\forall integer y; y < x ==> y < 1) ;

  lemma f_1: f(1);

  }*/
