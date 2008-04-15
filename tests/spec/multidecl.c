/*@ predicate p0(integer x) = x == 0;
  @ predicate p1(integer x) = x == 1;
  @ lemma excl: \forall integer x; ! (p0(x) && p1(x));
  @*/

// not well-typed (testing localization of error messages)
/*@ predicate p2(int x) = x == 0;
  @ predicate p3(int x) = x == 1;
  @ lemma excl2: \forall integer x; ! (p2(x) && p3(x));
  @*/
