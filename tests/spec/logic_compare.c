/*@ type foo; */

/*@ predicate test(foo x) reads x; */

/*@ axiom foo_eq_refl: \forall foo x; x == x; */

/*@ axiom foo_test: \forall foo x; test(x); */
