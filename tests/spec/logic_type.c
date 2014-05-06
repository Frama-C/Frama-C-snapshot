/*@ type t; */
/*@ logic t create(int x); */
/*@ logic t1 create(int y); // error: type does not exist
*/

/*@ type t2 = t2; */
//@ logic t2 foo;
//@ predicate p(t2 x) = foo == x;
