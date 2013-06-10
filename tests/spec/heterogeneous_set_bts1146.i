/* run.config
   DONTRUN: bugfix in progress
   EXECNOW: make -s tests/spec/Type_of_term.cmxs
   OPT: -load-module ./tests/spec/Type_of_term.cmxs -check -print
*/

/*@ lemma foo: \union(1) == \union(1.0); */

/*@ lemma foo2: \union(1.0) == \union(1); */

/*@ lemma foo3: \union(1.0,2) == \union(1,2.0); */

/*@ lemma foo4: 1.0 == 1; */

/*@ lemma bar: \union() != \union(1); */

/*@ lemma bla: \union(1) != \union(); */

/*@ predicate P{L1,L2}(set<char *>s) = 
      \forall char* p; \subset(p,s) ==> \at(*p,L1) == \at(*p,L2);
*/

/*@ ensures P{Pre,Post}(x); */
void f(int *x, double *y);

/*@ ensures P{Pre,Post}(\union(x,y)); */
void h(int *x, int *y);

int x;

/*@ ensures P{Pre,Post}(&x); */
void g();
