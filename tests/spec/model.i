/* run.config
STDOPT: +"-load-script tests/spec/model.ml"
*/
struct S { int x; int y; };

typedef struct S T;

/*@ model struct S { integer z }; */
/*@ model struct S { integer x }; */ // KO field exists in the struct

/*@ model T { integer t; }; */

/*@ model T { integer z }; */ //KO field exists in parent type
/*@ model T { integer x }; */ //KO field exists in parent type

/*@ type invariant t_invariant(T t) = t.t == t.z * 2; */

/*@
  assigns *s;
  ensures s->z == \result;
*/
int f(struct S* s);

/*@ type invariant sum(struct S s) = s.z == s.x + s.y; */


void main() {
  struct S s = { 0, 0 };
  T t = {1,2};
  /*@ assert t.t == 6 && t.z == 3; */
  int a = f(&s);
  if (a && !s.x) { /*@ assert s.y != 0; */ } else { s.x == 1; }
  /*@ assert s.z != 0; */
}

/*@ model double { real exact }; */
/*@ model double { real round }; */

/*@ ensures \result == (double)(x+y); 
    ensures \result.exact == x.exact + y.exact;
    ensures \result == \result.exact + \result.round;
 */
double add(double x, double y);

double foo(double x) { return add(x,x); }

