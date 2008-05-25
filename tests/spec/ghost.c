/* run.config
DONTRUN: ghost code is not supported
*/

struct A { int x; };

/*@ ghost struct B { int y; }; */


/*@ ghost struct B b1; */

/*@ requires b1.y == 0 ; */
int main() {
  /*@ ghost struct B b; */
  struct A a;
  /*@ ghost b.y = 0; a.x = b.y; */
  return 0;
}
