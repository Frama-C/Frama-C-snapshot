/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -check -print
*/

int x[10];

struct S { int y; int z; } s;

int t;

/*@ predicate int_eq(int logical, int from_c) = logical == from_c; */

/*@ ensures int_eq(*(int*)((unsigned)0x1 + 0x2),(int)0); */
int f() {

*(int *)((unsigned)0x1 + 0x2) = 0;
return 0;
}

/*@ ensures int_eq(x[0], (int)1);
    ensures int_eq(s.y, (int)2);
    ensures int_eq(s.z, (int)3);
    ensures int_eq(t,(int)4);
*/
int main() {
  x[0] = 1;
  s.y = 2;
  s.z = 3;
  t = 4;
}
