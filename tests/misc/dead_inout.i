/* run.config
   OPT: -out -input -inout -inout -inout-callwise -main main_all
 */

// This tests a potential imprecision with the computation of input and outputs if one forgets to test that a statement is dead
int a, b;

void f() {
  a = b;
}

void g () {
  int x = 0;
  if (x) f ();
}

void main(){
  f ();
  g ();
}

// This tests the computation of inout with non-conditional ifs 
void f2(int v, int *p, int *q) {
  if (v)
    *q = 1;
  if (v)
    *p = 2;
}


void main2() {
  int x, y;
  f2(0, &x, &x);
  f2(1, &x, &y);
}

// Main

void main_all() {
  main();
  main2();
}
