/* run.config
   OPT: -load-script tests/syntax/add_allocates.ml -print
*/


int* x;

void f(void);

//@ allocates x;
void g(void);

//@ behavior b: requires \false; allocates x;
void main(int c) {
  f();
  while (c) {
    //@ loop allocates x;
    while (1) {
      while (!c);
    }
    //@ for b: loop allocates x;
    while (1) {
    }
  }
}
