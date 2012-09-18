int f();

int g () { return f(0) + h(1); }

/*@ requires a>=0;
  assigns \result \from a;
*/
int f(int a);

/*@ ensures \result == b + 1; */
int h(int b);
