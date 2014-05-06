/*@ requires \valid(f5); */
static char F4(int *f5);

int foo() { int x; F4(&x); return 0; }
