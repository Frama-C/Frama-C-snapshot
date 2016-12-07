/*@ requires \valid(p+(0..n-1)); */
void f(int *p,int n);

void g(int x) { f(&x,1); }
