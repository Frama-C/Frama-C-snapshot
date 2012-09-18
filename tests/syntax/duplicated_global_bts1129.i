void f(int* x);

void f(int* x) { *x++; }

int X;
//@ ensures X==1;
void f(int* x);
