//@ requires x>=0;
void g(int x);

void g(int a) {
  return;
}

void f(int a){ a=1;}
//@ ensures x>0;
void f(int x);
