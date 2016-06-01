volatile int vol;

char * str = "abc";

int x;
int y;

struct ts {
  int i1;
  int i2;
};


void f1 () {
  //@ assert \valid_read(str);
  //@ assert !\valid(str);
}


void f2() {
  x = 4;
  y = 5;
  //@ assert \at(x == 2 && y == 3, Pre) && x == 4 && y == 5;
  //@ assert x == 4+\at(x, Init);
}


void f3() {
  //@ assert \at(x == 2, Pre) || \at(x == 3, Pre);
  //@ assert \at(x == 2 || x == 3, Pre);
  //@ assert \at(x == 2, Pre) ==> x == 2;
}


void g4(struct ts s) {
  int x = 1;
  s.i1 = 3;
  //@ assert \initialized{Pre}(&s) ==> \at(s.i1 == 1,Pre);
  //@ assert \initialized{Pre}(&s) ==> s.i1 == 1;
}

void f4() {
  int *p, *q;
  int z;
  p = &z;
  q = &x;
  struct ts s;
  if (vol) {
    s.i1 = 1; s.i2 = 2;
  }
  g4(s);
  //@ assert !\initialized{Pre}(p);
  //@ assert  \initialized{Pre}(q);
}

//@ requires x==\at(x,Init)==0;
void main() {
  f1();

  x = 2;
  y = 3;
  f2();

  x = vol ? 2 : 3;
  f3();

  f4();
}
