volatile int nondet;

int t1() {
  unsigned int j = nondet;
  unsigned i = 1 << j;
  return i;
}

int t2() {
  unsigned int j = 1;
  if (nondet) j = 10;
  if (nondet) j = 31;
  if (nondet) j = 1000000000;
  if (nondet) j = 2000000000;
  unsigned i = 1 << j;
  return i;
}

void t3() {
  unsigned int x = 1000000000;
  int i;
  for (i = 1000000000; i < 2000000000; i++) {
    if (nondet) x = i;
  }
  //@assert 1 << (1 << x) > 0;
}

void t4() {
  unsigned int x = 1000000000;
  if (nondet)  x = 1000000001;
  //@assert 1 << (1 << x) > 0;
}

void t5() {
  unsigned int x = 1000000000;
  int i;
  for (i = 1000000000; i < 2000000000; i++) {
    if (nondet) x = i;
  }
  //@assert 1 << (1 >> x) > 0;
}

void t6() {
  unsigned int x = 1000000000;
  if (nondet)  x = 1000000001;
  //@assert 1 << (1 >> x) > 0;
}

void t7() {
  unsigned int x = 1022;
  if (nondet)  x = 1023;
  //@assert 1 << (1 << x) > 0;
}

void t8() {
  unsigned int x = 1022;
  if (nondet)  x = 1023;
  //@assert 1 << (1 >> x) > 0;
}

int main() {
  int r = 0;
  if (nondet) r += t1();
  if (nondet) r += t2();
  t3();
  t4();
  t5();
  t6();
  t7();
  t8();
  return r;
}
