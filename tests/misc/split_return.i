/* run.config
   STDOPT: +"-slevel-function init:3,main1:3,f2:4,main2:4,f4:3,main5:3" +"-val-split-return-function f2:0,f3:-2,f4:4,f5:-2" +"-then -report"
   STDOPT: +"-slevel 6" +"-val-split-return-auto" +"-then -report"
 */
/*@ assigns \result \from \nothing;
  assigns *p \from \nothing;
  ensures \result == 0 && \initialized(p) || \result == 1; */
int init(unsigned int *p);

unsigned int main1() {
  unsigned int x;
  int r = init(&x);

  switch(r) {
  case 0:
    x = x /2 + 2;
    break;
  case 1:
    x = 0;
    break;
  default:
    //@ assert \false;
  }
  return x;
}

extern unsigned int i2;
unsigned int f2() {
  if (!i2) {
    i2 = 0;
    return 0;
  } else if (!(i2+1)) {
    i2 = 5;
    return 5;
  } else {
    i2 = 5;
    return 7;
  }
}

void main2() {
  unsigned int r = f2();
  Frama_C_show_each_f2(r, i2);
  if (r == 0) {
    //@ assert i2 == 0;
  } else {
    Frama_C_show_each_f2_2(r, i2);
    //@ assert i2 != 0;
  }
}

extern int i3;
int f3() {
  int res1, res2;
  if (i3) {
    i3 = 0;
    res1 = -2;
  } else {
    i3 = 5;
    res1 = 7;
  }
  res2 = res1;
  return res2;
}

void main3() {
  int r = f3();
  Frama_C_show_each_f3(r, i3);
  if (r == -2) {
    //@ assert i3 == 0;
  } else {
    //@ assert i3 != 0;
  }
}

extern int i4;
int f4() {
  if (i4) {
    i4 = 0;
    return 4;
  } else {
    i4 = 5;
    return 7;
  }
}

void main4() {
  int r = f4();
  Frama_C_show_each_f4(r, i4);
  if (r == 4) {
    //@ assert i4 == 0;
  } else {
    //@ assert i4 != 0;
  }
}

extern int i5;
int f5() {
  int res;
  if (i5) {
    i5 = 0;
    res = -2;
  } else {
    i5 = 5;
    res = 7;
  }
  return res;
}

void main5() {
  int r = f5();
  Frama_C_show_each_f5(r, i5);
  if (r == -2) {
    //@ assert i5 == 0;
  } else {
    //@ assert i5 != 0;
  }
}

volatile v;

int f6() {
  int i = v;
  //@ assert -5 <= i <= 5;
  return i;
}

void main6() {
  if ((short)(f6())) {
  }
}

void main() {
  main1();
  main2();
  main3(); // not enough slevel in f3. One warning
  main4(); // not enough slevel in main4. No warning
  main5(); // no need for slevel, because we do not fuse on return instr
  main6();
}
