void main1(int i, int j, int k) {
  //@ assert i >= 5;
  j = j/7+3;
  if (k <= 7)
    while (1);
}

//@ requires i >= 6; assigns \nothing;
void main2(int i);

void f_main3 (int * p, int a) {
  *p += a;
}

void main3 (void) {
  long x = 3;
  long * p = &x;
  //@ assert sizeof(long) == sizeof(int);
  f_main3 (p, x); // go through the casts on p and x
  Frama_C_show_each(x);
}

void f_main4_1 (float f) {
  //@ assert f >= 10;
}

void f_main4_2 (unsigned int f) {
  //@ assert f <= 20;
}

void main4 (int v) {
  if (v <= 15) {
    f_main4_1(v);
    Frama_C_show_each_v(v); // no reduction for now, because of cast
  } else {
    f_main4_2(v);
    Frama_C_show_each_v(v); // same
  }
}

int f_main_2() {
  return 0;
}


void f_main_1(int v) {
  v = f_main_2();
}

void main5() {
  int l = 2;
  f_main_1(l); // no reduction here, l is written in f_main_1
}

void main(int v, int w, int x, int y, int z) {
  main1(x, y, z);
  main2(w);

  Frama_C_dump_each(); // i, j, k must have been removed. 
                       // w, x and z should have been reduced.

  main3();
  main4(v);
  main5();
}
