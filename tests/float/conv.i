/* run.config*
   STDOPT: #"-big-ints-hex 65536"
*/

float f;
double d;

volatile v;

// Conversion from binary integer representation to float
void main1() {
  int i;

  i = 0;
  Frama_C_show_each(*(float *)&i);

  i = 1;
  Frama_C_show_each(*(float *)&i);

  i = v ? 100 : 10000000000;
  f = *(float *)&i;
  Frama_C_show_each(f);
  i = *(int *)&f;
  //@ assert *(int *)&f == i;

  i = 0x7F7FFFFF;
  f = *(float *)&i;
  Frama_C_show_each(f);
  //@ assert *(int *)&f == i;

  i += 1;
  if (v) { f = *(float *)&i; // fails
    Frama_C_show_each_unreached(f); }

  i = 0xff7fffff;
  f = *(float *)&i;
  Frama_C_show_each(f);
  //@ assert *(int *)&f == i;

  i ++ ;
  if (v) { f = *(float *)&i; // fails
    Frama_C_show_each_unreached(f); }
  
  i = v ? 1 : (-0x7FFFFFFF-1); // ok
  f = *(float *)&i; Frama_C_show_each(f); f += 0;

  if (v) {  i = v;
    //@ assert -10 <= i <= -1;
    f = *(float *)&i; Frama_C_show_each_unreached(f); /* Fails */ }

  i = v ? 0 : 0x7FFFFFFF; // problem with 0x7FFFFFFF
  f = *(float *)&i; Frama_C_show_each(f);

  if (v) { unsigned int ui = 0xFFFFFFFF;
    f = *(float *)&ui; Frama_C_show_each(f); /* Fails */ }
}

// Conversion from binary integer representation to double
void main2() {
  long long i;

  i = 0;
  Frama_C_show_each(*(double *)&i);

  i = 1;
  Frama_C_show_each(*(double *)&i);

  i = v ? 100 : 7000000000000000000LL;
  Frama_C_show_each(*(double *)&i);

  i = 0x7fefffffffffffff;
  d = *(double *)&i;
  Frama_C_show_each(d);
  //@ assert *(long long*)&d == i;

  i ++;
  Frama_C_show_each(*(double *)&i); // fails

  i = -1;
  Frama_C_show_each(*(double *)&i); // fails

  i = 0xffefffffffffffff;
  d = *(double *)&i;
  Frama_C_show_each(d);
  //@ assert *(long long*)&d == i;

  i ++;
  Frama_C_show_each(*(double *)&i); // fails

  i = v ? 1 : (-0xffffffffffffffff-1);
  Frama_C_show_each(*(double *)&i);

  i = v;
  //@ assert -10 <= i <= -1;
  Frama_C_show_each(*(double *)&i); // fails

  i = v ? 0 : -1; // problem with -1
  Frama_C_show_each(*(double *)&i);

  unsigned long long ui = 0xFFFFFFFFFFFFFFFF;
  Frama_C_show_each(*(double *)&ui);  // fails
}

void main() {
  main1();
  main2();
}
