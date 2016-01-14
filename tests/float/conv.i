/* run.config
   STDOPT: #"-big-ints-hex 65536"
*/

float f;
double d;

volatile v;

// Conversion from binary integer representation to float
void main1() {
  int i;

  i = 0;
  Frama_C_show_each(*(float *)&i+0);

  i = 1;
  Frama_C_show_each(*(float *)&i+0);

  i = v ? 100 : 10000000000;
  f = *(float *)&i+0;
  Frama_C_show_each(f);
  i = *(int *)&f;
  //@ assert *(int *)&f == i;

  i = 0x7F7FFFFF;
  f = *(float *)&i+0;
  Frama_C_show_each(f);
  //@ assert *(int *)&f == i;

  i += 1;
  f = *(float *)&i+0; // fails
  Frama_C_show_each(f);

  i = 0xff7fffff;
  f = *(float *)&i+0;
  Frama_C_show_each(f);
  //@ assert *(int *)&f == i;

  i ++ ;
  f = *(float *)&i+0; // fails
  Frama_C_show_each(f);
  
  i = v ? 1 : (-0x7FFFFFFF-1); // ok
  Frama_C_show_each(*(float *)&i+0);

  i = v;
  //@ assert -10 <= i <= -1; // fails
  Frama_C_show_each(*(float *)&i+0);

  i = v ? 0 : 0x7FFFFFFF; // problem with 0x7FFFFFFF
  Frama_C_show_each(*(float *)&i+0);

  unsigned int ui = 0xFFFFFFFF;
  Frama_C_show_each(*(float *)&ui+0);
}

// Conversion from binary integer representation to double
void main2() {
  long long i;

  i = 0;
  Frama_C_show_each(*(double *)&i+0);

  i = 1;
  Frama_C_show_each(*(double *)&i+0);

  i = v ? 100 : 7000000000000000000LL;
  Frama_C_show_each(*(double *)&i+0);

  i = 0x7fefffffffffffff;
  d = *(double *)&i+0;
  Frama_C_show_each(d);
  //@ assert *(long long*)&d == i;

  i ++;
  Frama_C_show_each(*(double *)&i+0); // fails

  i = -1;
  Frama_C_show_each(*(double *)&i+0); // fails

  i = 0xffefffffffffffff;
  d = *(double *)&i+0;
  Frama_C_show_each(d);
  //@ assert *(long long*)&d == i;

  i ++;
  Frama_C_show_each(*(double *)&i+0); // fails

  i = v ? 1 : (-0xffffffffffffffff-1);
  Frama_C_show_each(*(double *)&i+0);

  i = v;
  //@ assert -10 <= i <= -1;
  Frama_C_show_each(*(double *)&i+0); // fails

  i = v ? 0 : -1; // problem with -1
  Frama_C_show_each(*(double *)&i+0);

  unsigned long long ui = 0xFFFFFFFFFFFFFFFF;
  Frama_C_show_each(*(double *)&ui+0);
}

void main() {
  main1();
  main2();
}
