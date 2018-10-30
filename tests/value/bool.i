/* run.config*

*/

volatile int rand;


/* Tests valid operations on _Bool values. */
int valid_bool () {
  _Bool x = 0;
  int y;
  Frama_C_show_each(x);
  x=2;
  Frama_C_show_each(x);
  y=x+1;
  Frama_C_show_each(x, y);
  x=x+1;
  Frama_C_show_each(x);
  x=x+1;
  Frama_C_show_each(x);
  return y;
}

union u_bool { _Bool b; unsigned char c; unsigned short s; };

/* Tests trap representations of _Bool variables. */
void invalid_bool () {
  union u_bool ub;
  _Bool b;
  /* Precise _Bool values. */
  ub.c = 42;
  if (rand)
    b = ub.b; // red alarm
  ub.s = 256;
  b = ub.b;
  Frama_C_show_each_zero(b);
  ub.s = 257;
  b = ub.b;
  Frama_C_show_each_one(b);
  ub.s = 258;
  if (rand)
    b = ub.b; // red alarm
  /* Reduction of imprecise _Bool values. */
  ub.s = rand;
  b = ub.b;
  Frama_C_show_each_zero_or_one(b); // unknown alarm
  /* Write through a pointer. */
  b = 17;
  Frama_C_show_each_one(b);
  *((char *)&b) = 17;
  if (rand)
    b = b; // red alarm
  /* Read through a pointer. */
  char c = rand;
  _Bool *p = (_Bool*)&c;
  b = *p; // unknown alarm, reduction of c
  Frama_C_show_each_zero_or_one(c);
}

void main () {
  valid_bool ();
  invalid_bool ();
}
