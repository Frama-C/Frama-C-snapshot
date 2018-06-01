volatile v;

//@ requires c; assigns \nothing;
void __assert(int c);

#define TEST(cond) \
  if ((float) x cond ) {                            \
    __assert ((float)x cond );                        \
    Frama_C_show_each_then(__LINE__, x);            \
  } else {                                          \
    __assert (! ((float)x cond ) );                   \
    Frama_C_show_each_else(__LINE__, x);            \
  }

//@ requires -10000 <= x <= 10000;
void main(int x) {

  TEST ( > 30.1f )
  TEST ( > 30.f )
  TEST ( >= 30.f )

  TEST ( > -30.1f )
  TEST ( >= -30.f )
  TEST ( > -30.f )

}
