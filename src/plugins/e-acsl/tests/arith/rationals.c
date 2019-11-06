/* real numbers */

/*@ ensures
    \let delta = 1;
    \let avg_real = (a+b)/2;
    avg_real - delta < \result < avg_real + delta; */
double avg(double a, double b) {
  return (a+b)/2;
}

int main(void) {
  /*@ assert 3 != 1.5; */ ;
  /*@ assert 3 == 1.5 + 1.5; */ ;
  /*@ assert 0.1 == 0.1; */ ;
  /*@ assert (double)1.0 == 1.0; */ ;
  /*@ assert (double)0.1 != 0.1; */ ;
  /*@ assert (float)0.1 != (double)0.1; */ ;
  /*@ assert (double)1.1 != 1 + 0.1 ;*/ ;
  /*@ assert 1 + 0.1 == 2 - 0.9; */ ;
  float x = 0.2f,
        y = 0.3f,
        sum = x + y;
  /*@ assert sum != x * y; */ ;
  /* @ assert \let n = 1; 4 == n + 3.0; */ ; // TODO: fail at runtime, I don't know why

  double d = 0.1;

  avg(4.3, 11.7);

  /*@ assert 1.1d != 1 + 0.1; */ ;

  // Not yet:
  // long double ld = 0.1l;
  // /*@ assert d + 1 != ld + 1; */ ; // long double
  // /*@ assert 3 != 1e5; */ ; // number not written in decimal expansion form
  ///*@ assert \let n = 99999999999999999999999999;
  //     4 != n + 3.7; */ ; // creating Q from Z
}
