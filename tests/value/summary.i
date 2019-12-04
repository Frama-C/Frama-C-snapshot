/* run.config*
  STDOPT: +"-eva-msg-key=summary -main minimalist"
  STDOPT: +"-eva-msg-key=summary -main minimal"
  STDOPT: +"-eva-msg-key=summary -main bottom"
  STDOPT: +"-eva-msg-key=summary -main main"
  STDOPT: +"-rte -eva-msg-key=summary -main main"
*/

/* Tests the summary on the smallest possible program. */
void minimalist ();
void minimal () {}

/* Sure alarm and non-terminating function. */
void bottom () {
  int x = 10 / 0;
}

volatile int undet;
volatile double volatile_d;

/* Tests the summary on most kinds of alarms. */
void alarms () {
  int x = 0, y = 0;
  int *p, *q;
  int t[10] = {0};
  p = &x + undet;
  x = *p;                 // invalid read memory access
  p = &x + undet;
  *p = x;                 // invalid write memory access
  x = t[undet];           // out of bound index + uninitialized read
  x = 100 / undet;        // division by zero
  x = undet + undet;      // overflow
  x = undet << undet;     // invalid shift
  double d = volatile_d;
  d = d - d;              // nan and infinite floating-point value
  x = (int) d;            // invalid cast from floating-point to integer
  p = undet ? &x : &y;
  q = undet ? &y : &x;
  if (undet) x = p - q;   // invalid pointer comparison
  if (p < q) x = 0;       // invalid pointer comparison
  if (undet) {
    int z;
    p = &z;  // eva warning about escaping z
  }
  x = *p;                 // dangling pointer
}

void f(void);
void g(void);

/* 1 valid assertion, 1 unknown assertion, 1 invalid assertion. */
void logic () {
  /*@ assert \true; */
  /*@ assert undet == 0; */
  if (undet)
    /*@ assert \false; */
    ;
}

// 2 kernel warnings, 1 eva warning, no error.
void main () {
  alarms ();
  logic ();
  f(); // kernel warning: no specification for function f
  g(); // kernel warning: no specification for function g
}

/* Assertions in this function should not appear in the summary. */
void dead () {
  /*@ assert \true; */
  /*@ assert \false; */
}
