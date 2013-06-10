/* run.config
   STDOPT: +"-context-width 3 -then -print -then -context-width 1"
*/

int a;

void main(int *p) {
  if (! (p == 0)) {
    if (! (p+1 == 0)) {
      if (! (p+2 == 0)) {}
    }
    if (p+1 == &a) {}

    *p = 1;
    *(p+1) = 2;
    *(p+2) = 3;
  } else { /*@ assert \false; */ }
}
