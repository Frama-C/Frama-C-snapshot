
volatile int rand;

void main () {
  double a = 42.;
  double b = 11.;
  double min, max;

  double *p = rand ? &a : &b;
  double *q = rand ? &a : &b;

  if (*p < *q) {
    max = *q;
    min = *p;
  }
  else if (*q < *p) {
    max = *p;
    min = *q;
  }
  else {
    max = a;
    min = b;
  }

}
