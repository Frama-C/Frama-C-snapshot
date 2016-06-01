volatile int v;

void main() {
  int* x, y;
  y = 1;

  if (v) {
    int v;
    x = &v;
  } else {
    x = &y;
  }

  if (v) {
    //@ assert !\dangling(&x);
    //@ assert !\dangling(&x);
  }
  if (v) {
    int i = *x + 1;
    int j = *x + 2;
  }
  if (v) {
    //@ assert \dangling(&x);
    int j = *x + 1;
  }
}
