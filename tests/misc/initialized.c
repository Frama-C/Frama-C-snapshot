extern int b1, b2, b3;

int main () {
  int r1, x1, x2, r2, x3, r3;
  
  if (b1) x1 = 1;
  //@ assert \initialized(&x1);
  r1 = x1+1;

  if (b2)
    x2 = r2 + 1;

  if (b3) x3 = 1;
  r3 = x3 + 1;

  return 0;
}
