extern int f(void);
extern int g(void);

int non_compliant_1() {
  if (!f() & g() == 0) return 1; else return 0;
}

int non_compliant_2() {
  int x = f();
  int y = g();
  if ((x++  == 0)| (y-- == 0)) return 1; else return 0;
}

int non_compliant_3() {
  _Bool b = f();
  _Bool c = g();
  if (b ^ c) return 1; else return 0;
}

int compliant_1() {
  if (!f() && g() == 0) return 1; else return 0;
}

int compliant_2() {
  int x = f();
  int y = g();
  if (x++ == 0 || y-- == 0) return 1; else return 0;
}

int compliant_3 () {
  _Bool b = f();
  _Bool c = g();
  if ((b || c) && !(b && c)) return 1; else return 0;
}

int compliant_4() {
  _Bool b = f();
  _Bool c = g();
  // Parenthesized expression indicates that bitwise operand is intended.
  if ((b ^ c)) return 1; else return 0;
}
