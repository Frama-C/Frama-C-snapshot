// Test a change in [lv] during a call [lv = f()]. Currently inactive

int x, y;
int *p;

volatile int v;

int f() {
  p = &y;
  return 1;
}

void main1() {
  p = &x;
  *p = f(); // Warn
}

int g() {
  int z = *p;
  return 1;
}

void main2() {
  p = &y;
  if (v)
    p++;
  *p = g(); //Do not warn (even though p is {&y, &y+1} before and {&y} after)
}

void main() {
  main1();
  main2();
}
