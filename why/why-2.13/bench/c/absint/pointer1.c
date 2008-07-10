
void f1(int* t1, int* t2) {
  int i;
  for (i = 0; i < 5; ++i) t1[i] = t2[i];
}

void f2(int* t1, int* t2) {
  int i;
  for (i = 0; i < 5; ++i) t1[i] = t1[i+1];
}

void f3(int* t1, int* t2) {
  int i;
  for (i = 0; i < 5; ++i) t1[i] = t2[i+1];
}

void f4(int* t1, int* t2) {
  int i = 0;
  while (i < 5) { t1[i] = t2[i]; ++i; }
}

void f5(int* t1, int* t2) {
  int i = 0;
  while (i < 5) { t1[i] = t1[i+1]; ++i; }
}

void f6(int* t1, int* t2) {
  int i = 0;
  while (i < 5) { t1[i] = t2[i+1]; ++i; }
}

int main() {
  int t1[6];
  int t2[6];
  f1(t1,t2);
  f2(t1,t2);
  f3(t1,t2);
  f4(t1,t2);
  f5(t1,t2);
  f6(t1,t2);
  return 0;
}

/* Local Variables: */
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int pointer1.c" */
/* End: */
