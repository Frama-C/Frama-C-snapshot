
void f1(int* p1, int* p2) {
  int i;
  for (i = 0; i < 5; ++i) *p1++ = *p2++;
}

void f2(int* p1, int* p2) {
  int i;
  for (i = 0; i < 5; ++i) { *p1 = *(p1 + 1); p1++; }
}

void f3(int* p1, int* p2) {
  int i;
  for (i = 0; i < 5; ++i) { *p1++ = *(p2++ + 1); }
}

void f4(int* p1, int* p2) {
  int i = 0;
  while (i < 5) { *p1++ = *p2++; ++i; }
}

void f5(int* p1, int* p2) {
  int i = 0;
  while (i < 5) { *p1 = *(p1 + 1); p1++; ++i; }
}

void f6(int* p1, int* p2) {
  int i = 0;
  while (i < 5) { *p1++ = *(p2++ + 1); ++i; }
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
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int arith1.c" */
/* End: */
