
void f1(int* p1, int* p2, int s) {
  int i;
  for (i = 0; i < s; ++i) *p1++ = *p2++;
}

void f2(int* p1, int* p2, int s) {
  int i;
  for (i = 0; i < s; ++i) { *p1 = *(p1 + 1); p1++; }
}

void f3(int* p1, int* p2, int s) {
  int i;
  for (i = 0; i < s; ++i) { *p1++ = *(p2++ + 1); }
}

void f4(int* p1, int* p2, int s) {
  int i = 0;
  while (i < s) { *p1++ = *p2++; ++i; }
}

void f5(int* p1, int* p2, int s) {
  int i = 0;
  while (i < s) { *p1 = *(p1 + 1); p1++; ++i; }
}

void f6(int* p1, int* p2, int s) {
  int i = 0;
  while (i < s) { *p1++ = *(p2++ + 1); ++i; }
}

int main() {
  int t1[6];
  int t2[6];
  f1(t1,t2,5);
  f2(t1,t2,5);
  f3(t1,t2,5);
  f4(t1,t2,5);
  f5(t1,t2,5);
  f6(t1,t2,5);
  return 0;
}

/* Local Variables: */
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int arith2.c" */
/* End: */
