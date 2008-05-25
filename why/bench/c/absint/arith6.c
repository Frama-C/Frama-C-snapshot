
void f1(int* p1, int* p2, int s) {
  int i;
  for (i = 0; i < s; ++i)
    if (p1) *p1++ = *p2++;
}

void f2(int* p1, int* p2, int s) {
  int i;
  for (i = 0; i < s; ++i) 
    if (p1 != 0) { *p1 = *(p1 + 1); p1++; }
}

void f3(int* p1, int* p2, int s) {
  int i;
  for (i = 0; i < s; ++i) 
    if (!(p1 == 0)) { *p1++ = *(p2++ + 1); }
}

void f4(int* p1, int* p2, int s) {
  int i = 0;
  while (i < s) 
    if (p1) { *p1++ = *p2++; ++i; }
}

void f5(int* p1, int* p2, int s) {
  int i = 0;
  while (i < s) 
    if (p1 != 0) { *p1 = *(p1 + 1); p1++; ++i; }
}

void f6(int* p1, int* p2, int s) {
  int i = 0;
  while (i < s)
    if (!(p1 == 0)) { *p1++ = *(p2++ + 1); ++i; }
}

int main() {
  int t1[6];
  int t2[6];
  // Calls with [p1] null are commented out. Indeed, code above is testing 
  // nullity of [p1], which is incremented in the loop !
  // Therefore it is incorrect to call these functions with [p1] null.
  f1(t1,t2,5); // f1(0,t2,4); f1(0,0,-1);
  f2(t1,t2,5); // f2(0,t2,4); f2(0,0,-1);
  f3(t1,t2,5); // f3(0,t2,4); f3(0,0,-1);
  f4(t1,t2,5); // f4(0,t2,4); f4(0,0,-1);
  f5(t1,t2,5); // f5(0,t2,4); f5(0,0,-1);
  f6(t1,t2,5); // f6(0,t2,4); f6(0,0,-1);
  return 0;
}

/* Local Variables: */
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int arith6.c" */
/* End: */
