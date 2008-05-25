
void f1(char* t1, char* t2, int s) {
  int i;
  for (i = 0; i < s; ++i) t1[i] = t2[i];
}

void f2(char* t1, char* t2, int s) {
  int i;
  for (i = 0; i < s; ++i) t1[i] = t1[i+1];
}

void f3(char* t1, char* t2, int s) {
  int i;
  for (i = 0; i < s; ++i) t1[i] = t2[i+1];
}

void f4(char* t1, char* t2, int s) {
  int i = 0;
  while (i < s) { t1[i] = t2[i]; ++i; }
}

void f5(char* t1, char* t2, int s) {
  int i = 0;
  while (i < s) { t1[i] = t1[i+1]; ++i; }
}

void f6(char* t1, char* t2, int s) {
  int i = 0;
  while (i < s) { t1[i] = t2[i+1]; ++i; }
}

int main() {
  char t1[] = "12345";
  char t2[] = "12345";
  f1(t1,t2,5);
  f2(t1,t2,5);
  f3(t1,t2,5);
  f4(t1,t2,5);
  f5(t1,t2,5);
  f6(t1,t2,5);
  return 0;
}

/* Local Variables: */
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int string2.c" */
/* End: */
