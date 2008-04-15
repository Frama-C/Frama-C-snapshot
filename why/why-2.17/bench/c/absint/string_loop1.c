
void f1(char* t1, char* t2) {
  int i;
  for (i = 0; i < 5 && t1[i] && t2[i]; ++i) t1[i] = t2[i];
}

void f2(char* t1, char* t2) {
  int i;
  for (i = 0; i < 5 && t1[i] && t1[i+1]; ++i) t1[i] = t1[i+1];
}

void f3(char* t1, char* t2) {
  int i;
  for (i = 0; i < 5 && t1[i] && t2[i+1]; ++i) t1[i] = t2[i+1];
}

void f4(char* t1, char* t2) {
  int i = 0;
  while (i < 5 && t1[i] && t2[i]) { t1[i] = t2[i]; ++i; }
}

void f5(char* t1, char* t2) {
  int i = 0;
  while (i < 5 && t1[i] && t1[i+1]) { t1[i] = t1[i+1]; ++i; }
}

void f6(char* t1, char* t2) {
  int i = 0;
  while (i < 5 && t1[i] && t2[i+1]) { t1[i] = t2[i+1]; ++i; }
}

int main() {
  char t1[] = "12345";
  char t2[] = "12345";
  f1(t1,t2);
  f2(t1,t2);
  f3(t1,t2);
  f4(t1,t2);
  f5(t1,t2);
  f6(t1,t2);
  return 0;
}

/* Local Variables: */
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int string_loop1.c" */
/* End: */
