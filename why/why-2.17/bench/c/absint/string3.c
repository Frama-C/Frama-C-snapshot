
void f1(char* t1, char* t2) {
  int i;
  if (t1 != (char*)0 && !(t2 == 0)) 
    for (i = 0; i < 5; ++i) t1[i] = t2[i];
}

void f2(char* t1, char* t2) {
  int i;
  if (t1 != (char*)0 && !(t2 == 0)) 
    for (i = 0; i < 5; ++i) t1[i] = t1[i+1];
}

void f3(char* t1, char* t2) {
  int i;
  if (t1 != (char*)0 && !(t2 == 0)) 
    for (i = 0; i < 5; ++i) t1[i] = t2[i+1];
}

void f4(char* t1, char* t2) {
  int i = 0;
  if (t1 != (char*)0 && !(t2 == 0)) 
    while (i < 5) { t1[i] = t2[i]; ++i; }
}

void f5(char* t1, char* t2) {
  int i = 0;
  if (t1 != (char*)0 && !(t2 == 0)) 
    while (i < 5) { t1[i] = t1[i+1]; ++i; }
}

void f6(char* t1, char* t2) {
  int i = 0;
  if (t1 != (char*)0 && !(t2 == 0)) 
    while (i < 5) { t1[i] = t2[i+1]; ++i; }
}

int main() {
  char t1[] = "12345";
  char t2[] = "12345";
  f1(t1,t2); f1(t1,0); f1(0,t2); f1(0,0);
  f2(t1,t2); f2(t1,0); f2(0,t2); f2(0,0);
  f3(t1,t2); f3(t1,0); f3(0,t2); f3(0,0);
  f4(t1,t2); f4(t1,0); f4(0,t2); f4(0,0);
  f5(t1,t2); f5(t1,0); f5(0,t2); f5(0,0);
  f6(t1,t2); f6(t1,0); f6(0,t2); f6(0,0);
  return 0;
}

/* Local Variables: */
/* compile-command: "caduceus -print-norm --loc-alias --arith-mem --abs-int string3.c" */
/* End: */
