/* run.config*
   STDOPT: +" -slevel-function main8_aux:2,main5_bis:4 -value-msg-key d-gauges"
*/

volatile v;

void main0() {
  int i = 1;
  int j = 2;
  int k = 3;
  int l = 4;
  

  while (k <= 100) {
    i = 0;
    while (i <= 160) {
      Frama_C_show_each_0("in");
      if (! (i <= 160)) break; // exit condition depends only on one loop
      Frama_C_show_each_1("in");
      i += 3;
      j -= 4;
    }
    k += 2;
    l += 1;

  }
}

void main0_bis() {
  int i = 1;
  int j = 2;
  int k = 3;
  int l = 4;
  

  while (k <= 100) {
    //    i = 0;
    while (i <= 160) {
      Frama_C_show_each_0("in");
      if (! (i <= 160)) break; // exit condition depens on two loops
      Frama_C_show_each_1("in");
      i += 3;
      j -= 4;
    }
    k += 2;
    l += 1;

  }
}

int t[38];

void main1() {
  int j = 5;
  for (int i = 0; i<50; i+=3) {
    t[j] = i;
    Frama_C_show_each("in");
    j+= 2;
  }
  Frama_C_show_each("out");
}

int u[100];

void main2() {
  
  int *p = u;
  for (int i = 100; i>0; i--) {
    *(p++) = i;
    Frama_C_show_each();
  }
}

void main3() {
  int k = 0;
  for (int i = 0; i <15; i++) {
    for (int j = 0; j < 25; j++) {
      Frama_C_show_each("inner");
      k++;
    }
    Frama_C_show_each("outer");
    k ++;
  }
  Frama_C_show_each(k);
}

extern  int T[100];

long main4_search() {
  long c;
  long i;
  c = ( long)0;
  i = ( long)0;
  while (i < ( long)21) {
    Frama_C_show_each();
    if (T[i] == ( int)0x3000) {
      c ++;
    }
    i ++;
  }
  return c;
}

void main4 () {
  long i = main4_search();
}

void main5() { // This test checks what happens when a pointer changes base
  int x[10], y[10];
  int *p = x;
  for (int i = 0; i<10; i++) {
    if (i == 3)
      p = y;
    Frama_C_show_each(p);
    *p = i;
    p++;
  }
  p = p;
}

void main5_bis() { // identical to main5, but partly unrolled. In this case, we can infer something on the last iterations, and remain precise
  int x[10], y[10];
  int *p = x;
  for (int i = 0; i<10; i++) {
    if (i == 3)
      p = y;
    Frama_C_show_each(p);
    *p = i;
    p++;
  }
  p = p;
}

void main6() {
  int i = 0, j = 0;
  while (i <= 12) {
    i++;
    j++;
  }
}

void main7_aux (unsigned int toCopy) {
  Frama_C_show_each();
   int *p = &T[99];
   while( toCopy-- > (0u) )
   {
     *p-- = 1; // Currently, alarm, because we lose information on toCopy because of the underflow on the last iteration, and then on toCopy
   }
}

void main7_aux2 (signed int toCopy) {
  Frama_C_show_each();
   int *p = &T[99];
   while( toCopy-- > 0 )
   {
     *p-- = 1; // No pointer alarm, but underflow on toCopy above
   }
}

void main7() {
  unsigned int toCopy = 100U;
  main7_aux(toCopy);
  main7_aux2(toCopy);
}

void main8_aux (unsigned int n) {
  int arr[65536];
  int *p = arr;
  do {
    Frama_C_show_each(n);
    *p++ = n;
  } while (--n);
}

void main8() {
  main8_aux(0);
}

void main9() {
  int x[10], y[10];
  int *p = x;
  int *q = y;
  int z;
  for (int i=0; i<10; i++) {
    if (i >= 3) {
      z = (int)p + (int)q; // Do not build gauges with multiple variables
      int *r = z;
      *r = 1;
    }
    *p = i;
    *q = -i;
    p++;
    q++;
  }
}

float main10_aux(float* p, const float* A, const float* B, int n) {
  int numNonZero = n - 1;
  while (numNonZero-- > 0) { // Works only with '> 0', the orinal code was 'numNonZero--' only. Also, underflow on numNonZero. In both cases, we need to stop at 0
    *p *= (*A++) * (*B++); // Requires very powerful relations no to overflo on floating-point values here...
  }
  return *p;
}

int main10() {
  float p = 1;
  float A[10] = {1};
  float B[10] = {2};
  return (int)main10_aux(&p, A, B, 10);
}

void main11 () {
  int n = 100;
  int i = 0;
  do {
    Frama_C_show_each();
    i++;
  } while(n-- > 0);
}

void main12() {
  int i, j;
  for (i=0, j=0; j<10 ; i++) { // Exit condition depends on j, which is incremented randomly. Nothing can be derived for the lower bound of i;
    if (v) j++;
  }
  i = i; j = j;
}

/* functions main13_* test the backward propagation when the exit condition
   does not correspond to an integral number of iterations */

void main13_1() {
  int i = (v ? 5 : 6);
  int j = 0;
  
  while (i <= 44) { // 6 iterations, regardless of the initial value of i 
    i += 7;
    j += 1; 
  }
  Frama_C_show_each(i, j);
}

void main13_2() {
  int i = (v ? 5 : 6);
  int j = 0;
  
  while (i <= 47) { // 6 or 7 iterations, depending on i
    i += 7;
    j += 1;
  }
  // We obtain an interval for i, but only two values are really possible
  Frama_C_show_each(i, j);
}

void main13_3() {
  int i = (v ? 5 : 6);
  int j = 0;
  
  while (i >= -52) {
    i -= 7;
    j += 1; 
  }
  Frama_C_show_each(i, j);
}

void main13_4() {
  int i = (v ? 5 : 6);
  int j = 0;
  
  while (i >= -57) {
    i -= 7;
    j += 1;
  }
  Frama_C_show_each(i, j);
}

void main13 () {
  main13_1();
  main13_2();
  main13_3();
  main13_4();
}

void main14() {
  int i = 5;
  int s = v ? 5:6;
  int j = 0;

  while (i >= -587) {
    i -= s;
    j += 1;
    //@ slevel merge;
    ;
  }
  Frama_C_show_each(i, j);
}

void main15 () {
  int x, y;
  int *p = &x;
  int i = 0;
  while(i <= 10) {
    i++;
    Frama_C_dump_each(); // After the first iteration, nothing should be known on p
    p = &y;
  }
}

void main16() {
  /* Bug found by Csmith. Widening was incorrectly implemented, and returned
     false results when a pointer points to multiple bases in multiple
     iterations. Nested loops were probably needed for the bug to manifest
     itself. */
  int a = 1;
  int b = 1;
  int *p = &a;
 L1: // This is a loop head
  b = 0;
  while (b < 1) {
    int i;
    for (i = 0; i < 3; i++);
    for (i = 0; i < 2; i++);
    Frama_C_dump_each();
    for (i = 0; i < 1; i++);
    while (i < 3) {
      if (*p) {
        p = &b;
        goto L1;
      } else
        return;
    }
  }
}

void main() {
  main0();
  main0_bis();
  main1();
  main2();
  main3();
  main4();
  main5();
  main5_bis();
  main6();
  main7();
  main8();
  main9();
  main10();
  main11();
  main12();
  main13();
  main14();
  main15();
  main16();
}
