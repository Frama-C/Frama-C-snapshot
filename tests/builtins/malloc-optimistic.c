/* run.config*
   STDOPT: +"-slevel 30 -val-slevel-merge-after-loop @all -val-malloc-functions malloc -memexec-all"
*/

//@ assigns \result \from \nothing;
void *malloc(unsigned long size);

//@ assigns \nothing; frees p;
void free(void *p);

volatile v;

void* main1() {
  int i, *p;
  i = v ? 0 : 1;
  Frama_C_show_each_1_1(i);
  p = malloc ((i+1)*sizeof(int));
  Frama_C_show_each_1_2(i);
  p[i] = i;
  return p;
}

void* main2() {
  int i, *p;
  i = v ? 2 : 1;
  Frama_C_show_each_2_1(i);
  p = malloc ((i+1)*sizeof(int));
  Frama_C_show_each_2_2(i);
  p[i] = i;
  return p;
}

void* main_3_aux(int i) {
  int *p = malloc ((i+1)*sizeof(int));
  p[i] = i;
  return p;
}

void* main_4_aux(int i) {
  int *p = malloc ((i+1)*sizeof(int));
  p[i] = i;
  return p;
}

void* main3() {
  int i, *p;
  i = v ? 0 : 1;
  p = main_3_aux(i);
  return p;
}

void* main4() {
  int i;
  int *p;
  i = v ? 2 : 1;
  p = main_4_aux(i); // a size of 2 is allocated first, then of 1, all accesses succeed
  return p;
}

void* main5() {
  int i, *p;  
  for (int j=0; j<10; j++) {
    i = v ? j : j+1;
    p = malloc ((i+1)*sizeof(int));
    p[i] = j;
    int k = p[i] - 2;
    Frama_C_dump_each();
    free(p);
  }
  return 0;
}

void* main6() {
  int i, *p;  
  for (int j=0; j<10; j++) {
    i = v ? j : j+1;
    p = malloc ((i+1)*sizeof(int));
    p[i] = j;
    int k = p[i] - 2;
    Frama_C_dump_each();
    if (i == j) free(p); // Leak, the variable allocated in p will become weak
  }
  return 0;
}

void* main7() {
  int i, *p;  
  for (int j=0; j<100; j++) {
    i = v ? j : j+1;
    p = malloc ((i+1)*sizeof(int));
    p[i] = j;
    int k = p[i] - 2;
    Frama_C_dump_each();
    free(p);
  }
  return 0;
}

void* main8() {
  int i, *p;  
  for (int j=0; j<100; j++) {
    i = v ? j : j+1;
    p = malloc ((i+1)*sizeof(int));
    p[i] = j;
    int k = p[i] - 2;
    Frama_C_dump_each();
    if (i == j) free(p); // Leak, the variable allocated in p will become weak
  }
  return 0;
}

void main9_aux(int *p) {
  *p = -20;
}

void* main9() {
  int i, *p;  
  for (int j=0; j<100; j++) {
    p = malloc (4);
    Frama_C_show_each(j,p);
    *p = j;
    Frama_C_show_each(*p);
    main9_aux(p);
  }
  return 0;
}


void main() {
  void *p;
  /* main1 and main2 try to see if slevel makes a difference w.r.t to the
     allocation of bases. Currently, the answer is now, because the propagation
     strategy evaluates the two states on 'malloc', then the resulting states
     on 'p[i]=i'. See the difference with main3 and main4. */
  p = main1();
  free(p);
  p = main2();
  free(p);
  p = main3();
  free(p);
  p = main4();
  free(p);

  main5(); // Well-parenthesized loop with allocation/free: variable remains strong
  main6(); // Ill-parenthesized loop with allocation/free: variable becomes weak

  // Same as above, with bigger loops (not fully unrolled)
  main7();
  main8();
  main9();
}
