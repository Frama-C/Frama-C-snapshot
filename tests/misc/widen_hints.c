/* run.config
   OPT: -val -val-show-progress -cpp-extra-args=-DSYNTAX_ERRORS -continue-annot-error
   OPT: -val -val-show-progress -cpp-extra-args=-DNONCONST
   OPT: -val -val-show-progress -slevel 1 -value-msg-key widen-hints
   OPT: -val -val-show-progress -cpp-extra-args=-DALLGLOBAL -value-msg-key widen-hints
*/
#define N 2

const int x = 9;
int not_const = 42; // cannot be used as widen hint

#ifdef SYNTAX_ERRORS
int main1() {
  /*@ widen_hints x; */ // error: no hints
  return 0;
}

int main2() {
  /*@ widen_hints 1; */ // error: no variable
  return 0;
}

int main3() {
  /*@ widen_hints x, b, 1; */ // error: b must be a constant value
  return 0;
}

int main() {
  /*@ widen_hints x, not_const; */ // error: not_const not a global constant
  return 0;
}

#else

#ifdef ALLGLOBAL
int f() {
  int m = 10;
  int n = 33+m;
  int t[100];
  // global:"all" hints should apply here
  for (int a = 0; a < n*2+1; a++) {
    for (int b = 0; b < a; b++) {
      t[b] = 1;
    }
  }
  return 0;
}
#endif

#ifdef EXTGLOBAL
// ext_i and external_f are defined in widen_hints_external.c
void external_f();
#endif

void using_dynamic_global(int *i) {
  int b;
  //@ widen_hints *i, 87; //note: b itself is NOT in the hint
  for (b = 0; b < *i; b++) {
  }
}

int main() {
#ifdef NONCONST
  const int local_const = 17; // cannot be used as widen hint
  /*@ widen_hints x, local_const; */ // error: local_const not a global constant
#endif
  int y;
  int m = 10;
  int n = 33+m;
  // without hints for a, there is a signed overflow
  //@ loop widen_hints a, (N+(6*x)+118)/2;
  for (int a = 0; a < n*2+1; a++) {
    for (int b = 0; b < a; b++) {

    }
  }

#ifdef ALLGLOBAL
  /*@ widen_hints global:"all", 88; */
  f();
#endif

  struct st {
    int i;
    double d;
  } ss;
  //@ widen_hints ss.i, 87;
  for (ss.i = 0; ss.i < n*2+1; ss.i++) {
    for (int b = 0; b < ss.i; b++) {

    }
  }

  int ip = 0;
  int *p = &ip;
  //@ widen_hints *p, 87;
  for (*p = 0; *p < n*2+1; (*p)++) {
    for (int b = 0; b < *p; b++) {

    }
  }

  int ip2 = 0;
  int *p2 = &ip2;
  int **pp = &p2;
  //@ widen_hints **pp, 87;
  for (**pp = 0; **pp < n*2+1; (**pp)++) {
    for (int b = 0; b < **pp; b++) {

    }
  }

  typedef struct { int i; } istruct;
  istruct iarray[2] = {{0}, {0}};
  istruct *piarray[2] = {&iarray[0], &iarray[1]};
  for (piarray[1]->i = 0; piarray[1]->i < n*2+1; (piarray[1]->i)++) {
    //@ widen_hints piarray[1]->i, 87;
    for (int b = 0; b < piarray[1]->i; b++) {
    }
  }

  int outer_i;
  for (outer_i = 0; outer_i < n*2+1; outer_i++) {
    using_dynamic_global(&outer_i);
  }

  return 0;
}
#endif
