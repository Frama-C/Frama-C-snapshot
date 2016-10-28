/* run.config
   OPT: -val -cpp-extra-args=-DSYNTAX_ERRORS -continue-annot-error
   OPT: -val -cpp-extra-args=-DNONCONST
   OPT: -val -slevel 1 -kernel-msg-key widen-hints
   OPT: -val -cpp-extra-args=-DALLGLOBAL -kernel-msg-key widen-hints
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

  return 0;
}
#endif
