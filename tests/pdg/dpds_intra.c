/* run.config
   GCC:
   OPT: -fct-pdg test_struct -journal-disable -pdg-print -pdg-verbose 2
   OPT: -fct-pdg test_if_simple -journal-disable -pdg-print -pdg-verbose 2
   OPT: -fct-pdg test_goto_simple -journal-disable -pdg-print -pdg-verbose 2
   OPT: -fct-pdg test_goto_arriere -journal-disable -pdg-print -pdg-verbose 2
   OPT: -fct-pdg test_goto_else -journal-disable -pdg-print -pdg-verbose 2
   OPT: -main test_ctrl_dpd_multiple  -journal-disable -pdg-print -pdg-verbose 2
        => ne passe pas
   OPT: -fct-pdg test_simple_loop -journal-disable -pdg-print -pdg-verbose 2
   OPT: -fct-pdg main -journal-disable -pdg-print -pdg-verbose 2
   OPT: -fct-pdg multiple_global_inputs -journal-disable -pdg-print -pdg-verbose 2
*/
/* bin/toplevel.opt -deps -main g tests/slicing/dpds_intra.c */
/* bin/toplevel.opt -fct-pdg test_goto_simple tests/slicing/dpds_intra.c -pdg-dot */

extern int G;

typedef struct {
  int a;
  int b;
} Tstr;

extern Tstr S;

int test_struct (void) {
  Tstr s1, s2;
  s1.a = 1;
  /* s1.b = 2; */
  s2 = s1;
  return s2.a;
}

int multiple_global_inputs (void) {
  return S.a + G;
}

int test_if_simple (void) {
  int x0 = 0, x1 = 1, x2 = 2, x3 = 10, x;
  if (G < x0) /* G < 0 */
    x = x0;
  else /* G >= 0 */ if (G < x1) /* G < 1 */
    x = x1;
  else /* G >= 1 */ if (G > x2) { /* G > 2 */
    if (G < x3)
      x = x3;
    else
      x = -1;
    }
  // pas de else
  return x;
}

int test_goto_simple (void) {
  int r;
  if (G > 0) goto Lelse;
  r = -1;
  goto Lfin;
  Lelse : r = 1;
  Lfin : return r;
}

int test_goto_arriere (void) {
  int x = 1;
  L : x++;
  if (G-- > 0) goto L;
  return x;
}

int test_goto_else (void) {
  int x, a, b = 0;
  if (G) {
    x = 1;
    goto L;
    }
  else {
    a = 1;
    L : b = 1;
    }
  return b;
}

/* ne passe pas l'analyse de valeur (bouclage)
./bin/toplevel.opt  -val -main test_ctrl_dpd_multiple tests/slicing/dpds_intra.c
 * cf. mail Pascal Re: loop_pragma UNROLL_LOOP du 09.05.2006 15:03 */
int test_ctrl_dpd_multiple (void) {
  int x = 0;
  if (G > 3)
    x = 1;
  else
    L : x = x - 2;
  if (G < x)
    goto L;
  return x;
}

int test_simple_loop (int n) {
  int i, s = 0;
  for (i = 0; i < n; i++) {
    s++;
    }
  return s;
}


int main (void) {
  int a;
  int b = G;
  int res = 0;
  Tstr s = { 1, 2 };
  int *p, *q;

  a = b++ + s.a;
  b = 2*a;

  if (b > G)
    p = &a;
  else {
    int a = 1;
    p = &b;
    a++;
    }

  *p += 1;

  res += test_struct ();
  res += test_if_simple ();
  res += test_goto_simple ();
  res += test_goto_arriere ();
  res += test_goto_else ();
  res += test_simple_loop (G);
  res += multiple_global_inputs ();

  return *p + res;
}
