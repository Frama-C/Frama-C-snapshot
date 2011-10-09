/* run.config
   EXECNOW: make -s tests/slicing/simple_intra_slice.opt
   CMD: tests/slicing/simple_intra_slice.opt
   OPT: -check -deps -slicing-level 2 -no-slice-callers -journal-disable
*/
int Unknown;
int G;


/* on sélectionne le return.
   on doit garder juste a (pas G et b) */
int f1 (int x, int y) {
  int a = 1, b = 2;
  G = x + a;
  return y + b;
}
/* on sélectionne le return.
   pas de réduction intreproc -> b doit être marqué Spare
   et recursivement a aussi.
*/
int f2 (void) {
  int a = 1, b = a+1, c = 3;
  return f1 (b, c);
}

/* avec un IF : sélection de la branche then */
int f3 (int c) {
  int a = 1, b = 2;
  int x = 0;
  if (c > Unknown)
    x = b;
  else
    G = a;
  return x;
}
/* avec un IF : sélection de la branche else */
int f4 (int c) {
  int a = 1, b = 2;
  int x = 0;
  if (c > Unknown)
    G = a;
  else
    x = b;
  return x;
}

int f5 (int c) {
  int x = 0;
  if (c > Unknown) goto Lsuite;
  x += 1;
Lsuite : if (c < Unknown) goto L2;
  G++;
L2 : x += 1;
  return x;
}

int f6 (int n) {
  int i = 0;
  while (n < 10) {
    if (Unknown > 3) {
      i = 1;
      break;
      }
    if (n%2)
      continue;
    n++;
  }
  if (i)
    return 0;
  else
    return 10*n;
}
typedef struct { int a; int b; int c; } Tstr;
Tstr S, S1, S2;

void f7 (Tstr s0) {
  int x = S.a;
  if (x > 0) {
    S.a += 3;
  }
  else {
    s0.a += 1;
    S = s0;
  }
}
void f8 (Tstr * ps) {
  ps->a ++;
  ps->b ++;
}


int main (void) {
  int res = 0;

  /* make Unknown really unknown */
  volatile int uninit=0, uninit2=0 ;

  while(uninit)
    if (uninit-1) Unknown++; else Unknown--;
  while(uninit2)
    if (uninit2-1) S.a++; else S.a--;

  res += f2 ();
  res += f3 (1);
  res += f4 (1);
  res += f5 (1);
  res += f6 (Unknown);
  f7 (S);
  if (Unknown)
    f8 (&S1);
  else
    f8 (&S2);
  return res;
}
