/* run.config
   STDOPT: +"-report -then -slevel 10"
*/


int S=0;
int T[5];
int n = 1;

void loop () {
  int i;
  int *p = &T[0] ;
  for (i=0; i<5; i++)
    { S = S+i; *p++ = S; }
  return S;
}

void compute_n () {
  for (int i=1; i <= 5; i++) {
    n *= i;
  }
}

void init_p (int *p) {
  if (n == 120)
    *p = 0;
}

void initialized_p (int *p) {
  int x = *p + 1;
}

int main(void) {
  loop ();

  compute_n ();

  int x, y;

  init_p (&x);
  initialized_p (&x);

  init_p (&y);
  initialized_p (&y);
}

