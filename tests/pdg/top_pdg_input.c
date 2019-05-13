/* run.config
   STDOPT: +"-eva -pdg -out -input -deps -no-results-function no_results -eva-no-builtins-auto -load-module pdg -pdg -then -main main_asm"
*/
volatile int nondet;
int no_results() {return 1;}

int tab[2] = {0, 7};
typedef struct {int a; int t[5]; } Ts;
Ts S;
int G;

int f1 (void) {
  int i = no_results(); /* InTop element of PDG */
  int v = nondet ? tab[i] : i;
  G ++;
  return v;
}

int f2 (void) {
  int i = no_results(); /* InTop element of PDG */;
  Ts s;
  S.a = 2;
  s = S;
  return s.a + (nondet ? s.t[i] : i);
}

int strlen(char* p ) {
  char* q ;
  int k = 0;

  for (q = p; *q ; q++) k++ ;

  return k;
}


int main (char *p_str[]) {
  int i = f1 ();
  i += f2 ();
  return strlen (nondet ? p_str[i] : p_str[0]);
}


int fun_asm(i) {
  asm("BLA");
  return i+1;
}

int main_asm () {
  int j = 3;
  return fun_asm(j);
}
