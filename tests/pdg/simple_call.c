/* run.config
   GCC:
   OPT: -fct-pdg main -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -main call_in_loop -fct-pdg call_in_loop -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -main call_mix_G1_G2 -fct-pdg call_mix_G1_G2 -journal-disable  -pdg-print -pdg-verbose 2
   OPT: -main call_multiple_global_outputs -fct-pdg call_multiple_global_outputs -journal-disable  -pdg-print -pdg-verbose 2
*/

extern int G, G1, G2;

typedef struct {
  int a;
  int b;
} Tstr;

extern Tstr S, S1, S2;

/*----------------------------------------------*/
/* check if we don't mix up inputs and outputs */
void mix_G1_G2 (void) {
  int tmp = G1;
  G1 = G2;
  G2 = tmp;
}

int call_mix_G1_G2 (void) {
  int x1, x2;
  mix_G1_G2 ();
  x1 = G1;
  x2 = G2;
  return x1+x2;
}
/*----------------------------------------------*/
void multiple_global_outputs (int x, int y) {
  S.a = x;
  G = y;
}
void call_multiple_global_outputs (int x, int y) {
  multiple_global_outputs (x, y);
}

/*----------------------------------------------*/

int call (int x, int y) {
  G += y;
  return x;
}

int call_in_loop (int c) {
  int i, a = 0;
  for (i = 0; i < G; i++)
    a += call (i, c);
  return a;
}

int main (void) {
  int a = 0, b = 1, c = 3;
  int i;
  a = call (a+b, b+c);
  return a;
}
/*----------------------------------------------*/
