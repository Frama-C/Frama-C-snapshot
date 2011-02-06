/* run.config
   OPT: -val -out -input -deps -pdg -journal-disable
*/

int ** top_input() ;

int tab[2] = {0, 7};
typedef struct {int a; int t[5]; } Ts;
Ts S;
int G;

int f1 (void) {
  int i = **top_input(); /* InTop element of PDG */
  int v = tab[i];
  G ++;
  return v;
}

int f2 (void) {
  int i = **top_input(); /* InTop element of PDG */;
  Ts s;
  S.a = 2;
  s = S;
  return s.a + s.t[i];
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
  return strlen (p_str[i]);
}
