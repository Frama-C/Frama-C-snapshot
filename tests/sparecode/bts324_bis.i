/* run.config
   OPT: -sparecode-analysis -sparecode-debug 1 -journal-disable
   OPT: -sparecode-analysis -sparecode-debug 1 -main main_bis -journal-disable
   OPT: -sparecode-analysis -sparecode-debug 1 -sparecode-no-annot -journal-disable
*/

int ki[2], k ;
int f (int vi, int i) {
  static int si[2] = 0, so[2] = 0;
  int vo = so[i]/k + ki[i]*(vi - si[i]) ;

  so[i] = vo ;
  si[i] = vi ;
  return vo ;
}

int volatile e0,e1;
int s0, s1 ;
void loop_body (void) {
  int acq0 = e0 ;
  int acq1 = e1;
  int val0 = f (acq0, 0) ;
  int val1 = f (acq1, 1) ;
  s0 = val0 ;
  s1 = val1 ;
}

int is_ok ;
void init (int *pres) {
  ki[0] = 2 ;
  ki[1] = 4 ;
  k = 8 ;
  *pres = 1 ;
}

void main (int c) {
  init (& is_ok);
  if (is_ok)
    while (1) {
      loop_body () ;
      // note: sparecode conserve les pragmas de slicing et par conséquent ce
      // qui calcule "s0", l'option -sparecode-no-annot ni change rien
      //@ impact pragma expr s0;
      //@ slice pragma expr s1;
      }

}


void main_bis (int c) {
  init (& is_ok);
  if (is_ok)
    while (1) {
      loop_body () ;
      }

}
