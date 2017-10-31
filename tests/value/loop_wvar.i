/* run.config*
   OPT: -no-autoload-plugins -load-module value,inout -no-annot -val @VALUECONFIG@ -then -continue-annot-error -annot -val -journal-disable
   OPT: -no-autoload-plugins -load-module from,inout,value -continue-annot-error  -val @VALUECONFIG@ -main main3 -journal-disable
   OPT: -no-autoload-plugins -load-module value,inout -continue-annot-error  -val @VALUECONFIG@ -main main_err1 -journal-disable
   OPT: -no-autoload-plugins -load-module value,inout -continue-annot-error -val @VALUECONFIG@ -main main_err2 -journal-disable
*/


void main(void)
{ int n = 13;
  int i,j;
// ceci était une annotation, mais on ne fait pas moins bien sans
// maintenant:
// loop pragma WIDEN_VARIABLES i;
  /*@ loop widen_hints i, 12, 13; */
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}


void main_err1(void)
{ int n = 13;
  int i,j;
  /*@ loop widen_hints "all", 12 ; */
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}



void main_err2(void)
{ int n = 13;
  int i,j;
  /*@ loop pragma WIDEN_VARIABLES 12; */
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}


void main_unhelpful () {
  int max = 25;
  int next = 0;
  int i;

/*@ loop widen_hints next, 24; */ // This pragma is unhelpful, but used to interfere with the bound for i.
  for (i=0;i<30;i++) {
    int vsize = max;
    int vnext = next;

    if(vsize > vnext)
      next++;
  }
}

void main_multiple_hints () {
  int maxj = 17;
  int maxk = 11;
  int j = 0;
  int k = 0;

  //@ loop widen_hints j, 17; loop widen_hints k, 11;
  // 18 and 12 are actually better bounds in this case (one less iteration)
  for (int i=0; i<10; i++) {

    Frama_C_show_each(i, j, k);

    if (j <= maxj) {
      j++;
    }
    if (k <= maxk) {
      k++;
    }

  }
}

void main3() {
  main_unhelpful ();
  main_multiple_hints ();
}
