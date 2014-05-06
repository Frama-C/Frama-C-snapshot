/* run.config
   GCC:
   OPT: -no-annot -val -journal-disable
   OPT: -val -journal-disable
   OPT: -val -main main_err1 -journal-disable
   OPT: -val -main main_err2 -journal-disable
*/

int i,j;

void main(void)
{ int n = 13;
// ceci était une annotation, mais on ne fait pas moins bien sans
// maintenant:
// loop pragma WIDEN_VARIABLES i;
  /*@ loop pragma WIDEN_HINTS i, 12, 13; */
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}


void main_err1(void)
{ int n = 13;
  /*@ loop pragma WIDEN_HINTS 12 ; */
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}




void main_err2(void)
{ int n = 13;
  /*@ loop pragma WIDEN_VARIABLES 12; */
  for (i=0; i<n; i++)
    {
      j = 4 * i + 7;
    }
}
