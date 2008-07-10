/* run.config
   GCC:
   OPT: -slice-print -slice-calls call1
   OPT: -slice-print -slice-calls call2
*/

//@ assigns \result \from v;
int call1 (int v);

//@ assigns \result \from v;
int call2 (int v);

void oper (int * s, int * p, int i) {
  *s = *s + i;
  *p = *p * i;
}

void main (int n) {
  int i;
  int sum = 0;
  int product = 1;
  
  for(i = 0; i < n; ++i)
    oper (& sum, & product, i);

  call1(sum);
  call2(product);
}
