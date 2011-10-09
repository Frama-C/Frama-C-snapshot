/* run.config
   OPT: -check -slice-calls call1 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -slice-calls call2 -journal-disable -then-on 'Slicing export' -print
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
