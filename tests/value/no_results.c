/* run.config*
  STDOPT: #"-no-results-function init -inout-callwise -calldeps -slevel 10000" +"-inout"
*/

#define N 3000

int t[N];

void init() {
  for(int i=0; i<N; i++)
    t[i]=i;
}

void f() {
  t[1]=t[0];
}

void main() {
  init();
  f();
}
