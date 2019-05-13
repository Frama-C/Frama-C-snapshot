/* run.config*
   STDOPT: +"-slevel 50 -no-results"
   STDOPT: +"-eva-warn-key=missing-loop-unroll=feedback -eva-warn-key=missing-loop-unroll:for=active -main main2"
*/
static int a = 7;

int test()
{
  return a--;
}

int main()
{
  for(test();test();test())
  {
    Frama_C_show_each_t(test());
  }
  return 0;
}

volatile int v;
void main2() {
  while (v) {}
  //@ loop unroll 1;
  for(;v;);
  for(;v;);
  do {} while(v);
}
