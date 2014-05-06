/* run.config
   OPT: -slevel 50 -val -deps -out -input -journal-disable -no-results
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
