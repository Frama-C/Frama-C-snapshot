/* run.config
   OPT: -constfold -slevel 0 -val -print -then -slevel 10 -val -print
   */

void g(double x) { double y= x*x; }

int main(double x)
{
    g(x);
    return 0;
}
