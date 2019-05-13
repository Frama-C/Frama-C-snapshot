/* run.config*
   OPT: -no-autoload-plugins -load-module from,inout,eva -constfold -slevel 0 -eva @EVA_CONFIG@ -print -then -slevel 10 -eva -print
   */

void g(double x) { double y= x*x; }

int main(double x)
{
    g(x);
    return 0;
}
