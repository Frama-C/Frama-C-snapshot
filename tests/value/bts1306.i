/* run.config*
   OPT: -no-autoload-plugins -load-module from,inout,value -constfold -slevel 0 -val @VALUECONFIG@ -print -then -slevel 10 -val -print
   */

void g(double x) { double y= x*x; }

int main(double x)
{
    g(x);
    return 0;
}
