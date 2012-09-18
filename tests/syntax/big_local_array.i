/* run.config
STDOPT: +"-load-module lib/plugins/Report" +"-val -report"
OPT: -load-module lib/plugins/Report -load-script tests/syntax/big_local_array_script.ml -then-on prj -print -report
STDOPT: +"-no-initialized-padding-locals -val"
*/

struct S {
  int a[50];
  int b[32];
};

int main () {
  struct S x[32] = 
    { [0] = { .a = { 1,2,3 }, .b = { [5] = 5, 6, 7 }},
      [3] = { 0,1,2,3,.b = { [17]=17 } }
    };
}
