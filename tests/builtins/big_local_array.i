/* run.config*
OPT: -val-show-progress -print -journal-disable -val -report
OPT: -load-script tests/builtins/big_local_array_script.ml -then-on prj -print -report
OPT: -val-show-progress -print -journal-disable -no-initialized-padding-locals -val
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
