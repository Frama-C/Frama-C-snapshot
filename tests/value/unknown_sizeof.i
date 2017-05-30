/* run.config*
   OPT: -no-autoload-plugins -load-module value -val @VALUECONFIG@ -main main1
   OPT: -no-autoload-plugins -load-module value -val @VALUECONFIG@ -main main2
*/

struct s;

struct s s;

void f(struct s) { // Argument has unknown size
  return;
}

struct s g() {
  return *(struct s*)((char*)(&s)+1); // Return has unknown size
}

void main1() {
  g(); // We used to not stop on this line because the return code was not used, but now we do
  struct s r; r = g();
}

void main2() {
  f(*(struct s*)((char*)(&s)+1));
}
