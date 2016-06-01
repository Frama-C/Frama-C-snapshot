/* run.config*
   OPT: -val @VALUECONFIG@ -main main1
   OPT: -val @VALUECONFIG@ -main main2
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
  g();
}

void main2() {
  f(*(struct s*)((char*)(&s)+1));
}
