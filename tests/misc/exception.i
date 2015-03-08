/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -load-module @PTEST_DIR@/@PTEST_NAME@ -print
   OPT: -load-module @PTEST_DIR@/@PTEST_NAME@ -remove-exn -print
 */
struct my_exn { int e; };

struct my_exn2 { char c; };

struct my_exn foo;

struct my_exn2 bar;

int x = 42;

int f1 (int c) {
  return c;
}

/*@ ensures \result == c+1; */
int f2 (int c) {
  return c+1;
}

int f3 (int c) {
  return c+2;
}

int f4 (int c) {
  return c+3;
}

/*@ ensures \result != 42; */
int h(int c) {
  if (c-1<=0) return f1(c);
  else if (c-100<=0) /*@ returns \result == c+1; */ return f2(c);
  else if (c>360) return f3(c);
  else return f4(c);
}
