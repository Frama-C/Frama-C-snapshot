/* run.config
OPT: @PTEST_DIR@/merge_inline_2.c -aggressive-merging -check -print
*/

/* Test that we rename properly inlines even if they have prototypes and
   if they are used before they are defined */
int foo(int x); /* Declare it here.  */

inline int foo(int x) { return x; } 

extern getfoo2();

int main() {
  if(getfoo2() != (int)foo) {
    return 1;
  }

  return 0;
}
