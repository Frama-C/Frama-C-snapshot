/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -print
*/

struct R_1 {
   int r ;
};
struct S_1 {
   struct R_1 __attribute__((____fc_mutable__)) s;
};
struct T_1 {
   struct S_1 t ;
};
struct U_1 {
   struct T_1 u ;
};
struct V_1 {
   struct U_1 const v ;
};
struct W_1 {
   struct V_1 w ;
};
struct W_1 x;
int y;
void f()
{
  x.w.v.u.t.s.r = y;
}



