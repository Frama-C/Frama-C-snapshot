/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@ -val-show-progress
*/

struct S { int i; };

/*@ lemma foo: \forall struct S x; x.i >= 0 || x.i < 0; */

struct matrix {
        int m[100];
};

/*@ ensures \result >= x.i; */
int main(struct S x, struct matrix m) {
  int y = x.i;
  /*@ assert y == x.i; */
        int i = 0;
        int j = 0;
        struct matrix m_t;
        /*@ loop assigns i,j;
          @ loop assigns m_t;
          @ loop invariant 0 <= i <= 2;
          @ loop invariant 0 <= j <= 2;
          */
        for(i = 0; i < 2; i++){
          /*@ loop assigns j;
            @ loop assigns m_t;
            @ loop invariant 0 <= j <= 2;
            @ loop invariant 0 <= i < 2;
            */
          for(j = 0; j < 2; j++){
               m_t.m[i*2+j] = m.m[j*2+i];
          }
        }
  return y;
}
