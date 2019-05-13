/* run.config
   EXECNOW: make -s @PTEST_DIR@/compute_annot/compute_annot.cmxs
   OPT: -warn-special-float none -load-module @PTEST_DIR@/compute_annot/compute_annot -journal-disable
*/

int global = 15;

typedef struct cell {
  int val;
  struct cell* next;
} cell;


typedef struct other {
  cell c;
} other;


/*@ requires x > 0 ;
    requires (int) (x + y) != 0 ;
*/
int f(int x, int y, float z) {
  return x + y - (int) z;
}


int g(int a, int b) {
  return a / b ;
}

int main() {
  int a=2,b=3;

  return f(b-a,g(a,b),1.0);
}
