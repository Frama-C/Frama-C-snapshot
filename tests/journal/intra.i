/* run.config
   EXECNOW: make -s tests/journal/intra.opt tests/journal/intra.byte
   EXECNOW: BIN intra_journal.ml ./tests/journal/intra.opt -journal-enable -journal-name tests/journal/result/intra_journal.ml tests/journal/intra.i > /dev/null 2> /dev/null
   CMD: FRAMAC_LIB=lib/fc ./tests/journal/intra.byte
   OPT: -load-script tests/journal/result/intra_journal -journal-disable
*/

/* Waiting for results such as:
 * spare code analysis removes statements having variables with
 * prefix "spare_"
 *
 * slicing analysis removes statement having variables with
 * prefix "spare_" and "any_"
 */

int G;

int tmp (int a) {
  int x = a;
  //@ assert x == a ;
  int w = 1;
  //@ assert w == 1 ; // w is not spare or else
                      // the assertion should be removed !
  int spare_z = 1;
  int spare_y = a+spare_z;
  return x;
}

int param (int a, int spare_b) {
  return a;
}

int spare_called_fct (int a) {
  return a;
}

int two_outputs (int a, int b) {
  G += b;
  return a;
}

int call_two_outputs (void) {
  int x, spare_y;
  int any_b = 1;
  int any_a = 2;
  int a = 1;
  int b = any_b;
  x = two_outputs (a, b);
  G = 1; /* don't use b = any_b; */
  b = 2;
  a = any_a;
  spare_y = two_outputs (a, b);
      /* don't use spare_y so don't use a = any_a */
  return x;
}

void assign (int *p, int *q) {
  *p = *q ;
}

int loop (int x, int y, int z) {
  int i = 0;
  //@ assert i < z ;
  //@ loop invariant i < y ;
  /* should keep y in sparecode analysis even if it is not used in the function */
  while (i < x) {
    i ++;
  }
  return i;
}

void stop(void) __attribute__ ((noreturn)) ;

int main (int noreturn, int halt) {
  int res = 0;
  int spare_tmp = 3;
  int spare_param = 2 + spare_tmp;
  int spare_ref = 3;
  int x = 1;
  int y = 2;
  res += param (2, spare_param);
  res += tmp (4);
  spare_called_fct (5);
  res += call_two_outputs ();
  res += loop (10, 15, 20);
  assign (&x, &spare_ref) ; /* <- Here, best can be done for spare analysis */
  assign (&x, &y) ;
  if (noreturn) {
    if (halt)
      stop () ;
    else
      while (1);
    //@ assert \false ; // What should be done with
                        // assertions related to dead code?
    }

  return res + G + x;
}

/*-------------------------------------*/
struct { struct { int x; int y; } a; int b; } X10;
int Y10;
int f10 (int x) {
  //@ slice pragma expr X10;
  //@ slice pragma expr X10.a;
  //@ slice pragma expr X10.a.x;
  //@ slice pragma expr Y10;
  //@ assert X10.a.x >= 0;
  return x;
}
int main2 () {
  Y10 = 0;
  X10.b = 0;
  X10.a.y += f10 (3);
  return X10.a.x + X10.a.y;
}
/*-------------------------------------*/
