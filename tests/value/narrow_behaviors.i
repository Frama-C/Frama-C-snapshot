/* run.config*
   STDOPT: +"-slevel 10 -value-verbose 2"
*/

extern int nondet;
typedef struct { int x, y; } pair;

/*@
  assigns \result \from nondet;
  behavior a:
    assumes nondet == 0;
    ensures \result.x == 1;
    ensures \result.y == 1;
  behavior b:
    assumes nondet != 0;
    ensures \result.x >= 2;
    ensures \result.y <= 2;
  behavior c:
    assumes nondet == 1;
    ensures \result.x == 2;
    ensures \result.y == 2;
  behavior d:
    assumes nondet != 1;
    ensures \result.x <= 2;
    ensures \result.y >= 1;
  complete behaviors a, b;
  complete behaviors c, d;
 */
pair f1();

/*@
  assigns \result \from nondet;
  behavior a:
    assumes nondet == 0;
    ensures \result.x >= 1;
    ensures \result.y <= 1;
  behavior b:
    assumes nondet != 0;
    ensures \result.x >= 2;
    ensures \result.y <= 2;
  behavior c:
    assumes nondet == 1;
    ensures \result.x <= 1;
    ensures \result.y >= 2;
  behavior d:
    assumes nondet != 1;
    ensures \result.x <= 2;
    ensures \result.y >= 1;
  complete behaviors a, b;
  complete behaviors c, d;
 */
pair f2();

int main() {
  {
    pair p = f1(); Frama_C_dump_each ();
    pair q = f2(); Frama_C_dump_each ();
  }

  {
    //@ assert nondet == 0 || nondet == 1;
    pair r = f1(); Frama_C_dump_each ();
    pair s = f2(); Frama_C_dump_each ();
  }

  return 0;
}

