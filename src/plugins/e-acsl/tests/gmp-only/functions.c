/* run.config
   COMMENT: logic functions without labels
*/

/*@ predicate p1(int x, int y) = x + y > 0; */
/*@ predicate p2(integer x, integer y) = x + y > 0; */

/*@ logic integer f1(integer x, integer y) = x + y; */

// E-ACSL integer typing:
// types less than int are considered as int
/*@ logic char h_char(char c) = c; */
/*@ logic short h_short(short s) = s; */

/*@ logic int g_hidden(int x) = x; */
/*@ logic int g(int x) = g_hidden(x); */

struct mystruct { int k, l; };
typedef struct mystruct mystruct;
/*@ logic mystruct t1(mystruct m) = m; */
/*@ logic integer t2(mystruct m) = m.k + m.l; */

// To test function call in other clauses than assert:
/*@ predicate k_pred(integer x) = x > 0; */
/*@ requires k_pred(x); */
void k(int x) {}

// To test non-interference with global inits:
int glob = 5;

// To test that functions that are never called are not generated:
/*@ predicate never_called(int x) = x == x; */

/*@ logic double f2(double x) = (double)(1/x); */ /* handle in MR !226 */

// To test not_yet:
/*@ predicate p_notyet{L}(integer x) = x > 0; */
/*@ logic integer f_notyet{L}(integer x) = x; */

int main (void) {
  int x = 1, y = 2;
  /*@ assert p1(x, y); */ ;
  /*@ assert p2(3, 4); */ ;
  /*@ assert p2(5, 99999999999999999999999999999); */ ;

  /*@ assert f1(x, y) == 3; */ ;
  /*@ assert p2(x, f1(3, 4)); */ ;
  /*@ assert f1(9, 99999999999999999999999999999) > 0; */ ;
  /*@ assert f1(99999999999999999999999999999,
                 99999999999999999999999999999) ==
                 199999999999999999999999999998; */ ;

  /*@ assert g(x) == x; */ ;

  char c = 'c';
  /*@ assert h_char(c) == c; */ ;
  short s = 1;
  /*@ assert h_short(s) == s; */ ;

  mystruct m;
  m.k = 8;
  m.l = 9;
  /*@ assert t2(t1(m)) == 17; */ ;

  k(9);

  double d = 2.0;
  /*@ assert f2(d) > 0; */ ;

  // not yet supported
  /* /\*@ assert p_notyet(27); *\/ ; */
  /* /\*@ assert f_notyet(27) == 27; *\/ ; */
}
