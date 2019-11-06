/* run.config
   COMMENT: let binding
*/

int main(void) {
  int n = -2;
  /*@ assert \let u = n*n; u >= 0; */ ;
  /*@ assert
      \let u = n*n;
      \let v = u + 1; u > 0; */ ;

  /*@ assert ((\let u = 1; u) + 1) == 2; */ ;
  /*@ assert
      \let u = 1;
      (\let v = u + 1; v) == 2; */ ;
  /*@ assert
      \let u = 1;
      (\let u = u + 1; u) == 2; */ ;

  long m = 0x7fffffffffffffffL;
  /*@ assert (\let u = m; u*u) > m; */ ;

  char c = 'a';
  /*@ assert \let u = 'b'; c < u; */ ;

  float f = 1.0f;
  /*@ assert \let u = f; u == f; */ ;

  int t[4] = {1,2,3,4};
  /*@ assert \let u = t + 1; 1 == 1; */ ; // testing warnings when using
                                          // let on pointers/arrays
  /*@ assert (\let u = t + 1; 1) == 1; */ ; // same but for terms

  struct {int x, y;} r = {1, 2};
  /*@ assert \let u = r; u.x + u.y == 3; */ ;

  union {int x; char *y;} s;
  s.x = 5;
  /*@ assert (\let u = s; u.x) > 0; */ ;

  return 0;
}
