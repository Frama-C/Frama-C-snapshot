/* run.config
   COMMENT: arithmetic operations
*/

int main(void) {
  int x = -3;
  int y = 2;
  long z = 2L;

  /*@ assert -3 == x; */ ;
  /*@ assert x == -3; */ ;
  /*@ assert 0 != ~0; */ ;

  /*@ assert x+1 == -2; */ ;
  /*@ assert x-1 == -4; */ ;
  /*@ assert x*3 == -9; */ ;
  /*@ assert x/3 == -1; */ ;
  /*@ assert 0xffffffffffffffffffffff/0xffffffffffffffffffffff == 1; */ ;
  /*@ assert x % 2 == -1; */ ;
  /*@ assert -3 % -2 == -1; */ ;
  /*@ assert 3 % -2 == 1; */ ;

  /*@ assert x * 2 + (3 + y) - 4 + (x - y) == -10; */ ;

  /*@ assert (0 == 1) == !(0 == 0); */ ;
  /*@ assert (0 <= -1) == (0 > 0); */ ;
  /*@ assert (0 >= -1) == (0 <= 0); */ ;
  /*@ assert (0 != 1) == !(0 != 0); */ ;

  /*@ assert 0 == !1; */ ;
  /*@ assert 4 / y == 2; */ // non trivial division added when fixing bts #751

  // example from the JFLA'15 paper (but for a 64-bit architecture)
  /*@ assert 1 + ((z+1) / (y-123456789123456789)) == 1; */

  /*@ assert 1 - x == -x + 1; */ // test GIT issue #37

  return 0;
}
