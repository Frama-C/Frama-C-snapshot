/* run.config_ci
   COMMENT: recursive logic functions
   STDOPT: +"-eva-ignore-recursive-calls"
*/

/*@ logic integer f1(integer n) =
    n <= 0 ? 0 : f1(n - 1) + n; */

/*@ logic integer f2(integer n) =
    n < 0 ? 1 : f2(n - 1)*f2(n - 2)/f2(n - 3); */

/*@ logic integer g(integer n) = 0; */
/*@ logic integer f3(integer n) =
    n > 0 ? g(n)*f3(n - 1) - 5 : g(n + 1); */

/*@ logic integer f4(integer n) =
    n < 100 ? f4(n + 1) :
    n < 0x7fffffffffffffffL ? 0x7fffffffffffffffL :
    6; */

int main (void) {
   /*@ assert f1(0) == 0; */ ;
   /*@ assert f1(1) == 1; */ ;
   /*@ assert f1(100) == 5050; */ ;

   /*@ assert f2(7) == 1; */ ;

  /*@ assert f3(6) == -5; */ ;

  /*@ assert f4(9) > 0; */ ;
}
