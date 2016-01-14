/* run.config_qualif
   OPT: -wp-literals
*/

void f(void)
{
  char * a = "abc" ;
  char * b = "def" ;
  char * c = "def" ;
  char * d = "abcdef" ;
  //@ assert AB: \separated(a,b);
  //@ assert B_valid: \valid_read(b+(0..2));
  //@ assert B_out: !\valid_read(b+3);
  //@ assert B_rw: !\valid(b+1);
  //@ assert VAL: \forall integer i; 0 <= i <= 2 ==> b[i] == d[3+i] ;
  /* The ending '0' is ok ! */
}
