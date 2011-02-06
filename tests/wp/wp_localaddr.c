/* run.config_pruntime
   OPT: -wp -wp-model Runtime -wp-no-logicvar -journal-disable -wp-proof simplify  -wp-print -wp-verbose 2
*/

//@ensures \result ;
int f(void)
{
  int *p;
  int *q;

  { int x=1 ; p = &x ; }
  { int y=2 ; q = &y ; }

  //@assert p_invalid_ok: !\valid(p) ;
  //@assert q_invalid_ok: !\valid(q) ;
  //@assert p_q_compare_ko: p != q ; 
  //@assert p_q_compare_ok: \valid(p) ==> \valid(q) ==> p!=q ;

  //@assert p_read_ko: *p == 1 ;
  //@assert q_read_ko: *q == 2 ;

  return *p == 1 && *q == 2 ;

}

/*@ ensures ok : \result == 0 || \result == 1;
    ensures ko1 : \result == 0;
    ensures ko2 : \result == 1;
*/
int cmp_invalid_addr (void) {
  int *p;
  int *q;

  { int x=1 ; p = &x ; }
  { int y=2 ; q = &y ; }

  return (p == q) ? 1 : 0;
}

/*@ ensures ok : \result == 0 || \result == 1;
    ensures ko1 : \result == 0;
    ensures ko2 : \result == 1;
*/
int cmp_invalid_addr_as_int (void) {
  int p;
  int q;

  { int x=1 ; p = (int) &x ; }
  { int y=2 ; q = (int) &y ; }

  return (p == q) ? 1 : 0;
}

