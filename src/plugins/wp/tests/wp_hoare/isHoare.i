/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-model +ref
*/

/*@ ensures ok : \result == 0 || \result == 1; */
int cmp_invalid_addr_as_int (void) {
  int p;
  int q;

  { int x=1 ; p = (int) &x ; }
  { int y=2 ; q = (int) &y ; }

  return (p == q) ? 1 : 0;
} 
