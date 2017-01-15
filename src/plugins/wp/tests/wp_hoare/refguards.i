/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
   OPT: -wp-model +ref
*/

//@ ensures \separated(c,d) ;
void f( int *a, int *b ,int *c ,int * d, int k )
{
  a[k] = b[k] ;
  *d = *c ;
  *c = k ;
}

//@ ensures a[k]==b[k] ;
void g( int *a, int *b, int k )
{
  a[k] = b[k] ;
}

/* No warning ; ensures checks the added hyps */
//@ ensures \separated(c,d);
void h( int *c, int *d, int k )
{
  *d = *c ;
  *c = k ;
}

/* No warning ; ensures checks the added hyps */
/*@ ensures \separated(c,d);
  @ ensures \separated(c,*c);
  @ ensures \separated(c,*d);
  @ ensures \separated(*c,d);
  @ ensures \separated(d,*d);
  @ ensures KO: \separated(*c,*d);
*/
void s( int **c, int **d, int k )
{
  **d = **c ;
  **c = k ;
}
