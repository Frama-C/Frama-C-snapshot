// assigns p[-128..127] ;
void f(int *p,int *v)
{
  char k = *((char *) v ) ;
  /*@ assert OUT: k < 256 ; */
  p[k] = 1 ;
}
