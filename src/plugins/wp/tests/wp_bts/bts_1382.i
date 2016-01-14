/* run.config_qualif
   DONTRUN: nothing to prove
*/

//@ assigns *((char*)dst+(0..size-1)) \from *((char*)src+(0..size-1));
void copy(void* dst, const void* src, unsigned int size);

//@ assigns *p;
void f(unsigned char *p) ;

int dest, src;
void loop (void) {
  int i ;
  i=0 ;
  //@ loop assigns i, dest;
  while (i<100) {
    f(&dest);
    copy(&dest, &src, sizeof(dest));
    i++;
  }
}
