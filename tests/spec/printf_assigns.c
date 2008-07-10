/* run.config
   DONTRUN: support for discussion on printf specification
*/
/* How to 
specify this code ?
 */
#ifndef PTEST
#include <stdio.h>
#else
extern int printf (__const char *__restrict __format, ...);
#endif

int main(int argc, char* argv[])  {
  char * str = (argc < 2 ? "" : argv [1]) ;
  int pos;
  printf("%2$2s%1$n.\n", &pos, str);
  printf("dot position=%d\n", 1+pos);
  return 0;
}

