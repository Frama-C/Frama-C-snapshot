/* run.config*
   STDOPT: #"-warn-signed-overflow"
   STDOPT: #"-no-warn-signed-overflow"
*/
extern int printf (__const char *__restrict __format, ...);
/* L'analyseur déborde et dit i=-1 */
int main (int c) {
  unsigned long long i = 0xFFFFFFFFFFFFFFFFULL;
  unsigned long j = 0xFFFFFFFFUL;

  long long is = 0xFFFFFFFFFFFFFFFFULL;
  long js =    0xFFFFFFFFUL;
  long minjs = - (j/2) -1  ;
  long maxjs =  j/2  ;
  
  unsigned long long i1 = i+1;
  unsigned long j1 = j+1;

  int y = c?1:100000;
  int x = (60000 * y) / 100000;
  int z = y * 1000 * 1000;
  int t = (-y) * 10000000; 
/*
 printf("unsigned long long:%llu (+1:%llu)\nunsigned long:%lu (+1:%lu)\n"
         ,i,i1,j,j1);
  printf("signed long long:%lld (+1:%lld)\nlong:%ld (+1:%ld)\n"
         ,is,is+1,js,js+1);
  printf("min signed long:%ld (-1:%ld)\n"
         ,minjs,minjs-1L);
*/
  if (-c) {}

  return 0;
}
