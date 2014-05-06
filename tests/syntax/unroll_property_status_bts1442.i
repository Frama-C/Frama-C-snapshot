/* run.config
OPT: -load-module lib/plugins/Report -report
OPT: -load-module lib/plugins/Report -ulevel -1 -report
*/

int u(void);

char *strcpy(char*dst, char*src) {
  char* ldst=dst;
  /*@ loop pragma UNROLL 20; */
  while (*ldst++ = *src++)
    ;
  return dst;
}

void main1(void) {
  char *p;
  {
    char a[10] = "Not ok";
      char b [5];
    if (u()) strcpy(b,a);
   }
}
