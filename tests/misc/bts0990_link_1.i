/* run.config
   DONTRUN: main test is at bts_0990_link.i
 */
char s[100];

void perror(const char *);

void g(void){
  perror(s);
}
