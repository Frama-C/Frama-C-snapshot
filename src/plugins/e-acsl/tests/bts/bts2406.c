/* run.config
   COMMENT: bts #2306, do not monitor incomplete types
*/

const char tab[]; /* not monitored */
char t[10]; /* monitored */

int main(void) {
  char *p = tab; /* monitored */
  /*@ assert !\valid(p+(0..9)); */
  /*@ assert \valid(t+(0..9)); */
  return 0;
}
