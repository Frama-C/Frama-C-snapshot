/* run.config_qualif
   DONTRUN:
*/

/* bug gitlab #9 */

unsigned int  *T1[1];
unsigned char *T2[1];


int main(void) {
  int **p = &T1;
  char **q = &T2;
  /*@ assert \true; */
  return 0;
}
