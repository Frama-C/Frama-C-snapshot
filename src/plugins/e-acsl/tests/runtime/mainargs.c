/* run.config
   COMMENT: the contents of argv should be valid
*/

#include <string.h>

/*@ requires \valid(&argc);
  @ requires \valid(&argv); */
int main(int argc, char **argv) {
  int i;

  /*@ assert \forall int k; 0 <= k && k < argc ==> \valid(argv + k) ; */
  /*@ assert \block_length(argv) == (argc+1)*sizeof(char*) ; */

  /*@ assert argv[argc] == \null ; */
  /*@ assert ! \valid(argv[argc]) ; */
  for (i = 0; i < argc; i++) {
    int len = strlen(argv[i]);
    /*@ assert \valid(argv[i]) ; */
    /*@ assert \forall int k; 0 <= k && k <= len ==> \valid(&argv[i][k]) ; */
  }
}
