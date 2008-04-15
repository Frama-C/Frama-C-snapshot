/* run.config
* OPT: -slicing-level 0 -slice-return uncalled -slice-print -slicing-debug -no-slice-callers  -journal-disable
* OPT: -slicing-level 2 -slice-return main     -slice-print -journal-disable
* OPT: -slicing-level 2 -slice-return strlen   -slice-print -journal-disable
*
*
*
*
*
*
*/

int uncalled (int x) {
  return x+1;
}

int strlen(char* p ) {
  char* q ;
  int k = 0;

  for (q = p; *q ; q++) k++ ;

  return k;
}

int main (char *p_str[], int i ) {
  return strlen (p_str[i]);
}
