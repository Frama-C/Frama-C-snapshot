/* run.config
* OPT: -check -slicing-level 0 -slice-return uncalled -no-slice-callers  -journal-disable -then-on 'Slicing export' -print
* OPT: -check -slicing-level 2 -slice-return main -journal-disable -then-on 'Slicing export' -print
* OPT: -check -slicing-level 2 -slice-return strlen -journal-disable -then-on 'Slicing export' -print
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
