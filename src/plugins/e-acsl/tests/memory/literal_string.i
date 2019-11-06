/* run.config
   COMMENT: literal string
*/

int main(void);

char *T = "bar";
int G = 0;

void f(void) {
  /*@ assert T[G] == 'b'; */ ;
  G++;
}

char *S = "foo";
char *S2 = "foo2";
int IDX = 1;
int G2 = 2;

const char *s_str = "the cat";
const char *l_str = "the dog and the cat";

int main(void) {
  char *SS = "ss";
  /*@ assert S[G2] == 'o'; */
  /*@ assert \initialized(S); */
  /*@ assert \valid_read(S2); */
  /*@ assert ! \valid(SS); */
  f();

  /* Make sure that compiler does not "merge strings", i.e., represents literal
   * strings as separate memory blocks. An assertion enabled in the debug mode
   * fails the execution if `s_str` is used as a part of `l_str`. */
  s_str++;
  l_str++;
  return 0;
}

char *U = "baz";
