/* run.config
STDOPT:
STDOPT: +"-cpp-extra-args='-DHIDING_TYPEDEF'"
STDOPT: +"-cpp-extra-args='-DREDEFINITION'"
*/
typedef int digit;

struct S {
  digit d1;
  int const digit;
  digit d2;
};

const digit D = 10;
long const int L = 10L;
digit A;

int main () {
  digit x = 4;
  int digit = 3;
  // error: digit is now a variable
#ifdef HIDING_TYPEDEF
  digit y = 5;
#endif
  return x + digit+A;
}

digit A = 8;

#ifdef REDEFINITION
int digit = 10;
#endif
