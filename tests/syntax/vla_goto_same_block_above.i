volatile int nondet ;
int main() {
  int i = 42 ;
  toto : ;
  char vla[i] ;
  if (nondet) goto toto ;
  return 0 ;
}

int f() {
  int i = 42 ;
  if (nondet) { toto : ; }
  char vla[i] ;
  if (nondet) goto toto ;
  return 0 ;
}

int g() {
  int i = 42 ;
  char vla[i] ;
  if (nondet) { toto : ; }
  if (nondet) goto toto ;
  return 0 ;
}
