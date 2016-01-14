/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-ltl tests/aorai/test_switch3_et_recursion.ltl -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/

/* Calcul de la longueur cumulee des chaines de caracteres prises en parametre */

/* Calcul de la longueur d'une chaine */
int countOne(char* argv) {
  int r=0;
  
  switch (argv[0]) {
  case 0: r=0; // ICI il n'y a pas de break. C'est un bug, mais c'est un test ;)
  case 1:
  case 2:
  case 3:
  default: 
    r++; 
    r+=countOne(argv+1); 
  } 
  return r;
}

/* Somme de chacune des longueurs */
int count(int argc, char** argv) {
  //  printf(" --> '%s'\n",argv[0]);
  if (argc>0) return countOne(argv[0])+count(argc-1,argv+1);
  return 0;
}

int main(int argc, char** argv) {
  int somme;
  somme=count(argc,argv);
  //  printf("Longueur cumulee des params = %d\n",somme);
  return 1;
}
