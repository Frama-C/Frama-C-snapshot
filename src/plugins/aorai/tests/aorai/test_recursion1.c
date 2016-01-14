/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-ltl tests/aorai/test_recursion1.ltl -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/


/* Calcul de la longueur cumulee des chaines de caracteres prises en parametre */

//======================== 
// Chaines de caracteres
//---------------------- 
/*@ axiomatic string_len {
      logic integer string_len{L}(char *s); 
      axiom strlen0{L}: \forall char *s ; \valid(s) ==> string_len(s)>=0;
      axiom strlen1{L}: \forall char *s ; \valid(s) &&  s[0]=='\0' ==> string_len(s)==0 ;
      axiom strlen2{L}: \forall char *s ; \valid(s) ==> s[string_len(s)]=='\0' ;
      axiom strlen3{L}: \forall char *s ; \valid(s) && s[0]!='\0' ==> string_len(s)==1+string_len(s+1) && \valid(s+1) ;
      axiom strlen4{L}: \forall char *s ; \valid(s) ==> 
                                              \forall integer i ; 0<=i<string_len(s) ==> s[i]!='\0' ;
    }
*/
//@ predicate valid_string{L}(char *s) = \valid(s) && \valid_range(s,0,string_len(s)) ;
//======================== 



/* Calcul de la longueur d'une chaine */
/*@ requires valid_string(argv);
  @ ensures \result==string_len(argv);
 */
int countOne(char* argv) {
  int r=0;

  if(argv[0]==0) return 0;
  r++;
  r+=countOne(argv+1);
  return r;
  
}

/* Somme de chacune des longueurs */
/*@ requires argc>0 && \valid(argv) && 
             \valid_range(argv,0,argc) &&
             (\forall integer i; 0<=i<argc ==> valid_string(argv[i]));
  @ ensures \result>=0;
 */
int count(int argc, char** argv) {
  //  printf(" --> '%s'\n",argv[0]);
  int s=countOne(argv[0]);
  if (argc>1) s+=count(argc-1,argv+1);
  return s;
}

/*@ requires argc>=0 && (argc>0 ==> \valid(argv)) && 
             \valid_range(argv,0,argc) &&
             (\forall integer i; 0<=i<argc ==> valid_string(argv[i]));
  @ ensures \result==1;
 */
int main(int argc, char** argv) {
  int somme=0;
  if (argc>0) somme=count(argc,argv);
  //  printf("Longueur cumulee des params = %d\n",somme);
  return 1;
}
