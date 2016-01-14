/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-buchi tests/aorai/test_recursion2.promela -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
   OPT: -aorai-buchi tests/aorai/test_recursion3.promela -aorai-test 1 -aorai-acceptance -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/

/*
    OPT: -buchi tests/aorai/test_recursion2.promela -ltl-output-c-file tests/aorai/result/test_recursion2_annot.c 
    OPT: tests/aorai/result/test_recursion2_annot.c -jessie -jessie-int-model exact -jessie-why-opt -fast-wp  
*/


/* Calcul de la longueur cumulee des chaines de caracteres prises en parametre */

//======================== 
// Strings
//--------
//
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
//
//
//======================== 
// Sum of a tab
//-------------
//
/*@ axiomatic sum_tab {
      logic integer sum_tab{L}(char *t,integer l,integer i);
      axiom sum_tab0{L}: \forall char *t, integer l ; \valid_range(t,0,l) ==> sum_tab(t,l,0)==t[0];
      axiom sum_tabi{L}: \forall char *t, integer l, integer i ; \valid_range(t,0,l) && 0<i<=l ==> sum_tab(t,l,i)==sum_tab(t,l,i-1)+t[i];
      axiom sum_tabn{L}: \forall char *t, integer l ; \valid_range(t,0,l) && l>0 && l==string_len(t) ==> sum_tab(t,l,l)==sum_tab(t,l,l-1);
    }
*/
//
//======================== 


int global_argc=0;

/* Calcul de la longueur d'une chaine */
/*@ requires valid_string(argv);
  @ ensures \result==string_len(argv);
 */
int count(char* argv) {
  if(argv[0]==0) return 0;
  return 1+count(argv+1);
}


/*@ requires \valid(t) && length>=0 && length==string_len(t) && \valid_range(t,0,length);
  @ ensures \result==sum_tab(t,length,length);
 */
int sumOne(char* t, int length) {
  //  printf(" --> t : '%s' / length : %d\n",t,length);
  int sum=0;
  int i=0;
//printf(" ----> c = '%d'\n",t[i]);
  /*@ loop invariant ranges: 0<=i<=length;
    @ loop invariant sumValue0: i==0 ==> sum==0;
    @ loop invariant sumValuei: i>0 ==> sum==sum_tab(t,length,i-1);
  */
  for(i=0;i<length;i++){
//printf(" ----> c = '%d'\n",t[i]);
    sum+=t[i];
  }
  return sum;
}



/*@ requires argc>=0 && (argc>0 ==> \valid(argv) && valid_string(argv[0])); 
//             && \valid_range(argv,0,argc) &&
//             (\forall integer i; 0<=i<argc ==> valid_string(argv[i]));
  @ ensures \result==1;
 */
int main(int argc, char** argv) {
  int sum=0;
  int length;
  global_argc=argc;

  if (argc>0) {
    length=count(argv[0]);
    sum=sumOne(argv[0],length);
  }
//printf("Somme cumulee du premier param = %d\n",sum);
  return 1;
}
