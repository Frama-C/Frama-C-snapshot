/* 
   kind : Positive
   model name : Store ; bhv : Proved with alt_ergo
   model name : Hoare ; bhv : Out of Scope
 */
int A;

/*@ ensures A == 5 ; */
int main() {
  int *p = &A;
  *p = 5; 
  return *p;
}
