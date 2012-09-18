/* run.config
STDOPT: +"tests/syntax/bts0442.i"
*/
enum E { E0=0, E1=1} ve1=E1;
void f (void) {
  ve1=E0;
}
