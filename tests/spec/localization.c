/* This file contains various annotations errors to test the localization
   of error messages.
 */
/*@ logic integer f(integer i) = 1;

  @ logic integer g(integer i) = j;

  @ logic integer h(integer i) = k;

  @ logic integer i(integer i) = l;
  @*/
void ComposerPage(void)
{
    int x = 0;
    x++; /*@ assert bar; */
    //@ assert foo;
    return ;
}

void f() {
  //@ ghost int index = 0; // comment
  int x = 0;
  //@ assert wrong;
  return;
}

void g() {
}

typedef struct _S S;
/*@ axiomatic S {
logic S S00;
logic S1 S0;
} */
