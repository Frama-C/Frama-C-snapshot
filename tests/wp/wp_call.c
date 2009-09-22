struct Ts { int a; int b; };
int G;
struct Ts S;
int * P;


/*@ 
  requires \true;
  assigns G, S.b;
  ensures G == \old(G) + x + 1
       && S.b == \old(S.b) + 1
       &&  \result == x + 1;
*/
int add_G (int x) {
  x++;
  G += x ;
  S.b++;
  return x;
}

/*@ 
  requires G == 0;
  ensures G == 6 && S.a == 1 && S.b == 1 && \result == 6;
*/
int main(void) {
  int r;

  S.a = 1;
  S.b = 0;

  r = add_G (5);
  
  return r;
}

int T[10];
int X;

// this is to test the assigns order : T[X] is T[\old(X)] !
//@ assigns T[X], T[X+1], X;
void f (int x) {
  T[X] = 0;
  T[X+1] = 0;
  X = x;
}

// Be carreful : wrong translation of quantification in M2
//@ ensures (\forall int i; 2 <= i < 10 ==> T[i] == \old(T[i]));
void call_f (void) {
  int a = 3;
  X = 0;
  f (a);
}

//@ ensures (T[3] == \old(T[3]));
void call_f_1 (void) {
  int a = 3;
  X = 0;
  f (a);
}

/*~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/* This is to test is [a] and \old(a) are correctly handdled,
 * even if P points on a. 
 * ACSL says that [a] in a postconditon always means \old(a).
 */
/*@ ensures a == \old(a);
    assigns *P;
    */
void f_param (int a) {
  a++;
  *P = 0;
}
void call_f_param (void) {
  int x = 1;
  P = &x;
  f_param (x);
  //@ assert x == 0;
}
/*~~~~~~~~~~~~~~~~~~~~~~~~~~*/
