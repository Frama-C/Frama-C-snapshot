/* run.config
OPT: -wp-mm 0 -wp-debug 1 -journal-disable -wp-no-proof
OPT: -wp-mm 1 -wp-debug 1 -journal-disable -wp-no-proof
OPT: -wp-mm 2 -wp-debug 1 -journal-disable -wp-no-proof
COMMENT: -wp-mm 3 -wp-debug 2 -lib-entry  -journal-disable
COMMENT: -wp-mm 3 ne passe pas (bouclage ergo ???)
*/
/* run.config_dev
OPT: -wp-mm 0 -wp-debug 2 -journal-disable -wp-proof
OPT: -wp-mm 1 -wp-debug 2 -journal-disable -wp-proof
OPT: -wp-mm 2 -wp-debug 2 -journal-disable -wp-proof
*/

//@ assigns \nothing;
void infinite (int c) {
  int s = 0;
  if (c) {
    //@ loop assigns s;
    while (1)
      s++;
    //@ assert 2 > 1;
  }
  //@ assert c == 0;
}

//@ assigns \nothing;
void loops (int c) {
  int x;

  if (c) x=0;
    x=3;


  //@ loop assigns x;
  while (c) { x=0;}
  x=1;


  if (c) 
    //@ loop assigns x;
    while(c) x=0;
  x=1;


  if (c) 
    //@ loop assigns x;
    do {x=0; } while(c);
  x=1;
  //@ assert x == 1;

}

/*@  assigns \nothing;
  ensures \result == 9 ; */
int classical_loop () {
  int i;
  int s = -1;

  /*@ loop invariant s + 1 == i && i <= 10; 
    @ loop assigns i, s;
   */
  for (i=0; i< 10; i++) s = i ;

  return s;
}

/*@ assigns \nothing;
  ensures \result == 5; */
int simple_loop (int c) {
  int x;
  int c = 0;

  /*@ loop invariant 0 <= c <= 6 && ((c==0 || x == c-1));
    @ loop assigns c, x;
    */
  for(c=0;c<=5;) {
    //   CEA_DUMP();
    x = c;
    c++;
    //   CEA_DUMP();
  }
// CEA_DUMP();
//@ assert c == 6;
  return x;
}

/*@ assigns \nothing;
  ensures \result == 6; */
int goto_natural_loop (int c) {
  int c = 0;
L : if (c > 5) goto R;
    c++;
    goto L;
R : return c;
}

int T[10];

//@ ensures \result == T + 5;
int * ptr_on_array () {
  int * p = T;
  int i;
  /*@ loop invariant i <= 5 && p == T+i;
    @ loop assigns i, p;
    */
  for (i = 0; i < 5; i++)
    p++;
  return p;
}


void loop_assert () {
  int i = 0;
  /*@ loop invariant i >= 0;
    @ loop assigns i; */
  while (i < 10) {
    ++i;
    //@ assert 0 < i <= 10;
  }
}

