/* run.config
   DONTRUN: not available for Carbon release.
   OPT: -wp -wp-froms -wp-print
*/

/*@ axiomatic A {
      logic int D_FI_fext_1 (int c, int x, int y);
      logic char test (char x);
    }
*/

struct Tstr {int a; int b; int c; } S;
int x ,y ,z , * p ;
int T[100];

/*@
   assigns \result \from c, x, y;
   assigns x \from c, y;
   behavior c_true :
     assumes c != 0;
     assigns x \from \nothing;
     assigns \result \from x;
   behavior c_false:
     assumes c == 0;
     assigns x \from y;
     assigns \result \from y;
*/
int fh (int c) {
  int a = c ? x : y;
  x = c ? 0 : a;
  return a;
}
/*@
   assigns \result \from c, x, y;
   assigns x \from c, y;
*/
int fext (int c);

/*@
   assigns \result \from c, x, y;
   assigns x \from c, y;
   assigns y \from y;
*/
int call (int c) {
  int xa;
  c++;
  xa = fext (c);
  y++;
  return xa;
}

int unknown (int);

//@ assigns \result \from x, y; // test comment...
int spec () {
  int a = 0;
  int b = x;

  //@ assigns b \from y, a; 
  if (y > 0) 
    b = unknown (a);

  return b;
}

/*@  assigns \result \from x, n;
*/
int loop (int n) {
  int s = 0;
  /*@ loop assigns s \from i, x;
      loop assigns i \from i;
   */
  for (int i = 0; i < n; i++) {
    s += x;
  }
  return s;
}

int loop2 (int n) {
  int s = 0;
  /*@ loop assigns s \from s, T[0..(i-1)];
    @ loop assigns T[0..(i-1)] \from \nothing;
    @ loop assigns i \from i;
  */
  for (int i = 0; i < n; i++) {
    s += T[i];
    T[i] = 0;
    }
  return s;
}

/*@ assigns \result \from S;
*/
int * maxS (void) {
  int * p = &(S.a);
  if (*p < S.b) p = &(S.b);
  if (*p < S.c) p = &(S.c);
  return p;
}


/*@
     assigns x \from c , y , x ;
     assigns z \from c , x , z ;
     assigns p \from c ;

     behavior c_true :
       assumes c ;
       assigns x \from y ;
       assigns p \from \nothing ;

     behavior c_false : // et ça behavior 
       assumes ! c ;
       assigns z \from x ;
       assigns p \from \nothing ;
*/
void f ( int c ) {
  if ( c ) p = & y ; 
  else p = & z ;

  if (! c ) * p = x + 1; 
  else x = * p + 1; 
}

int main (void) { return 0 ; }
