/* run.config
   DONTRUN: main test is merge_different_assigns.i
*/

int x, t, u, v, w, x;

/*@ assigns x;
  assigns t \from t;
  assigns u;
  assigns v;
  assigns \result \from y;
  assigns w \from w;
  assigns x;
*/
int f(int y);

