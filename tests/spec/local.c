/*@ ensures \let i = x + 1; i == \result;
    ensures \result == \let i = x; i+1;
*/
int f(int x) { return x+1; }

/*@
requires (int)(x+y+1) == x+y+1;
ensures
    \let f =
       \lambda integer x;
          \let x1 = x + 1;
             \lambda integer y; x1 + y;
    \let P = \lambda integer x,y; x == y;
        P(f(x,y),\result);
*/
int g(int x, int y) { return (x+y+1); }
