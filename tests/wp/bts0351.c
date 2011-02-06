struct Ts {int x; int y; };
struct Tstr {int a; struct Ts s; int t[10]; struct Tstr * p; } S;

//@ ensures S.s.x == x && S.s == { \old(S.s) \with .x = x };
void rw_field_field (int x) {
  S.s.x = x;
}
int main (void) {return 0;}
