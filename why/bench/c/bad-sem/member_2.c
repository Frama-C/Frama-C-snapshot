union S { int x; } s;
void f() { s.y = 0; } /* union has no member named y */

