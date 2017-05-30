/* run.config*
  GCC:
  STDOPT: #"-main f"
*/

int x,y,c,d;


void f() {
  int i;
  for(i=0; i<4 ; i++) {
    if (c) { if (d) {y++;} else {x++;}}
    else {};
    x=x+1;
    }
}
