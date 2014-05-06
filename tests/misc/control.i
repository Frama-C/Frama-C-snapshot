/* run.config
  GCC:
  OPT: -val -deps -out -input  -main f -journal-disable
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
