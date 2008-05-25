int *T[2];


int x;

/*@
  requires \valid (p) ;
  ensures *p == 3 ;
*/
int main(int *p) {

  *p = 3;
//  x = *p;

}
