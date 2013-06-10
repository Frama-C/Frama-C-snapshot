/* run.config
   OPT: -load-script tests/rte/rte_api/rte_get_annot.ml -journal-disable
*/


int f(int x, int y) {
  if (x + y != 0) {
    if ((x == 2147483647) && (y == 1)) return -1;
    return 0;
  } 
  return 1;
}

int main() {
  int x =1 , y =2;
  int i;

  //@ assert (y > x);
  for (i = 0 ; i < 20 ; ++i) {    
    int tmp = x+y;
    y = x-y;
    x = tmp;
  }

  //@ assert (i > 0);
  if ((x > 0)  && (y > 0)) 
    return f(x,y);

  return 0;
}
