/* run.config
OPT: -load-script tests/rte/my_annot_proxy/my_annot_proxy.ml
*/


int f(int x, int y) {
  
  if (x + y != 0) {

    if ((x == 2147483647) && (y == 1)) return -1;
    else return 0;
  } else return 1;
}


int main() {
  int x =1 , y =2;
  int i;

  for (i = 0 ; i < 20 ; ++i) {    
    int tmp = x+y;
    y = x-y;
    x = tmp;
  }

  if ((x > 0)  && (y > 0)) 
    return f(x,y);
  else return 0;

}
