/* run.config
   STDOPT: +"-simplify-cfg"
*/

int f (int c) {
  int x = 0;
  switch (c) {
    /*@ breaks x == 1; */
    { 
    case 0: x = 1; break;
    case 1: x = 3;
    case 2: x++;
    default: x++;
    }}
  while (1) {
    /*@ breaks x == \old(x);
        continues x == \old(x) + 1;
    */
    {
      if (x < c) { x++; continue; }
      break;
    }
  }
  return x;
}

/*@ ensures x==1 ==> \result==1; */
int f5 (int x){
  int y = 0;

  switch (x) {
  case 1 :
    while (x>0) /*@ breaks x > 0; */ break ;
    y = 1;
  }
  return y;
}
