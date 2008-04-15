
/* should warn about unreachable statement y=1 */
/*@ ensures \result==0 */
int f (int x){
  int y = 0;

  switch (x) { 
    y = 1;
  case 0 :
    break ;
  }
  return y;
}


