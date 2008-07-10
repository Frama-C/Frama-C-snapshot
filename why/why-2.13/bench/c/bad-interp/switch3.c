
/* should error about duplicate case 0 */
/*@ ensures \result==0 */
int f (int x){
  int y = 0;

  switch (x) { 
  case 0 :
    break ;
  case 0 :
    break;
  }
  return y;
}
