int Tab[10];
/*@ requires n < 10 ;
    behavior foo:
       assumes reset;
       assigns Tab[0..n-1];
    behavior bar: 
       assumes !reset;
       assigns \nothing;
*/
int h(int reset, int n) {
  int i, r = 0 ;
  /*@
    for foo:
      loop assigns Tab[0..i];
    for bar:
      loop assigns \nothing;
  */
  for (i = 0 ; i < n ; i++) {
    r += Tab[i] ;
    if (reset)
      Tab[i] = 0 ;
   }
  return r ;
}
  
// Notice that even if g() assigns nothing, it still return an unknown result.
/*@
  assigns \nothing;
*/
int g();

/*@ assigns \nothing ;
    ensures (reset?\result == 3 : \result == 2); 
    behavior foo:
       assumes reset; ensures \result==3;
    behavior bar: 
       assumes !reset; ensures \result == 2;
*/  
int f (int reset)
{
  int r=2; 
  if(reset){
    r= g(); 
  //@ assert r == 3;
  }
  return r; 
}
