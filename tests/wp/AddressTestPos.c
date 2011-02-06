/* 
   kind : Positive
   model name : Store; bhv : Provable
   model name : Hoare; bhv : Out of Scope
*/

int * t[2];

//@ ensures *(t[0])== 4; 
int main()
{

  int i=4; 
  int j=3;
  int * p; 
  int * q;
  
  p = &i ;
  q = &j;
  t[0]= p;
  t[1]= q;
  return 0; 

}
