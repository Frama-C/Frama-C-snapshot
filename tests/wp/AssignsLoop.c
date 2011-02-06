/* 
   kind : Positive
   model name : Store ; bhv : Not Sure About
   model name : Hoare ; bhv : Not Sure About 
 */

int r;

//@ requires i==1; ensures r==3;
void no_loop_assigns (int i)
{ 
  while(i!=4) {r=i ; i++;}
}

//@ requires i==1; ensures r==3; 
void loop_with_assigns (int i)
{ 
  //@ loop assigns r,i;
  while(i!=4) {r=i ; i++;}
}

int main (void) {return 0;}
