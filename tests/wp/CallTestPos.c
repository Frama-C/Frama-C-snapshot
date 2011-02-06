/* 
   kind : Positive
   model name : Store ; bhv : Provable
   model name : Hoare ; bhv : Proved with all
 */

int z ; 
/*@ assigns z;
   ensures z==x ;
*/
void g(int x,int y); 

void call_void(void)
{
  g(4,5);
  
  //@ assert z==4;
}


int pre;
/*@
   requires pre!= 0; 
   assigns pre ; 
   ensures \result == post ; 
*/
int f (int x, int post)
{
  pre = x ; 
  return post;

}

void call_with_ret(void)
{
  int x ; 
  x = f(1,2);
  //@ assert x == 2;
}


int a; 

/*@ assigns a; 
  @ ensures (x>= y ==> \result == x) && (x<y ==> \result ==y);
*/
int f1(int x,int y);

void call_with_ret_sig(void)
{
  int x = 1; 
  int y = 2; 
  x = f1(x,y);
  //@assert x==y;

}



int main (void) {return 0;}
