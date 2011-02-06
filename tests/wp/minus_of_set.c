

//@assigns *( \union(p,q)-1);
void f(int * p, int * q) 
{ *(p-1) = 0 ; *(q-1)=0;}

//@assigns *(p-(0..2));
void f2(int * p, int * q) 
{ *(p-1) = 0 ; *(q-1)=0;}
