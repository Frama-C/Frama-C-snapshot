void g(int *ip) {
   *ip = 1;
 }

 int a;
 void f(){
   g(&a);
 }
