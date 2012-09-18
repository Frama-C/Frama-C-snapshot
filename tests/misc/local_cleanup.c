/* run.config
   STDOPT:  +"-inout-callwise"
*/

void f(int *p){
  p[1]=12;
}

void g(int x){
  int t[2];
  f(t);
}

void main(){
  int lmain[2];
  f(lmain);
  g(2);
}  
