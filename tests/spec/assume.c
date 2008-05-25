
void f(int x) {
  int * p,*q;

//@ behavior default: assumes \valid(p); // je ne veux pas vérifier cette assert

 q = p ;

//@ assert \valid(q); // je veux vérifier cette assert

}
