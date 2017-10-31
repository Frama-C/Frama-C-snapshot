//@ behavior b1: assumes x>=0; behavior b2: assumes x<=0;
void f(int x);
//@ disjoint behaviors b1, b2;
void f(int x) {
  int * p,*q;

//@ for b1,b2: behavior default: assumes \valid(p); // je ne veux pas vérifier cette assert

 q = p ;

//@ assert \valid(q); // je veux vérifier cette assert

}
