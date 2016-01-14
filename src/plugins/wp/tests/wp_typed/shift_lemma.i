struct t {
  int b;
  int c;
};

struct s {
  int d;
  struct t u[10];
  int e;
};

/*@
  predicate inv{L}(struct s *p) = p->d == 0 && p->e == 0 &&
    \forall int i; 0 <= i < 10 ==> p->u[i].c == 0;
  @*/

/*@
  requires inv(p);
  @*/
  void f(struct s *p){
    /*@ assert p->d == 0; @*/
    /*@ assert \forall int i; 0 <= i < 10 ==> p->u[i].c == 0; @*/
    /*@ assert p->u[0].c == 0; @*/
    /*@ assert p->u[1].c == 0; @*/
    /*@ assert p->e == 0; @*/
  }
