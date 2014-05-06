typedef struct {
  int a;
  int b;
} ts;

long t[5];
ts u[5];

volatile unsigned int v;

void f () {
  long *p = &t[v];
  //@ assert \valid(p+3);
  p[3]=1;
  long *q = ((int*)&t[v])+1;
  //@ assert \valid(q+3);  
  q[3]=1;
  p = p;
  q = q;
}

void g() {
  ts *p = &u[v];
  ts *q = ((int*)&u[v])+1;
  ts *r = ((int*)&u[v])+1;
  ts *s = ((int*)&u[v])+1;

  //@ assert \valid(&p->b);  
  p->a = 1;
  //@ assert \valid(&q->a);
  q->a = 2;
  //@ assert \valid(&r->b);
  r->b = 3;
  //@ assert \valid(s);
  s->a = 4;
  p = p;
  q = q;
  r = r;
  s = s;
}



void h(unsigned int c1, unsigned int c2) {
  int *p = &t[c1];
  int *q = &c2;
  //@ assert \valid(\union(q, q));
  //@ assert \valid(\union(p, q));
  p = p;
}

void i(unsigned int c1, unsigned int c2, unsigned int c3, unsigned int c4) {
  //@ assert \valid(&t[c1]);
  //@ assert \valid(&t[c2]);
  int *p = &t[0];
  //@ assert \valid(p+c3);
  ts *q = &u[0];
  //@ assert \valid(&(q+c4)->a);
  c1 = c1;
  c2 = c2;
  c3 = c3;
  c4 = c4;
}

void j() {
  int y;
  int *q = &y;
  int *p;
  int *r;
  { int x = 0;
    p = &x;
  }
  //@ assert \valid(q);
  //@ assert \valid(&y);
  //@ assert !\valid(p);
  if (v)
    p = & y+3;
  else p = &q+4;
  //@ assert !\valid(p);
  p = 0;
  //@ assert !\valid(p);
  if (v)
    r = &y;
  //@ assert \valid(r);

}

struct s {
  int *f1;
};

extern struct s *vs;

/*@ requires \valid(p->f1);
  requires \valid(p->f1);
  requires \valid(p);
  requires \valid(p);
  requires \valid(p->f1);
  requires \valid(p->f1);
*/
void k(struct s *p) {
  *(p->f1)=1;
}


void main () {
  f();
  g();
  h(v,v);
  i(v,v,v,v);
  j();
  k(vs);
}
