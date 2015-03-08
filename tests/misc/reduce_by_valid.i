typedef struct {
  int a;
  int b;
} ts;

long t[5];
ts u[5];

volatile unsigned int v;
volatile signed int sv;

void main1 () {
  long *p = &t[v];
  //@ assert \valid(p+3);
  p[3]=1;
  long *q = ((int*)&t[v])+1;
  //@ assert \valid(q+3);  
  q[3]=1;
  p = p;
  q = q;
}

void main2() {
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



void main3(unsigned int c1, unsigned int c2) {
  int *p = &t[c1];
  int *q = &c2;
  //@ assert \valid(\union(q, q));
  //@ assert \valid(\union(p, q));
  p = p;
}

void main4(unsigned int c1, unsigned int c2, unsigned int c3, unsigned int c4) {
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

void main5() {
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
void main6(struct s *p) {
  *(p->f1)=1;
}

void main7 () {
  int t7_1[10];

  int *p = t7_1;

  //@ assert !\valid(p+(0..1000));
  //@ assert !\valid(p+(-1..9));
  //@ assert \valid(p+(0..9));
  
  int t7_2[10000];
  p = t7_2;
  //@ assert !\valid(p+(0..10000));

  p = t7_2+v;
  //@ assert \valid(p+(-5..250));
  //@ assert \valid(p+(-5..250));
}

void main8 () {
  int t8_1[10];
  int t8_2[20];
  int *tp[4];

  tp[0] = t8_1;
  tp[1] = t8_1+v;
  tp[2] = t8_2+v;
  tp[3] = v ? t8_1+v : t8_2+v;

  int **p = &tp;

  if (v) {
    //@ assert \valid(p[0..3]);
    //@ assert \valid(p[0..3]); // Reduction succeeds
  }
  if (v) {
    //@ assert \valid(tp[0..3]);
    //@ assert \valid(tp[0..3]); // Same
  }

  if (v) {
    //@ assert \valid(tp[0..3]+1);
    //@ assert \valid(tp[0..3]+1); // Not written yet

  }
}

void main9() {
  struct s {
    int a;
    int b;
    char t[100];
  };

  char tc[sizeof(struct s) + 50];
  struct s* p = tc + v - 100;

  //@ assert \valid(&p->t[0..99]);
  p->t[0] = 1;
  p->t[99] = 2;
  //@ assert \valid(&p->t[0..99]);
  
}

void main10() {
  char t[40];
  char *p; 
  int u[20];
  int *q;

  p = &t[sv];
  *((int *)p) = 1;
  Frama_C_show_each_main10_1(p);
  //@ assert \valid((int *)p);

  p = &t[sv];
  *((int *)p+2) = 2;
  Frama_C_show_each_main10_2(p);
  //@ assert \valid(((int *)p)+2);

  p = &t[sv];
  *((int *)(p+2)) = 1; // TODO
  Frama_C_show_each_main10_3(p);
  //@ assert \valid((int *)(p+2));


  struct s {
    int a;
    int b;
  };

  p = &t[sv];
  ((struct s *)p)->b = 4;
  Frama_C_show_each_main10_4(p);
  ((struct s *)p)->b = 4;
  //@ assert \valid(&(((struct s *)p)->b));

  p = &t[sv];
  ((struct s *)p+2)->b = 4;
  Frama_C_show_each_main10_4(p);
  ((struct s *)p+2)->b = 4;
  //@ assert \valid(&(((struct s *)p+2)->b));


  q = (int*)(((char*)&u)+sv);
  *((char *)q) = 1;
  Frama_C_show_each(q);
  *((char *)q) = 1;
  //@ assert \valid((char *)q);

  q = (int*)(((char*)&u)+sv);
  *((char *)q+2) = 1;
  Frama_C_show_each(q);
  *((char *)q+2) = 1;
  //@ assert \valid((char *)q+2);

}

void main11() {
  char TC[500];
  char * p = TC+10;
  int off = v;
  int len = v;
  //@ assert 0 <= off <= 400 && 200 <= len <= 400;
  p = p + off;
  Frama_C_show_each(p);
  //@ assert \valid(p+(0 .. len-1));

  int TU[500];
  int * q = TU+10;
  off = v;
  len = v;
  //@ assert 0 <= off <= 400 && 200 <= len <= 400;
  q = q + off;
  Frama_C_show_each(q);
  //@ assert \valid(q+(0 .. len-1));

  p = TU+10;
  off = v;
  len = v;
  //@ assert 0 <= off <= 800 && 1800 <= len <= 2200;
  p = p + off;
  Frama_C_show_each(p);
  //@ assert \valid(p+(0 .. len-1));

  q = TC+10;
  off = v;
  len = v;
  //@ assert 0 <= off <= 100 && 50 <= len <= 100;
  q = q + off;
  Frama_C_show_each(q);
  //@ assert \valid(q+(0 .. len-1));
}

void main () {
  main1();
  main2();
  main3(v,v);
  main4(v,v,v,v);
  main5();
  main6(vs);
  main7();
  main8();
  main9();
  main10();
  main11();
}
