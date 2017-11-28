/* run.config*
   STDOPT: +"-val-show-progress -slevel 100"
*/

int *p;
int x, y, t[10];

volatile v;

//@ assigns \result \from i;
long long Frama_C_abstract_cardinal(long long i);
//@ assigns \result \from i;
long long Frama_C_abstract_max(long long i);

void setup1() {
  p = v ? &x : &y;
}

void test1() {
  setup1();

  Frama_C_builtin_split(*p, 15);
  Frama_C_show_each_split(p);

  Frama_C_builtin_split_pointer(*p, 15);
  Frama_C_show_each_split_pointer(p);
}


void setup2() {
  if (v)
    t[1] = 4;
  if (v)
    t[2] = 8;
  if (v)
    t[2] = 12;
}

void test2() {
  setup2();
  int i = v;
  //@ assert 0 <= i < 10;
  Frama_C_show_each_t_i_1(i,t[i]);
  Frama_C_builtin_split_all(t[i], 15);
  if (t[i] == 8) {
    Frama_C_show_each_t_i_2(i,t[i]);
  }
}

struct s {
  int v;
  struct s *p;
};

struct s s0, s1, s2, s3, s4, s5, s6, s7;

struct s * ps;

void setup3() {
  s0.v = 0;
  s1.v = v ? -1 : 1;
  s2.v = 2;
  s3.v = 3;
  s4.v = 4;
  s5.v = 5;
  s6.v = 6;

  s6.p = &s4;
  s5.p = v ? &s4 : &s3;
  s4.p = v ? &s2 : &s0;
  s3.p = v ? &s1 : &s0;

  ps = v ? &s5 : (v ? &s6 : &s7);
}

void test3() {
  setup3();

  if (v) {
    if (s5.p->p->v == 1) {
      /* Fonctionne déjà (avec un if, pas avec un assert). */
      Frama_C_show_each_s_1(&s5.p->p, s5.p->p->v); 
    }
  }



  if (v) {
    if (ps->p->p->v == 1) {
      // Fonctionne actuellement (avec un if, pas un assert
      Frama_C_show_each_s_2(&ps->p, &ps->p->p, &ps->p->p->v, ps->p->p->v); 
    }
  }

  if (v) {
    Frama_C_builtin_split_all(ps->p->p->v, 15);
    // Maintenant, on peut utiliser un if
    if (ps->p->p->v == 1) {
      Frama_C_show_each_s_3(&ps->p, &ps->p->p, &ps->p->p->v, ps->p->p->v);
    }
  }

  if (v) {
    Frama_C_builtin_split_all(ps->p->p->v, 15);
    // Maintenant, on peut utiliser un assert
    //@ assert ps->p->p->v == 1;
    Frama_C_show_each_s_4(&ps->p, &ps->p->p, &ps->p->p->v, ps->p->p->v);
  }

  if (v) {
    s2.v = v;
    Frama_C_builtin_split_all(ps->p->p->v, 15);
    // Remarque: beaucoup d'états
    Frama_C_show_each_s_5(&ps->p, &ps->p->p, &ps->p->p->v, ps->p->p->v);
  }

}

void test4() {
  int x = v;
  //@ assert -3 <= x <= 25;
  long long nb = Frama_C_abstract_cardinal(x);
  Frama_C_show_each_nb(nb);
  Frama_C_builtin_split(x, 15);  // Not enough
  Frama_C_show_each_test4_1(x);

  Frama_C_builtin_split(x, 30); // Enough
  Frama_C_show_each_test4_2(x);
}

void test5() {
  int x = v;
  long long y = (long long)x * x / 5;
  long long r = Frama_C_abstract_max(y);
  Frama_C_show_each_max(y,r);
}

void test6() {
  int z;
  if (v) {
    z = v;
    //@ assert 0 <= z <= 5;
  }
  //@ slevel merge;
  Frama_C_builtin_split(z, 10);
  Frama_C_domain_show_each_test6(z);
}

void main() {
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
}
