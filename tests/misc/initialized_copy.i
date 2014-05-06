/* run.config
   STDOPT: +"-val-warn-copy-indeterminate @all -then -main main2 -then -val-warn-copy-indeterminate=\"-main2\""
   STDOPT: 
*/

int w[10];
volatile int v;

struct s {
  char c;
  int i;
};

struct v {
  int i1;
  int i2;
};

void f(int i) {
}

void g(int i);


int main() {

  if (v) {
    int b;
    int a = b; // completely indeterminate
    Frama_C_show_each_unreached();
  }

  if (v) {
    int b;
    if (v)
      b = 1;
    int a = b; // possibly determinate
    Frama_C_dump_each();
  }

  if (v) {
    int c;
    if (v) {
      char* p = &c;
      *p = 1;
    }
    int a = c; // completely indeterminate on some bits
    Frama_C_show_each_unreached();
  }

  if (v) {
    int c;
    char* p;
    if (v) {
      p = &c;
      *p = 1;
    }
    p = (char*)&c+1;
    *p = 2;
    p = (char*)&c+2;
    *p = 3;
    p = (char*)&c+3;
    *p = 4;
    int a = c; // possibly determinate
    Frama_C_dump_each();
  }

  if (v) {
    struct s s1, s2;
    s1.c = 1;
    s1.i = 5;
    s2 = s1; // Never warn, this is a struct
    Frama_C_show_each(s2);
  }

  if (v) {
    struct v sv1, sv2;
    sv1 = sv2; // Never warn, even though we probably should
    Frama_C_show_each(sv1);
  }
  if (v) {
    struct v sv1, sv2;
    sv2.i1 = 1;
    sv1 = sv2; // Never warn, even though we probably should
    Frama_C_show_each(sv1);
  }

  if (v) {
    int i = v;
    //@ assert 0 <= i < 10;
    int t[10], v[10];
    t[i] = v[i];
    Frama_C_show_each_unreached(); // completely indeterminate
  }

  if (v) {
    int i = v;
    //@ assert 0 <= i < 10;
    int t[10], v[10];
    v[1] = 1;
    t[i] = v[i];
    Frama_C_dump_each();// possibly determinate. t is only partially
                        // initialized as it was not initialized before the copy
  }

  if (v) {
    int i = v;
    //@ assert 0 <= i < 10;
    int v[10];
    v[1] = 12;
    w[i] = v[i];
    Frama_C_dump_each();// possibly determinate; w is completely initialized after
  }

  if (v) {
    int a;
    return a; // completely indeterminate
  }

  if (v) {
    int a;
    if (v)
      a = 8;
    return a; // possibly determinate
  }

  if (v) {
    int a;
    f(a); // completely indeterminate.
    Frama_C_show_each_unreached();
  }

  if (v) {
    int a;
    g(a); // completely indeterminate. We also warn when the option is not active, as g has no body
    Frama_C_show_each_unreached();
  }

  if (v) {
    int a;
    if (v)
      a = 1;
    f(a); // possibly determinate
    Frama_C_dump_each();
  }

  if (v) {
    int a;
    if (v)
      a = 1;
    g(a); // possibly determinate. We also warn when the option is not active, as g has no body
    Frama_C_dump_each();
  }

  return 0;
}

int main2() {
  int x;
  return x; // Test that option can be deactivated
}
