/* run.config
 OPT: -val-use-spec f -slice-return main -journal-disable -then-on 'Slicing export' -print -check
 OPT: -main main2 -slicing-level 3 -slice-undef-functions -val-use-spec h -slice-return main2 -journal-disable -slicing-keep-annotations -then-on 'Slicing export' -print -check -val



 */

int x, y, z, t;
int G1, G2;

//@ assigns x \from  \nothing;
void g();

//@ assigns x \from  \nothing;
int f() {
  x = 1;
  g();
}

/* When -val-use-spec f is used, the body of f must not be kept (as it
   references the body of g, which is not kept since the body of f is not
   analyzed. */
int main() {
  f();
  return x;
}

//@ assigns G1 \from a; assigns G2 \from b; ensures G1 == a;
void h(int a, int b) {
  G1 = a;
  G2 = b;
}

/* Check that function specialization works well with -val-use-spec. The result
   of -slicing-keep-annotations is a bit surprising, but in fact quite  good. */
int main2(int v1, int v2, int v3, int v4) {
  h(v1, v2);
  int tmp = G1;
  h(v3, v4);
  return tmp + G2;
}
