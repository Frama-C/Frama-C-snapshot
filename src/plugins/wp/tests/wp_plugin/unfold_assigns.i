/* run.config
   OPT:
   OPT: -wp-unfold-assigns
 */

/* run.config_qualif
   DONTRUN:
 */

struct S { int a,b; };

//@ assigns (*p) ;
void f(struct S *p);

//@ assigns p->a , p->b ;
void g(struct S *p);

//@ assigns s->a, s->b ;
void NO_UNFOLD_OK_1(struct S *s) {
  g(s);
}

//@ assigns (*s) ;
void NO_UNFOLD_OK_2(struct S *s) {
  f(s);
}

//@ assigns (*s) ;
void NO_UNFOLD_OK_3(struct S *s) {
  g(s);
}

//@ assigns s->a, s->b ;
void NO_UNFOLD_KO(struct S *s) {
  f(s);
}
