/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-steps 50
*/

typedef struct las { double a; double b; } las;

/*@
assigns \nothing;
ensures \result>=x;
*/
double g(double x);


void f(las * p)
{
  p->a = g(p->b);
  //@ assert qed_ok: p->a>=p->b;
}

