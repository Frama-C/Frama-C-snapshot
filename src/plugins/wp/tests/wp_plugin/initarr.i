/* run.config
   OPT: -wp-init-const
 */
/* run.config_qualif
   OPT: -wp-init-const
 */

typedef struct { int *f; int *g; } S;

int a,b,c,d,e,f;
const S A[3] = { { .f=&a,.g=&b}, {.f=&c,.g=&d}, {.f=&e,.g=&f} };

/*@ 
  requires 0 <= i < 3 ;
  ensures SEP: \separated( A[i].f , A[i].g );
  ensures ALT:
    (A[i].f == &a) ||
    (A[i].f == &c) ||
    (A[i].f == &e) ;
*/
void job(int i) { return; }
