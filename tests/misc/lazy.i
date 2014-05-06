/* run.config
   OPT: -val -deps -out -input -journal-disable
   OPT: -val -deps -out -input -journal-disable -undefined-pointer-comparison-propagate-all
*/
int a=-1;
int b, d;
int *q, *r, *s, *t;
void main (int *p, int c, int d) {

  q = &a - !c; 	
  if (q) r=q;

  s = &a - !d; 	
  if (!s) t=s;
 
  if (p && *p ) *p = 0 ;
  if (&a) { a=0; b=1; }
  if (&a+1) a+=2;
  if (&a+2) a+=4;
  return;
}
