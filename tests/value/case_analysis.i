/* run.config*
   OPT: -no-autoload-plugins -load-module inout,eva -val @VALUECONFIG@ -slevel 30 -journal-disable -float-normal

*/

int sq,s;

float rq,r;

void main(int c)
{
  s = (c >= -10) ? ((c <= 10) ? c : 0) : 0;
  r = s;
  //@ assert s >= 0 || s < 0 ;
  sq = s * s;

  //@ assert r >= 0.0 || r < 0.0 ;
  rq = r * r;
}
