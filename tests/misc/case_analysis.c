/* run.config
   OPT: -memory-footprint 1 -val -slevel 30 -journal-disable
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
