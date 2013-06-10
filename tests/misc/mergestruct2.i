/* run.config
   OPT: -memory-footprint 1 -print -journal-disable tests/misc/mergestruct3.i tests/misc/mergestruct1.i
*/
struct s *p;

void g(void)
{
  p = 0;
}
