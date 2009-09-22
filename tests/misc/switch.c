/* run.config
   OPT: -memory-footprint 1 -val -deps -out -input -journal-disable
   OPT: -memory-footprint 1 -val -deps -out -input -journal-disable -simplify-cfg 
*/

int result1, result3;
int result2=7;
double d2;

int main (int c, int d, int e, int f, double d1) {

  switch (d)
    {
    case 1: 
      result1 = 1;
      break;
    case 2: 
      result1 = 2;
      break;
    case 3: 
      result1 = 3;
    case 4: 
      result1 = 4;
      break;
    }

  switch(c) 
    {
    case 0: CEA_F(c); return c;
    case 2: return c;
    }

  switch (e)
    {
    case 0: result2 = e;
    }
  f = f ? 14 : 42; 
  switch (f==14)
    {
    case 0: result3 = f;
    }

  switch(d1>=0.0)
    {
    case 0: d2=-d1;break;
    default: d2=d1; break;
    }
  return 77;
}
