/* run.config
   OPT:  -val -journal-disable -float-normal -lib-entry
*/

typedef struct S { float y; } S;

S s;

double r, cv, un, zp, zm, zs;

long long l;

double Ha[5], THa[5];

/*@
	requires -1000.0 <= x <= 1000.0;
	requires 0.0 <= s.y <= 0.0;
*/

int main(float x)
{
  Ha[2] = 0.5;
  Ha[3] = 0x0.8000000000001p0;
  Ha[4] = 0x0.8000000000002p0;
  Ha[1] = 0x0.7ffffffffffffp0;
  Ha[0] = 0x0.7fffffffffffep0;

  THa[2] = 1.5;
  THa[3] = 0x1.8000000000001p0;
  THa[4] = 0x1.8000000000002p0;
  THa[1] = 0x1.7ffffffffffffp0;
  THa[0] = 0x1.7fffffffffffep0;

  if (l >= 4700000000000000000ll)  l = 4700000000000000000ll;
  if (l <= 4500000000000000001ll)  l = 4500000000000000001ll;
  cv = *(double*)&l + 1.0;
  r = x;
  s.y = s.y * 1.0;
  un = 1.0;
  zp = un - un;
  zm = - (un - un);
  zs = zp + zm;
  return 1;
}
