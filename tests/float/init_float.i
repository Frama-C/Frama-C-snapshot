/* run.config
   OPT: -memory-footprint 1 -val -journal-disable -float-normal -lib-entry
*/

typedef struct S { float y; } S;

S s;

double r, cv, un, zp, zm, zs;

long long l;


/*@
	requires -1000.0 <= x <= 1000.0;
	requires 0.0 <= s.y <= 0.0;
*/
int main(float x)
{
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
