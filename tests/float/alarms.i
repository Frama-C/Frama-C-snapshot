union { long long l ; float f ; double d ; } u1, u2;
float f;
double d, big;
unsigned long long ull;
double fd();

main (long long l){
  u1.l = l;
  f = u1.f + 1.0;
  u2.l = l;
  d = u2.d + 1.0;
  Frama_C_dump_each();
  
  float vf = fd();
  double vd = fd();
  long long i = vd;
  long long j = vf;
  vd = fd();
  double mvd = -vd / 4.;

  big = 0x1.8p63;
  ull = big;
}
