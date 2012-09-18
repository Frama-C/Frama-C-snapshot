union { long long l ; float f ; double d ; } u1, u2;
float f;
double d;

main (long long l){
  u1.l = l;
  f = u1.f + 1.0;
  u2.l = l;
  d = u2.d + 1.0;
  Frama_C_dump_each();
}
