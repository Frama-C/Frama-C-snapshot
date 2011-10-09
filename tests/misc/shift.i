int a,b,d,e,f,g,h;
unsigned int ua,ub,uc,ud,ue,uf;

int main(int c, int z, int zz) {
  a=5024;
  d = 255;
  f= -255;
  if ((c<=3) && (c>=0)) {
    c = 2*c-1;
    a = 157 << c;
    d=1975;
    d = d >> c;
    f= -1975;
    f = f >> c;
    }

  if (z) z=1<<32;
  if (zz) zz=1>>5555;

  if (c) {
    b = 66;
    b = b << b;
    };


  ua = 5607;
  ua >>= 2 ;
  ub = (unsigned int)(-3000);
  ub >>= 2;
  printf("ua:%u\nub:%u\n",ua,ub);

  return b;
}
