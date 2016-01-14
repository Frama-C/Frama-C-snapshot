/* run.config
   STDOPT: #"-val -warn-unsigned-overflow"
   STDOPT: #"-no-val-left-shift-negative-alarms"
*/

int a,b,d,e,f,g,h;
unsigned int ua,ub,uc,ud,ue,uf;

void printf(const char* c,...);

char t[10];


int main(int c, int z, int zz) {
  a=5024;
  d = 255;
  f= -255;
  if ((c<=3) && (c>=0)) {
    c = 2*c-1;
    int cc = c;
    a = 157 << c;
    //@ assert c >= 0; // Reduction by the alarm on RHS
    c = cc;
    d=1975;
    d = d >> c;
    //@ assert c >= 0; // Reduction by the alarm on RHS
    c = cc;
    f= -1975;
    f = f >> c;
    c = cc;
    c = c << 3;
    //@ assert c >= 0; // Reduction by the alarm on LHS
    }

  if (z & 1) z=1<<32;
  if (zz) zz=1>>5555;

  if (z & 16) {
    b = 66;
    b = b << b;
    };


  ua = 5607;
  ua >>= 2 ;
  ub = (unsigned int)(-3000);
  ub >>= 2;
  printf("ua:%u\nub:%u\n",ua,ub);

  if (z & 32)
  {
    int r = (unsigned long)t << 8;
    r += (long)t << 8;
  }

  unsigned int shl = 2U << 31; // "Unsigned overflow."

  return b;
}
