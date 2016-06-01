int r1,r2,r3,r4;

void main (int x,int y,int z,int t,int *p,int q[2]) {
  x = y ;
  x++;
  y--;
  p=(int*)(&p);
  p++;
  z = x;
  t=5;
  z+=t;
  *q=3;
  q++;

  r1 = x-y;
  r2 = z-y;
  r3 = *(q-1);
  r4 = *q;  
   Frama_C_dump_each();
}

void main1 (int x,int y,int z,int t,int *p,int *q) {
  *q = 3;
  q++;
  r3 = *(q-1);
  r4 = *q;  
   Frama_C_dump_each();
}
