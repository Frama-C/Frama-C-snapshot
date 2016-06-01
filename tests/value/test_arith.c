


int t[10]; 
int j,k,ecart,tmp,**pptr,*ptr,*qtr,m1,m2,m3;

void main(int v,int n)
{
  t[1] = 4;
  pptr = &ptr;
  ptr = t+1;
  t[5] = 7+t[1]; 
  j = t[2+3]+t[5];
  j = j+1;
  *ptr = 0;
  n = n + 1;
  qtr = ptr + 1;
  k = (int)ptr + 1;
  ecart = &t[7] - &t[5];

}

int G;
void main1() { // Currently unused
  G=0;
  G = 1;
  *((char*)&G) = 0;
  G = G+1;
}
