int i,j,k,n,r;

#if 0
void main2() {
  r=0;
  goto L;
  n=5;

  for (int i=0; i<n ; i++) {
 L:
    r = i+r;
    if(r && n-- || n++ && n--) r=99;

    }

  r=10;
}
#endif

int main() {
  r = 0;
  k= 0 ;
  n = 2;
  for (i=0; i<n; i++)
    for (j=0;j<n; j++)
      for (k=0; k<n; k++)
        r = i+j+k+r+1;
  return r;
}
