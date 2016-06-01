void main (int c) {
  int G=0,i=4;
  int MAX = 12;
  int JMAX=5;
  int j=3;

//@ loop pragma UNROLL 128;
  do {
    G += i;
    i++;
    j--;
    }
  while (i<=256 || j>=0);

//@ loop pragma UNROLL 10;
 do
    { if(c) continue;

    if(c--) goto L;
    c++;
  L: c++;
      }
  while(c);
}
