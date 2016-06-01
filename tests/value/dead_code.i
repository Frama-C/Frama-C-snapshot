void main(int in) {
  int i,j=6,k,l;
  
  i=10;
  //@ impact pragma stmt;
  i=1;
 L: if (i) {l= 17 ; goto OUT;}
//  i--;
//  j+=i;
//  goto L;
//  while (1);

 OUT: j = l;
  l=17;
}
