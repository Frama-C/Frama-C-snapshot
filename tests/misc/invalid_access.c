int t[10];
int x,*p,y,z;

void main (int c,int d) {
  p=d?t:(t+1);
  p[0]=5;
  p[1]=6;
  p[2]=5;
  z = p[!d];
/*  p[3]=5;
  p[4]=5;
  p[5]=5;
  p[6]=5;
  p[7]=5;
  p[8]=5;
  p[9]=5;*/
/*  y=p[1];
    if (c>=0 && c <=15) x = p[c];*/
}
