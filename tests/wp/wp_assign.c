int C;

//@ ensures \at(c, Pre) == C &&  \at(c, Pre) == c;
void main (int x,int c) {
  C = c;
  c = x;
  c++;
  //@ assert x == c-1;
}
