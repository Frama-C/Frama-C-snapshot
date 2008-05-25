int *p,T[10]={0,1,2,3,4,5,6,7,8,9};
char C[10]={0,1,2,3,4,5,6,7,8,9};
char *q;
int f(void) { /* make a top integer */
  int i = 0;
  while (&i+(int)&i) {
    i++;}
  return i;
};

int main () {

  p = &T[1] + f();
  q = &C[1] + f();
  
  if (p >= &(T[5])) {*p=88;*q=77;}

        
}
int main1 () {

  for (p = T;p<&(T[10]);p++) {*p=1;}


}
