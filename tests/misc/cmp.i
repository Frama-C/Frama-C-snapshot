
int cmp(const char *s1, const char *s2)
{
  if (*s1 == *s2) {;} ;
  return 1;
}

int main (unsigned int c,int d,int e) {
  char T[10];
  if(c) T[d]=e;
  int r = cmp(T+c,"V1.2");
  return r;
}
