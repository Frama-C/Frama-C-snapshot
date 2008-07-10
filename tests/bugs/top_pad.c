struct toto { char c; int s; char c2;};
struct toto *f(struct toto *);
 
int main(struct toto *y) {
  struct toto *z = (struct toto *) (1 - (int) y);
  char * p = (char*)y + 2;
  *p = 5;
  p = p+8;
  *p++;
  return z->s;
}
