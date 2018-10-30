


char t[15];
char *p = t;
int x;

int main(int c){
  if (c&1)
    /*@ assert \separated(p, p+1) ; */
    x = 1;
  else if (c&2)
    /*@ assert \separated(p, p) ; */
    x = 1;
  else if (c&4)
    /*@ assert \separated(p+1, p+1) ; */
    x = 1;
  else if (c&8)
    /*@ assert \separated(p+(0..8), p+(8..12)) ; */
    x = 1;
  else
    /*@ assert \separated(p+(0..5), p+(6..12)) ; */
    x = 1;
  return 0;
}
