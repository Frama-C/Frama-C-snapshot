struct s {
  int *a;
};

struct {
  struct s s1[3];
  struct s s2;
} ss;

int f (int t[10] , int n, int *ptr) {
if (t[n]);
if (*ptr);
return 65 ;
}

int main (void) {
int *p=0;
if (*p);
ss.s2.a; // not dangerous, remove
ss.s1[2].a; // maybe dangerous, keep
return 0 ;
}
