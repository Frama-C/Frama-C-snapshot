//@ assigns \result \from \nothing;
int any_int(void);
int x=55,y=77,t[4];
void main0() {
  int i = any_int ();
  t[1] = x;
  t[i] = y;
}

void main() {
  int i = any_int ();
  t[i] = 1;
  t[1] = y;
}

int TT[5][5]={0,1,1,0,0,0,0,0,0,0,0,1};
int TTT[5][5]={1,2,3,4,5,0,0,0,0,0,1};

void main1(){
  TT[5][5] =2;
}
/*
typedef struct {int a; int b;} T;
void g() {
  int x,y,i,t[4],*p;
  T s1,s2;
  t[i] = y;
  t[1] = x;
  s1 = s2;
  s1.a = s2.a;
  s1.b = s2.b;
}

int t[10][10];
int ***p, i,j,x;
char **c;
void g1() {
//  *(*(p+2)) = &i;
//  ***(p+i) = x;
  *(*(c+2)+1) = 'a';
}

void h1() {
  *(*(*(&c+2)+1)+5) = 'a';
}

*/
