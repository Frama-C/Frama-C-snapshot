struct st1 {
 int a;
 long b;
};

struct st2 {
 int a;
 int d[10];
 struct st1 b;
 struct st1 e[10];
 struct st2 *c;
};

struct st1 tabst[10], tabst2[10];

struct st2 tab_s[2];
struct st2 tab_s1[2];
struct st2 tab_s2[2];
struct st2 tab_s3[2];
struct st2 tab_s4[2];

struct st2 s1,s2,s4,s5,s6;
struct st1 s8,s7;

long x,y,z,t; volatile int v;

void main () {
  x = (long) &s1.d[9];
  y = (long) &s1.d[10];
  z = (long) &s1.b;


  
  s1.a=2;
  s1.c = &s1;
  s1.d[0] = 2;
  s1.d[1] = 2;
  s1.d[2] = 2;

  s1.b.a = 3;

  s1.d[5] = 7;


  s1.d[8] = 8;
  s1.d[9] = 8;

  if (v) s1.d[10] = 777;

}
