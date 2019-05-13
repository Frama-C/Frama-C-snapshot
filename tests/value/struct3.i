/* run.config*
   STDOPT: +"-eva-warn-key garbled-mix"
*/

struct st1 {
 int a;
 int *b;
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

struct st2 s1,s2;
struct st1 s8,s7;

volatile int v;

void main () {
  s1.a=2;
  s1.c = &s1;
  s1.d[0] = 1;
  s1.d[1] = 2;
  s1.d[2] = 2;

  s1.b.a = 3;
  s1.d[8] = 2;
  s1.d[9] = 2;

  if (v) s1.d[10] = 2;

  s2 = s1; // Creates a garbled mix internally; make sure not to log it

  s2.a = (int) (s2.c + (int) s2.c); // creates a garbled mix in the struct
}
