/* run.config*
   STDOPT: #"-from-verify-assigns"
*/
int u;
int v;
int k;

void f(void) {
  k = u;
}

/*@ assigns u,k;
  behavior true:
    assumes u <= 3;
    assigns u \from v;
    assigns k \from \nothing;
  behavior ok_too_large:
    assumes u <= 3;
    assigns u \from v,k;
    assigns k \from u;
  behavior wrong:
    assumes \true;
    assigns k \from \nothing;
    assigns u \from u;

    
*/
void main1(void)
{
  f();
  u = v;
  k = 3;
}

int a[10];

/*@ assigns a[3], a[4], a[5] \from \nothing;  */
void main15(void){
  a[3] = 2;
  a[4] = 3;
  a[5] = 4;
}


/*@ assigns a[3..5] \from \nothing;
    behavior true:
    assumes \true;
    assigns a[2..5] \from a[..]; 
    
    behavior wrongassigns:
    assumes \true;
    assigns a[3..4] \from \nothing;

    behavior true2:
    assumes \true;
    assigns a[..] \from a[..];

    behavior wrongfrom:
    assumes \true;
    assigns a[1..] \from \nothing; */
void main2(void){
  a[3] = 2;
  a[4] = 3;
  a[5] = 4;
}

/*@ assigns a[i] \from (indirect:i),a[..]; */
void main3(int i){
  a[i] = 3;
}


int constante = 2;

/*@ 

  behavior true:
  assigns a[constante] \from \nothing;

  behavior wrong:
  assumes \true;
  assigns a[..] \from a[i]; 
*/
/* This is correct: actually, only a[2] is assigned from \nothing.  */
void main4(int i){
  a[2] = 3;
}

/*@ 
  behavior wrong:
  assumes \true;
  assigns a[..] \from a[i]; 

  behavior true:
  assigns a[2] \from a[sizeof(int)];

*/
void main5(int i){
  a[2] = a[4];
}

/*@ assigns a[i..j]; */
void main6_wrong(int i, int j){
  a[2] = 0;
  a[3] = 0;
  a[4] = 0;
  a[5] = 0;
  a[6] = 0;
}

/*@ assigns a[i..j]; */
void main6_right(int i, int j){
  a[3] = 0;
  a[4] = 0;
  a[5] = 0;
}

/*@ behavior right:
     assigns a[\union(2,7,8)];

    behavior wrong:
    assigns a[\union(2,7)]; */
void main7(void){
  a[2] = 0;
  a[7] = 0;
  a[8] = 0;
}


/*@ assigns *p \from (indirect:p); */
void main8(int *p){
  *p = 4;
}

/*@ assigns *(char *)\null;
    assigns \empty;
    assigns a[\union(1,\empty)];
    assigns a[1+2];
    assigns a[(int)1+2];
  */
void main9(void) {}

/*@ assigns \result \from a,b;
    ensures a <= \result <= b; */
extern int Frama_C_interval(int a,int b);


void main10(void){
  int t10[50];
  extern int c(void);
  int i;
  for( i = 0; i < 50; i++) { if(c()) t10[i] = 3; }

  /*@ assert \initialized( &t10[11..25] + (3..12)); */
}

/*@ assigns a[i == 4? 1 : 2] \from \nothing; */
void main11(int i){
  a[2] = 3;
}


int t[100]; int* p_t = t;

/*@
  behavior ok:
  assigns \result \from \nothing;
 */
int * main12 (void) { return t; }


/*@
  behavior ok:
  assigns \result \from p_t;

  behavior bad:
  assigns \result \from \nothing;
 */
int * main13 (void) { return p_t; }

/*@ assigns t[\union((2 .. 17) , (18 .. 38))] \from \nothing; */
void main14(void){
  for(int i = 2; i <= 38; i++) {t[i] = i; }
}

/*@ assigns t[\union((2..17),\union(1,19,18))] \from \nothing; */
void main16(void){
  for(int i = 1; i <= 19; i++) {t[i] = i; }
}

int t17[10];

//@ assigns *(char*)(&p[0..i-1]) \from p, i;
void main17(int *p, int i) {
  for (int j = 0; j<i; j++) {
    *((char*)(p+j)) = j;
  }
}

typedef struct {
   int *addr;
   int i;
} s18;

//@ assigns *x, *y \from *x, *y;
void f18 (s18 *x, s18 *y);

void main18() {
  int base_a = 17;
  s18 a;
  a.addr = & base_a;
  int base_b = 11;
  s18 b;
  b.addr = & base_b;
  f18(&a, &b); /* There are bottom bits in a and b. Make sure that the proper
                  conflate_bottom is used to evaluate the contents of the from
                  clauses */
}


void main(void)
{
  constante = 2;
  main1();
  main15();
  main2();
  int j = Frama_C_interval(0,9);
  main3(6);
  main3(j);
  main4(j);
  main5(j);
  int a = Frama_C_interval(0,3);
  int b = Frama_C_interval(6,9);
  main6_right(a,b);
  main6_wrong(a,b);
  main7();

  int c, d;
  main8(&c);
  int *p = (j == 2? &c : &d);
  main8(p);

  main9();
  main10();
  main11(3);
  main11(4);
  main11(Frama_C_interval(3,4));
  main12();
  main13();
  main14();
  main16();
  main17(t17, 10);
  main18();
}


