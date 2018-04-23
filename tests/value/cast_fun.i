/* run.config*
   STDOPT: #"-calldeps"
*/

//@ assigns \nothing;
int f1();
//@ assigns \nothing;
int f2(void);
//@ assigns \nothing;
int f3(int);
//@ assigns \nothing;
void f4(int);
//@ assigns \nothing;
int f5(int, int);

struct s1 {
  int f1;
  int f2;
};

struct s2 {
  int f1;
  int f2;
};

struct s3 {
  int f1;
  int f2;
  int f3;
};

//@ assigns \nothing;
void f6(struct s1);

//@ assigns \nothing;
void f7(struct s2);

//@ assigns \nothing;
void f8(struct s3);

//@ assigns \nothing;
void f9(int, ...);

//@ assigns \nothing;
void f10();


void main(volatile int c) {
  int (*p1)(int);
  void (*p2)(struct s1);
  void (*p3)(int, ...);
  void (*p4)();
  void (*p5) (int);
  int* y;
  int x;
  struct s1 s = {0};

  if (c){
    p1 = f1;
    x = (*p1)(c);
  }

  if (c){
    p1 = f2;
    x = (*p1)(c);
  }

  if (c){
    p1 = f3;
    x = (*p1)(c);
  }

  if (c) {
    p5 = f3;
    (*p5)(c);
  }

  if (c){
    p1 = f4;
    x = (*p1)(c);
  }

  if (c){
    p4 = f5;
    (*p4)(c);
  }

  if (c){
    p2 = f6;
    (*p2)(s);
  }

  if (c){
    p2 = f7;
    (*p2)(s);
  }

  if (c){
    p2 = f8;
    (*p2)(s);
  }

  if (c) {
    p3 = f9;
    (*p3)(c,c);
  }

  if (c) {
    p4 = f10;
    (*p4)(c,c);
  }

// Not allowed any more: you can't mix function without prototype and variadic.
//  if (c) {
//    p4 = f9;
//    (*p4)(c,c);
//  }

  if (c) {
    p4 = f10;
    (*p4)(c,c);
  }

  if (c) {
    p1 = f10;
    x = (*p1)(c);
  }

  if (c) {
    p1 = f10;
    (*p1)(c);
  }

  if (c) {
    p4 = f1;
    (*p4)(c);
  }

  if (c) {
    y = f1;
    p4 = y;
    (*p4)(c);
  }

}
