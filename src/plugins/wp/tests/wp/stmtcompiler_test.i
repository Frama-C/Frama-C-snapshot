/* run.config
   OPT: -load-script tests/wp/stmtcompiler_test.ml -wp-msg-key success-only
*/

int empty (int c){
  /*@ assert \true; */
  return c;
}

int one_assign (int c){
  int d;
  d = c;
  /*@ assert d == c; */
  return d;
}

int one_if (int c){
  /*@ assert \true; */
  if(c) return 1;
  else return 2;
}

int some_seq (int c){
  int d;
  d = 0;
  if(c) d = 1;
  else d = 2;
  /*@ assert (c != 0 ==> d == 1); */
  /*@ assert (c == 0 ==> d == 2); */
  d += 1;
  return d;
}

/*@
  ensures \result == 0;
  assigns \nothing;
  @*/
int ensures_result(void);

int main_ensures_result(){
  int x;
  x = ensures_result();
  /*@ assert x == 0; */
  return 1;
}

int foo = 42;

void main(){
  /*@ assert foo == 42; @*/
}

void not_main(){
  /*@ assert bad: foo == 42; @*/
}

/*@
  ensures \result == x;
  ensures foo == x;
  assigns foo;
  @*/
int assigns_global(int x);

void main_assigns_global(int x){
  foo = 1;
  int r = assigns_global(x);
  /*@ assert x == 2 ==> foo == 2; */
  /*@ assert foo == r; */
  /*@ assert bad: foo == 1; */
}

/*@
  requires x >= 0;
  ensures \result >= 0;
  @*/
int zloop(int x){
  int i = 0;

  i++;
  /*@ loop invariant i <= 10; @*/
  for(; i < 10; i++){
    i++;
  }
  /*@ assert i >= 10; @*/
  /*@ assert i == 10; @*/
  /*@ assert x >= 0; @*/
  if(foo==0){ /*@ assert bad: i == 1; @*/ 0; }
  return x;
}


/*@
  behavior zero:
    assumes x == 0;
    ensures \result <= 0;
  behavior pos:
    assumes x >= 0;
    ensures \result >= 0;
  behavior neg:
    assumes x < 0;
    ensures \result < 0;
  complete behaviors zero, pos, neg;
  @*/
int behavior1(int x);

int behavior2(){
  int x;
  x = behavior1(-1);
  /*@ assert x < 0; */
  return 1;
}

int behavior3(){
  int x;
  x = behavior1(1);
  /*@ assert x >= 0; */
  return 1;
}

int behavior4(){
  int x;
  x = behavior1(0);
  /*@ assert x == 0; */
  return 1;
}


int behavior5(){
  int x;
  x = behavior1(1);
  /*@ assert bad: x < 0; */
  return 1;
}

int if_assert(int x, int y){
  if (x < 0) {
    /*@ assert x < y * y; @*/
  } else {
    x = - x;
    /*@ assert x < y * y; @*/
  }
  /*@ assert 0 < (y * y) - x; @*/
}

int compare(int a, int b, int c, int d) {
  int r;
  if(a > b)
    r = -1;
  else if((a == b) &&  ((c < d)))
    r = -1;
  else if((a == b) && !((c < d)))
    r = 0;
  else r = 1;
  /*@ assert (a < b) ==> r == 1; @*/
  return r;
}
