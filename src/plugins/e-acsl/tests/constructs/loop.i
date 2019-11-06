/* run.config_ci
   COMMENT: loop invariants
   STDOPT: +"-slevel 160"
*/

void simple_loop() {
  int sum = 0;
  /*@ loop invariant 0 <= i <= 10; */
  for(int i = 0; i < 10; i++)
    sum +=i;
}

void nested_loops() {
  int t[10][15];
  /*@ loop invariant 0 <= i <= 10; */
  for(int i = 0; i < 10; i++)
    /*@ loop invariant 0 <= j <= 15;
      @ loop invariant
      @   \forall integer k,l; 0 <= k < i && 0 <= l < j ==> t[k][l] == k * l; */
    for(int j = 0; j < 15; j++)
      t[i][j] = i * j;
}

void unnatural_loop() {
  int x = 0;
  /*@ loop invariant 0 <= i <= 6; */
  for(int i = 0; i < 10; i++) {
    if (x == 5) break;
    x = i;
  }
}

int main(void) {
  simple_loop();
  nested_loops();
  unnatural_loop();
}
