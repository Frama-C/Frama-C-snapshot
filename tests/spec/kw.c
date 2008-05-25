typedef int assert;

assert behavior = 0;

/*@ logic assert foo(assert x) = x; */

/*@ requires behavior >= 0;
    assigns behavior \from behavior;
    ensures behavior >= 0;
*/
int main () {
  behavior++;
  return 0;
}
