/* see bug #43 */

/*@ requires x >=0; */
int f (int x);

/*@ requires y <= 0; */
int f (int y);

int main () {
  f (0);
  return 0;
}
