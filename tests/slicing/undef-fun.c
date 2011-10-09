/* run.config
 OPT: -check -slice-undef-functions -slice-return f -journal-disable -then-on 'Slicing export' -print



 */

int G;

/*@ assigns \result \from a;
    assigns G \from b;
*/
int f (int a, int b);

int main (int x, int y) {
  x += 1;
  y += 2;
  x = f (x, y);
  return x;
}
