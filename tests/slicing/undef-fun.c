/* run.config
 OPT: -slice-print -slice-undef-functions -slice-return f



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
