/* run.config
   OPT: -val -semantic-const-folding -journal-disable
*/

void *p;

void main() {
  void **q = &p+1;
  void **r = q+1;
  void *s = p + 1;
}
