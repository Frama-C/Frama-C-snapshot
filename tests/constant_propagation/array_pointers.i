/* run.config
   OPT: -val -val-show-progress -scf -val-show-progress -journal-disable
*/

void *p;

void main() {
  void **q = &p+1;
  void **r = q+1;
  void *s = p + 1;
}
