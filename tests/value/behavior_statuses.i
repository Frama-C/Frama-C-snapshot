/* run.config*
   STDOPT: +"-kernel-msg-key property_status -kernel-verbose 3"
   STDOPT: +"-kernel-msg-key property_status -kernel-verbose 3 -val-use-spec f"
*/

struct s { int t; };
volatile int nondet;

/*@
  requires \valid_read(filename);
  assigns \result \from nondet;
  behavior t_null:
    assumes t == \null;
  behavior t_not_null:
    assumes t != \null;
    requires \valid_read(t);
  disjoint behaviors;
  complete behaviors;
 */
int f(const char *filename, const struct s *t) {
  return 1;
}

int main() {
  struct s t = {1};
  int r1 = f("/tmp/foo", 0);
  int r2 = f("/tmp/foo", &t);
  return 0;
}
