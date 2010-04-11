/* see bug 131 */

void g(int a[]);

/*@ predicate p(int *a) = \valid_index(a,0); */

/*@ requires p(a); */
void f(int a[]) {
  g(a);
}
