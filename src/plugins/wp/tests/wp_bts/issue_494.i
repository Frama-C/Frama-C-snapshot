//@ predicate R(integer x,integer y) = (y == x+1) ;

/*@ 
  ensures R(\old(*p),*p);
*/
void f(int *p) { (*p)++; }

void job_ko_fixed(int x) {
  f(&x);
  //@assert Wrong: \false;
  x++;
}

void job_ko_success(int y) {
  f(&y);
  //@assert Wrong: \false;
}
