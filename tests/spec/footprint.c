struct S {
  char *x;
  int *y;
};

/*@ logic set<char*> footprint(struct S s) =
  \union(s.x, (char*)s.y + (0 .. sizeof(int) - 1)) ;
*/

/*@
   requires \valid(\union(s,footprint(*s)));
   assigns *footprint(*s);
*/
int f(struct S* s) {
  return *s->x + *s->y;
}


struct V {
  char x;
  char t[10];
};

/* This version should NOT be accepted in an assigns clause:
   footprint2 returns a set of char with no link to the original
   locations. */

/*@ logic set<char> footprint2{L}(struct V *s) =
  \union(s->x, s->t[0..5]) ;
*/

/*@
   requires \valid(s);
   assigns footprint2(s);
*/
void g(struct V* s) {
  s->x = 1;
  s->t[4] = 1;
}
