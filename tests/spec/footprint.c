struct S {
  char *x;
  int *y;
};

/*@ logic set<char*> footprint(struct S s) =
  \union(s.x, (char*)s.y + (0 .. sizeof(int) - 1)) ;
*/

/*@
   requires \valid(\union(s,footprint(*s)));
   assigns footprint(*s);
*/
int f(struct S* s) {
  return *s->x + *s->y;
}
