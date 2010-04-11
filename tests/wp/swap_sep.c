

/*@
   requires \valid(p);
   requires \valid(q);
   requires \separated(p,q);

   assigns *p;
   assigns *q;

   ensures *p == \old(*q);
   ensures *q == \old(*p);
*/
void swap(int* p, int* q)
{
  int const save = *p;
  p++;p--;
  *p = *q;
  *q = save;
}
int main (void) { return 0 ; }
