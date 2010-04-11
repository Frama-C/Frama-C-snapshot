# pragma SeparationPolicy(none)

/*@
   requires \valid(p);
   requires \valid(q);


   assigns *p;
   assigns *q;

   ensures *p == \old(*q);
   ensures *q == \old(*p);
*/
void swap(int* p, int* q)
{
  int const save = *p;
  *p = *q;
  *q = save;
}

int main () {
  int p[2] = { 0,1};
  int *q = (int *)((char*)q+1);
  swap(p,q);
}
