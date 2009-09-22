/*@ requires \valid(p) && \valid(q);
    ensures *p == \old(*q);
    ensures *q == \old(*p);
    assigns *p, *q;
*/
void Swap(int *p, int *q)
{
  int temp;
  temp = *p;
  *p = *q;
  *q = temp;
}



/*@ requires \valid(a+ (0..k));
*/
void foo(int a[], int k) {
  Swap(&a[0], &a[k]);
}

