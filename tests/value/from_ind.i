/* run.config*
   STDOPT: +"-show-indirect-deps"
*/

int a[4];
int *p = a;
int r;

int main(void)
{
  r = *(p + 1);
}
