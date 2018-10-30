/* run.config
STDOPT: +"-unspecified-access"
*/
int x, *p;

int main(){
  p = &x;
  *p = (*p < 3);
  if (*p = (*p < 3))
    x = 4;
  return 0;
}
