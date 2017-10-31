/* run.config
*/

void f(void) { }

int main(void)
  {
  void (*p)(void) = &f ;
  int x = __alignof__(p) ;
  return __alignof__(*p) ;
  }
