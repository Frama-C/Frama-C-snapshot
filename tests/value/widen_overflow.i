/* run.config*
   OPT: -no-autoload-plugins -load-module value,inout -val @VALUECONFIG@
*/

int main() {
  Frama_C_show_each(sizeof(unsigned int));

  unsigned int i = 0;
  while (u())
    {
      i+=2;
    }
}

