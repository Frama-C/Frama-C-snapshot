/* run.config*
   OPT: -val @VALUECONFIG@
*/

int main() {
  Frama_C_show_each(sizeof(unsigned int));

  unsigned int i = 0;
  while (u())
    {
      i+=2;
    }
}

