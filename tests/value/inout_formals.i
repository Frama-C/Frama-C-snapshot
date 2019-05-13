/*run.config*
  OPT: -no-autoload-plugins -load-module from,inout @EVA_CONFIG@ -inout -input-with-formals  -inout-with-formals
*/
int x, y;

void main(int * const i) {
    *i=0;
    Frama_C_show_each(i);
    if (*i==x) *i=y;
}
