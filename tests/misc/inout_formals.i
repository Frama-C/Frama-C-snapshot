/*run.config
  OPT: -inout -input-with-formals  -inout-with-formals 
*/
int x, y;

void main(int * const i) {
    *i=0;
    Frama_C_show_each(i);
    if (*i==x) *i=y;
}
