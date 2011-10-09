/*run.config
  OPT: -inout -input-with-formals  -inout-with-formals 
*/
int x, y;

void main(int * const i) {
    *i=0;
    if (*i==x) *i=y;
}
