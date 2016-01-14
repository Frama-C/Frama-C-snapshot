/* run.config
   GCC:
   STDOPT: #"-main main"
   STDOPT: #"-absolute-valid-range 0-0x3"
*/
int *p,r=77;
void main () {
  r = *p;
}
