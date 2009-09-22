
//@ requires \valid(x);
// void change_endianness(int * x) {
void main (int * x) {
char * p = (char* ) x;
int t;

 /*@ assert \valid(p); */
  /*@ assert \valid(p+1); */
  /*@ assert \valid(p+2); */
  /*@ assert \valid(p+3); */
  /*@ assert ((8+(int )p[1] >= 0) && (8+(int )p[1] < 32)); */
  /*@ assert ((16+(int )*p >= 0) && (16+(int )*p < 32)); */


t = *(p+3) + *(p+2)<<8 + *(p+1)<<16 + *p<<24;
*x = t;
}

