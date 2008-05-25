
//@ requires \valid(x);
void change_endianness(int * x) {
char * p = (char* ) x;
int t = *(p+3) + *(p+2)<<8 + *(p+1)<<16 + *p<<24;
*x = t;
}
