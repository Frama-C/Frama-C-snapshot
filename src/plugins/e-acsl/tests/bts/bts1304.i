/* run.config
   COMMENT: argument of functions must be kept, so keep its parameter
*/

struct msgA { int type; int a[2]; };
struct msgB { int type; double x; };
union msg {
  struct { int type; } T;
  struct msgA A;
  struct msgB B;
};

void read_sensor_4(unsigned* m) {
  /* put 4 bytes from sensors into m */
  *m = 0;
}

int main(void) {
  unsigned char buf[sizeof(union msg)];
  int i;
  for(i = 0; i < sizeof(buf)/4; i++)
    read_sensor_4((unsigned*)buf+i);
  /*@ assert \initialized((union msg*)buf); */
  return 0;
}
