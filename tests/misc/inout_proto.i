/*run.config
  OPT: -inout -input-with-formals  -inout-with-formals -inout-callwise -main main_main
*/

typedef unsigned char   BYTE;
typedef  BYTE *     MESSAGE_ADDR_TYPE;


//@ assigns *RETURN_CODE \from MESSAGE_ADDR[0..length], length;
extern void SendBuffer
     (const MESSAGE_ADDR_TYPE /* Array */      /* in */        MESSAGE_ADDR,
     const int /* in */ length,
     int * const       /* out */       RETURN_CODE);


void main(const MESSAGE_ADDR_TYPE msg)
{
   int ret;
   SendBuffer((MESSAGE_ADDR_TYPE) &msg, 4, &ret);
}


int a, b, c;

//@ assigns a, b, c \from b;
void f();

//@ assigns p[0..3] \from p[3..4];
void g(int *p);

int t[10], u[20];

void g1() {
  g(&t[3]);
}

void g2() {
  g(&t[0]);
}

void g3(int *p) {
  g(p);
}

void main2(int i) {
  f();
  g1();
  g2();
  if (i >= 5 && i <= 6)
    g3(&u[i]);
}

void main_main(const MESSAGE_ADDR_TYPE msg, int i) {
  main(msg);
  main2(i);
}
