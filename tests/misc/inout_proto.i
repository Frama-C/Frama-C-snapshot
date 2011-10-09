/*run.config
  OPT: -inout -input-with-formals  -inout-with-formals
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
