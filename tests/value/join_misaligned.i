/* run.config*
   STDOPT: #"-eva-warn-key garbled-mix -big-ints-hex 257"
*/

int t[5]={0};
int u[5]={1,1,1,1,1};
int v[7]={0x22222222,0x22222222,0x22222222,1,1,1,1};
int w[7]={0};
char x[5]={0};
int y[7]={0x22222222,0x22222222,0x22222222,1,1,1,1};
unsigned char z[5] = {0xFF,0xFF,0xFF,0xFF,0xFF};
unsigned int a;
volatile unsigned short va;

void main(int c)
{
  if (c)
    {
      ((char*)t)[6]='a';
      ((char*)u)[6]='c';
      *((short*)((char*)v+6))=0x44444444;
      *((short*)((char*)w+6))=57;
      *((int*)((char*)y+6))= (int) &t;
      *((short*)(&z[3])) = 0x1111;
      *((short*) &a) = 0xFFFF;
      *((short*) &a+1) = 0xFFFF;
    }
  else
    {
      ((char*)t)[6]='b';
      ((char*)u)[6]='d';
      *((short*)((char*)v+7))=0x55555555;
      *((short*)((char*)w+7))=59;
      x[0]=1;
      x[1]=0;
      x[2]=1;
      *((int*)((char*)y+7))= (int) &u;
      a = va;
      a <<= 12;
      a--;
    }
}
