/* run.config
   STDOPT: +"-plevel 40" +"-big-ints-hex 0x55"
*/

int t[0xFFFF];

volatile int i;

void main() {
  int i1 = i;
  //@ assert 0 <= i1 <= 0x20;
  int i2 = i;
  //@ assert 0 <= i2 <= 0x40;
  
  t[0x100 + i1] = 1;

  t[0x200 + i2] = 2;

  t[0x300 + 2*i1] = 3;

  t[0x400 + 2*i2] = 4;

  int *p;
  p = &t[0x500+i1];
  *p = 0x5555;
  p = (int*)((short*)p+1);
  *p = 0x5656;

  p = &t[0x600+i2];
  *p = 0x6666;
  p = (int*)((short*)p+1);
  *p = 0x6767;

  p = ((char*)&t[0x700])+i1;
  *p = 7;

  p = ((char*)&t[0x800])+i2;
  *p = 8;
}
