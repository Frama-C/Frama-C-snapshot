typedef int T;
extern T f(char* p,int q, int i);

T G,H,I;
void main (void) {
  G = f((char*)&H,(int)&I,17);
  if (G == -1) G++;


}
