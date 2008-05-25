char T[10]={0};
int IT[10]={0};
int G;
char H;

int main () {
  G = *((int*)&(T[1]));
  H = *((char*)&(IT[9]));
  return T[0];
}
