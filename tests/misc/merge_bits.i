char T[] = { 1,0,0,0,1,2,3,4,5,0,1,1,1 } ;
int main() {
  CEA_F(*((int*)(T)));
  CEA_F(*((int*)(T+1)));
  CEA_F(*((int*)(T+4)));
  CEA_F(*((int*)(T+9)));
  *((int*)(T+2))=2U<<31 | 2U << 30 | 2U << 27 | 2U << 3;
  CEA_F(*((int*)(T)));
  return 0;
}
