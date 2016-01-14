char T[] = { 1,0,0,0,1,2,3,4,5,0,1,1,1 } ;
int main() {
  Frama_C_show_each_F(*((int*)(T)));
  Frama_C_show_each_F(*((int*)(T+1)));
  Frama_C_show_each_F(*((int*)(T+4)));
  Frama_C_show_each_F(*((int*)(T+9)));
  *((int*)(T+2))=2U<<31 | 2U << 30 | 2U << 27 | 2U << 3;
  Frama_C_show_each_F(*((int*)(T)));
  return 0;
}
