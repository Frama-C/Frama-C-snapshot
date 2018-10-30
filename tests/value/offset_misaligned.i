char t[300];

int main(unsigned char u){
  t[0] = 1;
  *(int*)(t+1) = 2;
  t[u+10] = 3;
  return 0;
}
