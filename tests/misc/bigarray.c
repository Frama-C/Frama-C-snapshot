#define SIZE (1<<23)
const int T[SIZE]={2,3};
const char*S = "uututututututu";

int main(int c) {
  int i;
  if (c) *(char*)S = 'E';
  for(i=0; i < SIZE/4; i++)
    *(int*)&T[i] = 1;
  for(i=0; i< SIZE/8; i++)
     *(int*)&T[i] = 1;
  for(i=0; i< SIZE/16; i++)
     *(int*)&T[i] = 1;
  for(i=0; i< SIZE/32; i++)
     *(int*)&T[i] = 1;

  return 0;
}
