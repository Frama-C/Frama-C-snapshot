#define SIZE (1<<23)
const int T[SIZE]={2,3};
const char*S = "uututututututu";

int main(int c) {
  int i;
  if (c) *(char*)S = 'E';
  if (c) for(i=0; i < SIZE/4; i++)
    *(int*)&T[i] = 1;

  return 0;
}
