#define BAR ((unsigned long)0xFFFFF000000)
int PTR;

unsigned long G = (unsigned long)&PTR - BAR;

void main () {
  *((int*)(G+BAR)) = 1;
}
