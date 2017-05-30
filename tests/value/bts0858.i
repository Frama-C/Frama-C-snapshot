typedef long int32_t;
typedef unsigned long uint32_t;


int main() {
  uint32_t tmp = 18446744073709551615UL ;
  if (0xffffffffUL == tmp)
    return 1;
  return 0;
}
