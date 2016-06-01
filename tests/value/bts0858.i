typedef long int64_t;
typedef unsigned long uint64_t;


int main() {
  uint64_t tmp = 18446744073709551615UL ; 
  if (0xffffffffUL == tmp)
    return 1;
  return 0;
}
