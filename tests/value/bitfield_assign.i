int g_18;

typedef unsigned int uint32_t;
typedef int int32_t;
typedef short int16_t;
typedef long long int64_t;

struct S0 {
   uint32_t f0;
   int16_t f1;
  signed f2 : 26;
   int64_t f3;
};

union U3 {
  signed f0 : 7;
   int32_t f1;
   int32_t f2;
   struct S0 f3;
};

static union U3 g_7[1] = {{0x00868BB4L}};

int g_5;
int g_2;

void Frama_C_show_each(unsigned);

int main(){
  unsigned short l_8 = 1UL;
  unsigned int l_16 = 0xBD4AA41AL;

  g_2 |= (g_7[g_5].f3.f2 = l_16);
  Frama_C_show_each(g_2);
  return 0;
}
