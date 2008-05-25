
typedef __attribute__((__packed__ (1,256,1))) struct {
  volatile char         Reserved[4];
} T_HCCA;

#define __packed__(...) __attribute__((__packed__(__VA_ARGS__)))

typedef __packed__ (1,256,1) struct {
  volatile char         Reserved[4];
} T_HCCB;

T_HCCA x;
T_HCCB y;

void main() {

}
