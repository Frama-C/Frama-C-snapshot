#include "string.h"

typedef unsigned char uint8_t ;
typedef struct {
  uint8_t length;
  uint8_t value[];
} TcpOption;

void main (void) {
 uint8_t buf [100];
 uint8_t value[2] = { 15, 20 };
 buf[0] = 42;
 TcpOption * option = buf + 10;
 option->length = 5;
 Frama_C_show_each(option->value);
 memcpy(option->value, value, 2);
}
