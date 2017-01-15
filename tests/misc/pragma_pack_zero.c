/* run.config
   STDOPT: #"-machdep gcc_x86_64"
   STDOPT: #"-machdep msvc_x86_64"
 */
// #pragma pack(0) is not supported by MSVC, but allowed in GCC.
// In MSVC mode, we ignore it.
// In GCC, its current (undocumented) behavior is equivalent to #pragma pack(),
// that is, disable packing (reset to default). We emulate this behavior,
// but with a warning.

#include "pragma-pack-utils.h"

#include <stdint.h>
#include <stddef.h>

struct s1 {
  char a;
  int b;
};

#pragma pack(1)

struct s2 {
  char a;
  int b;
};

#pragma pack(2)

struct s3 {
  char a;
  int b;
};

#pragma pack(0)

struct s4 {
  char a;
  int b;
};

#define PRINT_VAR(V) \
PRINTF("  %s %s {" ZU "}\n", STR(V), IN, V)

int main() {
  size_t z1 = sizeof(struct s1);
  size_t o1 = offsetof(struct s1, b);
  size_t z2 = sizeof(struct s2);
  size_t o2 = offsetof(struct s2, b);
  size_t z3 = sizeof(struct s3);
  size_t o3 = offsetof(struct s3, b);
  size_t z4 = sizeof(struct s4);
  size_t o4 = offsetof(struct s4, b);
  PRINT_VAR(z1);
  PRINT_VAR(o1);
  PRINT_VAR(z2);
  PRINT_VAR(o2);
  PRINT_VAR(z3);
  PRINT_VAR(o3);
  PRINT_VAR(z4);
  PRINT_VAR(o4);
  return 0;
}
