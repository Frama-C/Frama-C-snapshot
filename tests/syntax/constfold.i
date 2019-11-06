typedef struct {
  int a;
} st;

enum { PAD = 2 };

int main() {
  char a[sizeof(st) >= PAD ? 1 : PAD] = {0};
  int i = (signed char)256  ? 42 : 36; // UB prior simplification.
  int j = (unsigned char)256  ? 42 : 36;
}
