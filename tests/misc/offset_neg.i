char mem[1];
char *addr = mem;
unsigned long long off = -1;

int G[5];
int main () {
  addr = addr + off + 1;
  Frama_C_dump_each(); // the offset of addr overflow 64 bits. Beware in the pretty-printer
  G[-1] = 0;
}
