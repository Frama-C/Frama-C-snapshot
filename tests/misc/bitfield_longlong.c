/* run.config
   OPT: -val -cpp-command "gcc -C -E -Dprintf=Frama_C_show_each" -journal-disable
*/
struct X50 {
 long long int z:50;
} s50 = { 2 };
 
struct X10 {
 long long int z:10;
} s10 = { 2 };


struct U32 {
  unsigned long z:32;
} u32 = { -1 };

struct S32 {
  signed long z:32;
} s32 = { -1 };


int main() {
  int x = u32.z >=0;
  int y = s32.z >=0;
  printf("%zu %zu %zu %zu\n",
         sizeof(long long int),
         sizeof(s10.z+0),
         sizeof(s50.z+0),
         sizeof(u32.z+0)
         );
  printf("%d %d\n", x, y);
  return 0;
}
