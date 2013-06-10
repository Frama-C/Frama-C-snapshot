/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32
*/


int main() {
  long long z;

  z = 5LL << 63;
  
  return 0;
}
