/* run.config
   OPT: -rte -rte-print -machdep x86_32 -journal-disable
   OPT: -rte -rte-print -machdep x86_64 -journal-disable
*/


int main() {
int x; long y;

 x = 5 << 30;
 y = 5L << 30;
 
 return 0;
}
