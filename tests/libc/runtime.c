/* run.config*
   COMMENT: tests that the runtime can compile without errors (for PathCrawler, E-ACSL, ...)
   CMD: gcc -fsyntax-only -D__FC_MACHDEP_X86_64 share/libc/__fc_runtime.c -Wno-attributes -std=c99
   OPT:
 */

int main() {
  return 0;
}
