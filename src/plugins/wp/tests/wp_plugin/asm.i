void main() {
  int src = 1;
  int dst = 2;

  asm ("mov %1, %0\n\t"
       "add $1, %0"
       : "=r" (dst)
       : "r" (src));

  //@ assert OK: src == 1;
  //@ assert KO: dst == 2;
}
