
int f() {
  goto l1;
 l2: goto l3;
 l1: goto l2;
 l3: return 0;
}
