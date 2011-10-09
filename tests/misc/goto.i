int stop () {
 L: goto L;

}

int main() {
  volatile int c=0;
  c = c?1:0;

  if (c) stop ();

}
