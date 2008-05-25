/* run.config
   GCC:
   OPT: -security-analysis -lib-entry f -security-lattice weak
   OPT: -security-analysis -lib-entry f -security-lattice strong
   */

/*@ requires security_status(x) == public(); */
void send(int x);

/*@ ensures security_status( *x ) == public(); */
void crypt(int* x);

/*@ ensures security_status( *x ) == private(); */
void uncrypt(int* x);

int c;
int /*@ public */ d;

int f() {
  int x = (int /*@ public */) 0;
  int y = 1;
  int z = x;

  send(y);   /* faille averee */
  send(z);
  crypt(&y); /* y devient public */
  send(y);

  if (x) uncrypt(&y); /* code mort */
  send(y);

  if (d) uncrypt(&y);
  send(y);   /* faille potentielle */

  crypt(&y); /* y devient public */
  if (c) y = z;
  send(y);   /* faille potentielle si dep de ctrl */

  return 0;
}


/*
Local Variables:
compile-command: "../../bin/toplevel.opt  -security-analysis -lib-entry f \
                  -security-lattice weak simple_example.c"
End:
*/
