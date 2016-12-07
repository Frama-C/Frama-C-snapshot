/* run.config
   OPT:  -lib-entry -main f -slice-calls send -journal-disable -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  
   OPT:  -lib-entry -main g -slice-calls nothing -journal-disable -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  
   */

void nothing (void);

void send(int x);

void crypt(int* x);

void uncrypt(int* x);

int c;
int d;

int f() {
  int x = 0;
  int y = 1;
  int z = x;

  send(y);   /* faille averee */
  send(z);
  crypt(&y); /* y devient public */
  send(y);

  if (x) uncrypt(&y); /* code mort */
  if (y) send(y);

  if (d) uncrypt(&y);
  send(y);   /* faille potentielle */

  crypt(&y); /* y devient public */
  if (c) y = z;
  send(y);   /* faille potentielle si dep. de contrôle */

  return 0;
}

void g (void) {
  c = 1;
  nothing ();
  d = 3;
}
