/* run.config
   DONTRUN: cannot find entry point: main
*/

void f1() {
  f3();
}

void f2() {
  f4();
}

void f3() {
  f4 ();
}

void f4() {
  f3 ();
}
