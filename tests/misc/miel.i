/* run.config
   DONTRUN: cannot find entry point: main
*/

void g() { j();
}

void j() { j();}

void h() {
  j();
}

void h1() {
  j();
}

void h2() {
  j();
}

void h3() {
  j();
}

void h4() {
  j();
}

void h5() {
  j();
  p0();
}

void l1() {
  l2();
}

void l2() {

}

void r0() {
  l1 ();
}
void r1() {
  l1 ();
}
void r2() {
  l1 ();
}
void r3() {
  l1 ();
}

/*
void ldkfadl(void)
{
  p1();
}
*/

void p0() {
  //  p1 ();
}
void p1() {
  p2 ();
  p3 ();
  p0();
}
void p2() {
  p1 ();
  p3 ();
}
void p3() {
  p1 ();
  p2 ();
}


void g0() {
  g2 ();
}
void g1() {
  g2 ();
}
void g2() {
  g3 ();
}
void g3() {
  g4 ();
  g5 ();
}
void g4() {
  g6 ();  g3 ();
}
void g5() {
  g6 ();
}
void g6() {

}
