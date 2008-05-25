
struct S {
  int ii;
  int jj;
};

struct S ga1[1];
struct S ga2[2];
struct S ga3[3] = { 0, 0 };

/*@ ensures ga1[0].ii == i; 
  @ ensures ga2[1].ii == i; 
  @ ensures ga3[2].ii == i; 
  @ */
void fg(int i) {
  ga1[0].ii = i;
  ga2[1].ii = i;
  ga3[2].ii = i;
}

void testg() {
  fg(5);
  /*@ assert ga1[0].ii == 5; */
  /*@ assert ga2[1].ii == 5; */
  /*@ assert ga3[2].ii == 5; */
}

void lg(int i) {
  struct S la1[1];
  struct S la2[2];
  struct S la3[3] = { 0, 0 };

  la1[0].jj = i;
  la2[1].jj = i;
  la3[2].jj = i;

  /*@ assert la1[0].jj == i; */
  /*@ assert la2[1].jj == i; */
  /*@ assert la3[2].jj == i; */
}


/* 
Local Variables:
compile-command: "LC_ALL=C make array_struct"
End:
*/
