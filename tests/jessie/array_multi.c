
int ga1[1][1];
int ga2[2][2];
int ga3[3][3] = { 0, 0 };

/*@ ensures ga1[0][0] == i;
  @ ensures ga2[1][1] == i;
  @ ensures ga3[2][2] == i;
  @ */
void fg(int i) {
  ga1[0][0] = i;
  ga2[1][1] = i;
  ga3[2][2] = i;
}

void testg() {
  fg(5);
  /*@ assert ga1[0][0] == 5; */
  /*@ assert ga2[1][1] == 5; */
  /*@ assert ga3[2][2] == 5; */
}

void lg(int i) {
  int la1[1][1];
  int la2[2][2];
  int la3[3][3] = { 0, 0 };

  la1[0][0] = i;
  la2[1][1] = i;
  la3[2][2] = i;

  /*@ assert la1[0][0] == i; */
  /*@ assert la2[1][1] == i; */
  /*@ assert la3[2][2] == i; */
}


/* 
Local Variables:
compile-command: "LC_ALL=C make array_multi"
End:
*/
