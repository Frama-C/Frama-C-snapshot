
struct S {
  int i;
  int j;
};

struct T {
  struct S ai[1];
  struct S aj[2];
};

struct T sg;

/*@ ensures sg.aj[0].i == sg.ai[0].j - 1;
  @ ensures sg.aj[1].j == sg.ai[0].i + 1;
  @ */
void fg() {
  sg.aj[0].i = sg.ai[0].j - 1;
  sg.aj[1].j = sg.ai[0].i + 1;
}

void fl() {
  struct T lg;
  lg.aj[0].i = lg.ai[0].j - 1;
  lg.aj[1].j = lg.ai[0].i + 1;
  /*@ assert lg.aj[0].i == lg.ai[0].j - 1; */
  /*@ assert lg.aj[1].j == lg.ai[0].i + 1; */
}

/* 
Local Variables:
compile-command: "LC_ALL=C make array_and_field"
End:
*/
