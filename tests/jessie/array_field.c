
struct S {
  int ai[1];
  int aj[2];
};

struct S sg;

/*@ ensures sg.aj[0] == sg.ai[0] - 1;
  @ ensures sg.aj[1] == sg.ai[0] + 1;
  @ */
void fg() {
  sg.aj[0] = sg.ai[0] - 1;
  sg.aj[1] = sg.ai[0] + 1;
}

void fl() {
  struct S lg;
  lg.aj[0] = lg.ai[0] - 1;
  lg.aj[1] = lg.ai[0] + 1;
  /*@ assert lg.aj[0] == lg.ai[0] - 1; */
  /*@ assert lg.aj[1] == lg.ai[0] + 1; */
}

/* 
Local Variables:
compile-command: "LC_ALL=C make array_field"
End:
*/
