/* run.config
   EXECNOW: BIN multi_project.sav LOG multi_project_sav.res LOG multi_project_sav.err ./bin/toplevel.opt -save ./tests/saveload/result/multi_project.sav -semantic-const-folding ./tests/saveload/multi_project.i > tests/saveload/result/multi_project_sav.res 2> tests/saveload/result/multi_project_sav.err
   EXECNOW: make -s ./tests/saveload/multi_project.opt
   OPT: -load ./tests/saveload/result/multi_project.sav -journal-disable
   CMD: ./tests/saveload/multi_project.opt -val
   OPT:
*/
int f(int x) {
  return x + x;
}

int main() {
  int x = 2;
  int y = f(x);
  /*@ assert y == 4; */
  return x * y;
}
