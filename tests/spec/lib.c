/* run.config
   OPT: -pp-annot -cpp-command="gcc -C -E -I tests/spec" -cpp-extra-args="-include lib.h" -print -journal-disable
*/

/*@ ensures f((int)0) == (int)0; */
int main () { return 0; }
