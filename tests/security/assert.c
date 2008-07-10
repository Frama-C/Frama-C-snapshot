/* run.config
   GCC:
   OPT: -security-analysis -security-lattice weak -security-propagate-assertions
   */

void main() {
  int x = (int /*@ public */) 0;;
  int a = 5;
  //@ assert security_status(x) == public;
  //@ assert security_status(a) == public; // alarm
  //@ assert security_status(a) == public; 
}
