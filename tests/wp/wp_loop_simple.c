/* run.config
   DONTRUN: Fatal error: exception Calculus.Invalid_model("Hoare")
*/
void main () {
int X,c;

//@ loop invariant 0 <= c <= 6 && ((c==0 || X == c-1));
 for(c=0;c<=5;) {
//   CEA_DUMP();
   X = c;
   c++;
//   CEA_DUMP();
}
// CEA_DUMP();
//@ assert X == 5 && c == 6;
}
