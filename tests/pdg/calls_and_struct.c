/* run.config
   GCC:
   OPT: -deps -input -out -inout -pdg -journal-disable -pdg-print -pdg-verbose 2
   */

struct Tstr { int a; int b; int c; };

struct Tstr S;
int A, B, C;

int f (struct Tstr s) {
  A += s.a;
  S.a = S.b;
  return s.b;
}

int asgn_struct (void) {
 struct Tstr s = S;
 return s.a; /* \result FROM S{.a; .b; .c; }; */
}

int main () {
  int a = asgn_struct ();
  A = a;
  B = 2;
  C = 3;
  return f (S);
}
