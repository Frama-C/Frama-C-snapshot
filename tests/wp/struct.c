struct S { int i; };

/*@ ensures \forall struct S v ;  (v.i == 0 ==> \result == v);
  @ */
struct S main() {
  struct S s = { 0 };
  return s;
}
