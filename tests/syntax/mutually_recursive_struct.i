struct S1;
struct S2;

struct S1 { struct S2 s2[2]; int x; };

struct S2 { struct S1 s1[2]; int y; };

int main () {
  struct S1 s1;
  /*@ assert s1.s2[0].s1[1].x != 0; */
  return s1.s2[0].s1[1].x;
}
