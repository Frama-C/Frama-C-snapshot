/* run.config
  OPT: -metrics-locals-size f,g,level3,level2,level1,main
  OPT: -metrics-locals-size recurse2
 */

// locals size without temps: 0
int f() {
  static int count = 0; // not a local
  count++;
  return count;
}

// locals size without temps: sizeof(k) + sizeof(l2) + sizeof(__retres)
int g() {
  char l1 = 3;
  int l2 = l1++; // force creation of a temporary
  return l1 + l2;
}

// locals size without temps: sizeof(p) + sizeof(k) + sizeof(__retres)
int level3(int p) {
  char k = 13;
  return k + p;
}

// locals size without temps: sizeof(a) + sizeof(b)
int level2(int a, int b) {
  return level3(a + b + a);
}

// locals size without temps: sizeof(f) + sizeof(t1) + sizeof(t2)
int level1(int f) {
  int t1 = f;
  int t2 = f + 2;
  return level2(t1, t2);
}

typedef int incomplete[];

// locals size without temps: sizeof(i) + sizeof(k) + sizeof(j) +
//                            sizeof(__retres)
int main() {
  incomplete i = {0};
  int k = f() + g(); // temporaries
  int j = level1(f());
  return 0;
}

int recurse2(int);

int recurse1(int k) {
  if (k < 2) return 1;
  else return recurse2(k-1);
}

int recurse2(int k) {
  if (k < 3) return 0;
  else return recurse1(k-1);
}
