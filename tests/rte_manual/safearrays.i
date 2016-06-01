/* run.config
   OPT: -safe-arrays -rte -then -print
 */

struct S {
   int val;
   struct S *next;
};

struct C {
   struct S cell[5];
   int (*f)(int);
};

struct ArrayStruct {
   struct C data[10];
};

unsigned int i, j;

int main() {
  int a;
  struct ArrayStruct buff;
  // some code

  a = (buff.data[i].cell[j].next)->val;

  (*(buff.data[i].f))(a);

  return 0;
}
