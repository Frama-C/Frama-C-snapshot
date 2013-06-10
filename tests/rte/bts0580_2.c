/* run.config
   OPT: -rte -warn-signed-overflow -rte-no-all -rte-mem -print
*/

typedef int (*fptr)(int);

struct S {
  int val;
  struct S* next;
};

struct C {
  struct S cell[5];
  fptr f;
};

struct ArrayStruct {
   struct C data[10];
};

struct ArrayStruct buff ;


int main(int i ) 
{
  int a, b,d;
  int c[3];
  int* p;
  fptr f;

  a = buff.data[i].cell[*p].next->val;
  b = buff.data[c[2]].f(c[1]);
  d = f(buff.data[0].cell[0].val);
  return a > b;
}
