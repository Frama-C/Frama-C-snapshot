
#define STATE_FREE  1
#define STATE_USE   0

struct struct_my {
    char state;
    int data[];
}  /* __attribute__((__packed__)) */;

typedef struct struct_my my_structure;

int * my_tab;

void f(void)
{
    my_structure * p;
    p = (my_structure *)my_tab;
    p->state = STATE_FREE;
    p->data [2] = 99;
}

int G[10] = {77,88,99,100};

int main(void){
  my_tab = &G[1];
  f();
  return 1;}
