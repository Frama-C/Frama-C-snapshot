typedef struct _list {
  int i;
} *list;

/*@ requires \valid(p); */
void f(list p) {}

/*@ requires \valid(p); */
void g(struct _list* p) {}
