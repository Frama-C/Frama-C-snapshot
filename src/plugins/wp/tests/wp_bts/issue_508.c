#define HASHTBL_LEN 17

typedef struct {
  int b;
  int size;
} Buckets;

typedef struct {
  Buckets data[HASHTBL_LEN];
  int size;
} Hashtbl;

/*@ requires \valid(tbl);
  @ requires \valid(tbl->data+(0 .. HASHTBL_LEN - 1));
  @ requires 0 <= d < HASHTBL_LEN;
  @   
  @ assigns tbl->data[d], tbl->size; // approximation sur tbl->data[d]; si on écrit tbl->data[d].size, pas de problème
  @ */
int add(Hashtbl *tbl, int d) {
  unsigned int h = d;
  tbl->data[h].size = 0; // si on inline d, pas de probleme
  tbl->size = 0;
  return 0;
}
