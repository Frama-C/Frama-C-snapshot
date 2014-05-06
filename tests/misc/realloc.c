/*@ assigns ((char*)\result)[0..s-1] \from ((char*)p)[0..s-1]; */
void *Frama_C_realloc(void *p, unsigned long s);

int main(int c){
  int *p = Frama_C_alloc_size(sizeof(int));
  *p = 17;
  int *pp = p;
  Frama_C_dump_each();
  int *q = Frama_C_realloc(p, 2 * sizeof(int));
  Frama_C_dump_each();
}
