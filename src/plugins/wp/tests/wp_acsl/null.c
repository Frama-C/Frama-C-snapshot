
#define NULL ((void*)0)

//@ ensures \result == 0;
int null_is_zero (void) {
  void * p = NULL;
  return (int) p;
} 

/*@ lemma valid_non_null:      !\valid     ((char *)\null); */
/*@ lemma valid_read_non_null: !\valid_read((char *)\null); */


