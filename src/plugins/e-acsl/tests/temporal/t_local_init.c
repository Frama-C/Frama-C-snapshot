/* run.config
   COMMENT: Check local compound variable initializers
*/

#include <stdio.h>
#include <stddef.h>

struct tree_desc {
   int *extra_bits ;
};

typedef struct tree_desc tree_desc;

struct tree_desc2 {
   struct tree_desc desc;
};

typedef struct tree_desc2 tree_desc2;

void build_tree(tree_desc *desc) {
  int *extra;
  extra = desc->extra_bits;
  /*@assert \valid(extra); */
}

char * Strings [2][2] = {
  { "the", "tha" },
  { "thi", "tho" }
};

char * Str [4] = { "foo", "bar", "baz", NULL };

int main(int argc, const char **argv) {
  char * strings [2][2] = {
    { "the", "tha" },
    { "thi", "tho" }
  };

  char **p = (char**)&strings[0];

  /*@assert \valid_read(p); */
  /*@assert \valid_read(*p); */

  char * str [4] = { "foo", "bar", "baz", NULL };

  p = (char**)&str;

  /*@assert \valid_read(p); */
  /*@assert \valid_read(*p); */

  char **P = (char**)&Strings[0];

  /*@assert \valid_read(P); */
  /*@assert \valid_read(*P); */

  P = (char**)&Str;

  /*@assert \valid_read(P); */
  /*@assert \valid_read(*P); */

  int extra_lbits[] = {0};

  tree_desc l_desc =
    { .extra_bits = extra_lbits };

  tree_desc descs [2] = {
    { .extra_bits = extra_lbits },
    { .extra_bits = extra_lbits }
  };

  tree_desc2 l_desc2 =
    { .desc = { .extra_bits = extra_lbits }  };

  tree_desc2 descs2 [2] = {
    { .desc = { .extra_bits = extra_lbits } },
    { .desc = { .extra_bits = extra_lbits } }
  };

  build_tree(&l_desc);
  build_tree(&descs[0]);
  build_tree(&descs[1]);
  build_tree(&l_desc2.desc);
  build_tree(&descs2[0].desc);
  build_tree(&descs2[1].desc);
  return 0;
}
