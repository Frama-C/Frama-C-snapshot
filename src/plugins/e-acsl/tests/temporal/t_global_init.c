/* run.config
   COMMENT: Check global compound variable initializers
*/

struct tree_desc {
   int *extra_bits ;
};

typedef struct tree_desc tree_desc;

struct tree_desc2 {
   struct tree_desc desc;
};

typedef struct tree_desc2 tree_desc2;

static int extra_lbits[] = {0};

static tree_desc l_desc =
  /* This bit should be tracked via globals_init function */
  { .extra_bits = extra_lbits };

static tree_desc descs [2] = {
  { .extra_bits = extra_lbits },
  { .extra_bits = extra_lbits }
};

static tree_desc2 l_desc2 =
  { .desc = { .extra_bits = extra_lbits }  };

static tree_desc2 descs2 [2] = {
  { .desc = { .extra_bits = extra_lbits } },
  { .desc = { .extra_bits = extra_lbits } }
};

void build_tree(tree_desc *desc) {
  int *extra;
  extra = desc->extra_bits;
  /*@assert \valid(extra); */
}

const char * strings [2][2] = {
  { "the", "tha" },
  { "thi", "tho" }
};

int main(int argc, const char **argv) {
    build_tree(&l_desc);
    build_tree(&descs[0]);
    build_tree(&descs[1]);
    build_tree(&l_desc2.desc);
    build_tree(&descs2[0].desc);
    build_tree(&descs2[1].desc);

    char **p =  (char**)&strings[0];

    /*@assert \valid_read(p); */
    /*@assert \valid_read(*p); */

    return 0;
}



