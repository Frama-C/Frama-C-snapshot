/* run.config
   COMMENT: bts #2191, issue with unrolling types of struct members
*/

struct ST {
    char *str;
    int num;
};

struct ST _G[] = {
    {
        .str = "Struct_G[0]",
        .num = 99
    },
    {
        .str = "Struct_G[1]",
        .num = 147
    }
};

int main(int argc, char **argv) {
    /*@ assert \valid_read(_G[0].str); */
    return 0;
}
