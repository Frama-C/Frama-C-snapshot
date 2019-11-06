/* run.config
   COMMENT: Compound initializers
*/



int _F;

char *_A[2] = { "XX", "YY" };
char *_B = "ZZ";
char *_C;
int _D[] = { 44, 88 };
int _E = 44;
int _F = 9;;

struct ST {
    char *str;
    int num;
};

struct ST _G[] = {
    {
        .str = "First",
        .num = 99
    },
    {
        .str = "Second",
        .num = 147
    }
};

int main(int argc, char **argv) {
    /*@ assert \valid(&_A[0]); */
    /*@ assert \valid_read(_A[0]); */
    /*@ assert \valid_read(_A[1]); */
    /*@ assert \valid_read(_B); */
    /*@ assert \valid(&_C); */
    /*@ assert \valid(&_D[0]); */
    /*@ assert \valid(&_E); */
    /*@ assert \valid(&_F); */
    /*@ assert _E == 44; */
    /*@ assert \valid(&_G); */
    /*@ assert _G[0].num == 99; */
    return 0;
}
