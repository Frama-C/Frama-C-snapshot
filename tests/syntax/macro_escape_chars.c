struct file;

typedef struct file FILE;

extern FILE* fopen(const char*,const char*);
extern int fclose(FILE*);
extern int putc(int, FILE*);
extern int fputs(const char*, FILE*);

#define OUTPUT(F) putc('\\', F);
#define OUTPUTS(F) fputs("\\", F);

#define FOO '\\'

#define BAR "\\"

/*@ predicate foo(char* s) = \true; */

/*@ lemma test1: foo(BAR); */

/*@ lemma test2: FOO == '\\'; */

int main (int argc, char **argv) {
    FILE *f = fopen("/tmp/testfile.out","w");
    OUTPUT(f);
    OUTPUTS(f);
    fclose(f);
}
