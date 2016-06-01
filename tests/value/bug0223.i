/* run.config*
STDOPT: #"-unspecified-access"
STDOPT: #"-no-unspecified-access"
*/

// No warning should be raised: we can syntactically ensure that
// the order of evaluation of expressions does not matter here.

extern int F(int, int);

extern int my_strcnmp(const char * const s1, const char * const s2, int
n);
extern char *ch1, *ch2;
void h2(void) {
    int test;
    test = (my_strcnmp(&ch1[3],&ch2[3],12) == 0) ;
}

extern unsigned char get(unsigned int);

extern void set(unsigned int *);

void ptr_deref(unsigned int * const ui) {
    unsigned int s=0;
    set(&s);
    *ui+=get(s) ;
}

void main()
{
    int i=0, j=0, k=0, l;
    int *p = &j;
    l = (F(i,j) == k);
    *p = (F(*p,j) == k);
    h2();
}
