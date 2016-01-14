#define STR "to/*to"
#define FOO "to\
to"
#define BAR 'to\
toblublihyu'

/*@ predicate p(char * x) = x[0] == 't'; */

int main(void)
{
        const char c[] = STR;
        const char d[] = FOO;
        const char e = BAR;
        /*@ assert p(STR) && p(FOO); */
        /*@ assert (char)BAR == 'u'; */
        return c[sizeof(c)-1];
}
