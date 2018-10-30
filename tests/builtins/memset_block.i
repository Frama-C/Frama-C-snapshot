/* run.config*
STDOPT: #"-initialized-padding-locals"
STDOPT: #"-no-initialized-padding-locals"
*/

int main(void) {
const char S[5] = "12345";

struct t1 { int x; int y; int name[10];} v1;
struct t1 TS[29] = {1,3,01234570110};
struct t2 { int x2; short int y2; char *ptr;} v2;
char C;
char PC[]= "lkjlj";
struct t2 T2[50] = {{1,2,&PC[0]},{1,2,0}};

int T[10] = {1,0};
int U[] = {3,4};
int x = sizeof(U);
int y = sizeof(T);

return sizeof(U);
}
