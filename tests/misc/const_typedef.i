/* run.config
  OPT: -print -then -val -lib-entry -no-print
*/

typedef int INT[3][3];
typedef int INT2[][3];
typedef int INT3[2][];

const __attribute__((BLA)) INT x1 = {1, 2, 3, 4, 5, 6, 7};
const __attribute__((BLA)) INT x1bis;
const __attribute__((BLA)) INT2 x2 = {1, 2, 3, 4, 5, 6, 7 };
const __attribute__((BLA)) INT3 x3 = {1, 2, 3, 4, 5, 6, 7};

typedef struct {
  int s1;
  int s2;
} ts;

const __attribute__((BLA)) ts s[3] ={ 1, 2, 3, 4, 5};

typedef int INT4[7];
typedef int INT5[];

const INT4 y1 = {0, 1, 2, 3, 4};
const INT5 y2 = {1, 2, 3, 4, 5};

extern const INT4 y3;
const int y3[7] = {1, 2};

int main() {
}
