/* run.config
EXECNOW: make -s tests/syntax/Enum_repr.cmxs
OPT: -load-module tests/syntax/Enum_repr.cmxs -enums int -print -check
OPT: -load-module tests/syntax/Enum_repr.cmxs -enums gcc-short-enums -print -check
OPT: -load-module tests/syntax/Enum_repr.cmxs -enums gcc-enums -print -check
*/

// is represented by | int | gcc-enums          | gcc-short-enums 
// foo               | int | unsigned int       | unsigned char
// bar               | int | unsigned char      | unsigned char
// bu1               | int | unsigned int       | unsigned int
// bu2               | int | unsigned int       | unsigned int
// bu3               | int | unsigned long long | unsigned long long
// bs1               | int | unsigned int       | int
// bs2               | int!| long long          | long long
// bs3               | int!| long long          | long long
// bc1               | int | unsigned int       | unsigned char
// bc2               | int | unsigned int       | unsigned char
// bd1               | int | int                | signed char
// bd2               | int | int                | signed char

typedef enum { A = 3 } foo;
typedef enum __attribute__((packed)) { B = 6 } bar;
typedef enum { Bu1 = 0x7FFFFFFF } bu1;
typedef enum { Bu2 = 0xFFFFFFFF } bu2;
typedef enum { Bu3 =0x1FFFFFFFF } bu3;
typedef enum { Bs1 = 0x7FFFFFFF, Ms1=-1 } bs1;
typedef enum { Bs2 = 0xFFFFFFFF, Ms2=-1 } bs2;
typedef enum { Bs3 =0x1FFFFFFFF, Ms3=-1 } bs3;

typedef enum { Bc1 =(signed char)'c'   } bc1;
typedef enum { Bc2 =(unsigned char)'c' } bc2;
typedef enum { Bd1 =(signed char)'c',   Md1=-1 } bd1;
typedef enum { Bd2 =(unsigned char)'c', Md2=-1 } bd2;

int main () {
  foo x = A;
  bar y = B;
  bu1 u1 = Bu1;
  bu2 u2 = Bu2;
  bu3 u3 = Bu3;
  bs1 s1 = Bs1;
  bs2 s2 = Bs2;
  bs3 s3 = Bs3;
  bc1 c1 = Bc1;
  bc2 c2 = Bc2;
  bd1 d1 = Bd1;
  bd2 d2 = Bd2;
  if (x==A && y == B) { return 0; } return 1;
}

typedef unsigned int bla;

int f1(bla x);
int f2(bla x);
int f3(bla x);

int h1(foo x);
int h2(foo x);
int h3(foo x);

int g() {
  foo x = A;
  int res = f1((bla) x);
  res+= f2((unsigned int) x);
  res+= f3(x);
  res+= h1((bla) x);
  res+= h2((unsigned int) x);
  res+= h3(x);
  return res;
}
