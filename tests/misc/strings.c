/* run.config
  GCC:
  OPT: -memory-footprint 1 -val -deps -out -input  -main main1
  OPT: -memory-footprint 1 -val -deps -out -input  -main main2
  OPT: -memory-footprint 1 -val -deps -out -input  -main main3
  OPT: -memory-footprint 1 -val -deps -out -input  -main main4
  OPT: -memory-footprint 1 -val -deps -out -input  -main main5
  OPT: -memory-footprint 1 -val -deps -out -input  -main main6
  OPT: -memory-footprint 1 -val -deps -out -input  -main main7
*/

char s1[]="hello, world";
char s2[]="hello";

void main1(void)
{
  char x;
  char *p;
  p = &s1[3];
  x=*(p-4);
}
/* tests/misc/strings.c:10: Warning: out of bounds access.
 */

void main2(void)
{
  char x;
  char *p;
  p = &s1[3];
  x=*(p+12);
}
/* tests/misc/strings.c:20: Warning: out of bounds access.
 */

void main3(int c)
{
  char x;
  char *p;
  if (c)
    p = &s1[5];
  else
    p = &s2[3];
  x=*(p-4);
}
/* tests/misc/strings.c:33: Warning: out of bounds access.
Values for function main3:
  p -> {{ &s2 + {3; } ; &s1 + {5; } ;}}
  x -> {101; }

Les valeurs invalides dans le déréférencement ligne 33
sont oubliées en même temps que l'avertissement est affiché.
L'analyse continue avec celles des valeurs qui sont valides
(d'où x=101 à la fin de la fonction).
Il serait possible de réduire aussi les valeurs de p
sachant que p-4 doit être valide.
*/

char *strcpy(char*dst, char*src)
{
  char* ldst=dst;
  /*@ loop pragma UNROLL_LOOP 50; */
  while (*ldst++ = *src++)
    ;
  return dst;
}

void main4(void)
{
  char a[10] = "Not ok";
  char b[5];

  strcpy(b,a);
}

/*
tests/misc/strings.c:52: Warning: out of bounds access.

*/

unsigned int strlen(char *s)
{
  unsigned int l=0;
 /*@ loop pragma UNROLL_LOOP 50; */
  while(*s++ != 0)
    l++;
  return l;
}

int main5(char c)
{
  char a[15] = "A long string";
  a[3]=c;
  a[6]=c;
  return strlen(a);
}
/*
Values for function main5:
  __retres -> {3; 6; 13; }
*/

int main6(void)
{
  char *s;
  char c;
  s = "toto";
  c = *s;
  return c;
}

char *s3="tutu";
char *s4="tutu";

int main7(void)
{
  char c;
  *s3=' ';
  c=*s4;
  return c;
}
