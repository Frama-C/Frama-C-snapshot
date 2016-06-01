/* run.config*
STDOPT: +"-print"
*/

extern int v;

char str1[] = "absd";
char str2[] = "abdd";

struct s { int x; };
struct s s1;
struct s s2[8];

void f(void);
void g(void);

#define NULL (void*)0

void main () {
  int i;
  void (*p)(void) = (v ? &f : &g);
  // Valid
  i = (&str1 == &str2);
  i = (&s1 == NULL);
  i = (&s1+1 == NULL);
  i = (&s2[2] == &s2[4]);
  i = (&s2[8] == NULL);
  i = (&f == NULL);
  i = (&s1 == &s2);
  i = (&f == &g);
  i = (p == NULL);

  // Valid
  i = (&s2[2] < &s2[4]);

  // Invalid
  i = (&s2[9] == NULL);
  i = (&s2[9] == &s2[9]);

  // Invalid
  i = (&str1 < &str2);
  i = (&s1 < &s2);
  i = (&f < &g);

  // ?
  i = (&s1 > NULL);
  i = (&s1+1 > NULL);
  i = (&s2[8] > NULL);
  i = (&f == NULL); // Cabs2cil typing bug here; tolerated for now
  i = (&f > NULL); // Same
  i = (p > NULL);

}
