

/*@
assigns *p;
*/
extern int f(char * p); // fonction de mise à jour qui "écrit" dans *p

int main(char *x) {

  return f(x);
}
