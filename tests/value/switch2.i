/*run.config*
  STDOPT: #"-simplify-cfg"
 */

int f(int x) { return x+1; }

extern void g(int,int);

int main () {
  int exit_loop = 0;
    switch (16) {
    case 16:
      g(exit_loop++,({exit_loop++; exit_loop++;f(exit_loop);}));
        break;
    default:
      exit_loop = 1;
      break;
    }
  return 0;
}
