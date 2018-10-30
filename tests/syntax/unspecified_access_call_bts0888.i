/* run.config
STDOPT: +"-unspecified-access"
 */
int f(int);

int main(){
  int x,y,z;
  x = f(x=2); // not unspecified
  y = (x=f(y=2))+(z=3); // not unspecified
  z = (x=f(y=2))+y; // unspecified
  y = (x=f(y=2))+(y=3); // unspecified
  return 0;
}
