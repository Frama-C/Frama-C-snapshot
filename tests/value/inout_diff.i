/* run.config*
   STDOPT: #"-ulevel 10" +"-inout"
*/

int t[50];
int u[50];
int x;

int main(int c){
  int i;
  for (i=0; i<50; i+=5)
    {
      t[i] = 1;
      t[i+1] = 1;
      u[i] = 1;
    }
  c = 7 * (c & 15);
  x = t[c];
  x += u[c];
  x += u[c+1];
  return 0;
}
