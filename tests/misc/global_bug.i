/* run.config
   STDOPT: +"-load-module lib/plugins/Report -then -report"
*/

int i = 1;
int G[2] = 
  {99<<63, 1};
int j = 2;

int main () {
  G[1] ++;
  return (i == j);
}
