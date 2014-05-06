/* run.config
STDOPT: +"-kernel-msg-key printer:unspecified"
*/
int t[10];
int u[10];
int v[10];

int i = 4;
int j = 0;

void main() {
  int *p1 = &v[i];
  int *p2 = &v[j];

  t[i] += (*p1)++ + *p2;
  t[i] += v[i]++ + v[j];
}
