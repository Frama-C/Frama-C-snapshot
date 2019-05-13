/* run.config*
   STDOPT: #"-eva-msg-key initial-state"
*/

// Cf. Bts 856

volatile int v;

void main(int a, int tb[10][a], int tc[a][10]) {
  *(int*)tb = 1;
  Frama_C_dump_each();
  tb[9][100] = (int) &tb;
  Frama_C_dump_each();
  tc[1][1] = 3;
  if (v)
    tc[1][16] = 0;
}
