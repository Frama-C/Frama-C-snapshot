int t[10];

void Frama_C_assert(int cond);

int main(unsigned int c){
  Frama_C_assert(c < 10);
  return t[c];
}
