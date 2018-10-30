#define N 10
int a[N], b[N];

int main() {
  //@ loop unroll N;
  for (int i = 0; i < N; i++) {
    //@ loop unroll 1;
    for (int j = 0; j < N; j++) {
      a[i] = 42;
    }
  }

  //@ loop unroll 1;
  for (int i = 0; i < N; i++) {
    //@ loop unroll N;
    for (int j = 0; j < N; j++) {
      b[j] = 42;
    }
  }

  return 0;
}
