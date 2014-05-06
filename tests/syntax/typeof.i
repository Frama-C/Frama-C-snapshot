int f(int x) {
  return x;
}

typeof(f) g;

int A;
int T[5][6];
typeof(T[0][6]++) U;
typeof(&T[111]) V;
typeof(A++ + 1) V1;
typeof("FOO") S;

void h(int x) {
  g(3);
}


