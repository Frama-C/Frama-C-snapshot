// NOT defined as a constant. This file tests complicated widenings in presence
// of non-obvious bounds
int N = 24;

extern int A[24], B[26], C[24];

void main1() {
  int i, j;

  for (j=0; j<N-1; j++) {
    Frama_C_show_each_out(j);
    for (i=j+1; i<N; i++) {
      Frama_C_show_each_in(j,i);
      if (A[i] > A[j]) {
        // Swap	
      }
    }
  }
}

// Same as maiN1, except that the array is a bit larger. Ideally, we would
// infer the tight bound, not the size of B
void main2() {
  int i, j;

  for (j=0; j<N-1; j++) {
    Frama_C_show_each_out(j);
    for (i=j+1; i<N; i++) {
      Frama_C_show_each_in(j,i);
      if (B[i] > B[j]) {
        // Swap	
      }
    }
  }
}

// Same again, except that we access the array through an indirection. Our
// Syntactic heuristic does not work here, we should infer something directly
// from N
void main3() {
  int i, j;

  int *p = C;

  for (j=0; j<N-1; j++) {
    Frama_C_show_each_out(j);
    for (i=j+1; i<N; i++) {
      Frama_C_show_each_in(j,i);
      if (p[i] > p[j]) {
        // Swap	
      }
    }
  }
}

int t[20];
int u[40];

void main4() {
  int i, j;

  int maxi = 19;
  for (i = 0; i < maxi; i++) {
    t[i+1] = i;
  }
  t[i-maxi] = -1; // Optimal, inferred from t[i+1]

  int maxj = 40+3;
  for (j = 3; j < maxj; j++) {
    u[j-3] = j;
  }
  Frama_C_show_each(j); // Optimal, inferred from u[j-3]

  maxj = 35;
  for (j = 4; j < maxj; j++) {
    u[j-4] = j+1;
  }
  Frama_C_show_each(j); // Not optimal, inferred from u[j-4]. Could be improved
                        // by considering j < maxj instead
}


void main() {
  main1();
  main2();
  main3();
  main4();
}
