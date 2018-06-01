
typedef unsigned long size_t;

volatile size_t sv;

/*@ requires len: len > 0 && len <= 1024;
    requires n: n < 64; */
void bts2166(size_t len, size_t n)
{
  if (len > 0 && len <= 1024 && n < 64) { // logic reductions do not work in the Apron domain. We use this 'if' for now
    if (len >= 64 || len + n >= 64)
      {
        n = 64 - n;
        len -= n;
        Frama_C_dump_each();
      }
    Frama_C_show_each_end(len, n);
  }
}

extern int a[514];

//@ assigns \result \from i1, i2; ensures i1 <= \result <= i2;
unsigned int unsigned_interval(int i1, int i2);

int main2() {
  unsigned int i, t, n;
  int s = 0;
  i = unsigned_interval(0,512);
  t = unsigned_interval(0,512);
  n = unsigned_interval(0,512);
  for (i = n; i >= t+1; i--) {
    Frama_C_dump_each();
    int b3 = a[i-(t+1)] == 3;
    s += b3;
  }
  return s;
}

int T[7] = {0, 1, 42, 5, 6, 41, 42};

void main3 (int i) {

  if (i >= 0 && i < 2) {
    T[T[i]] = 2; // may trick symbolic domains: T[T[i]] == 2 may not hold afterwards, because the instruction writes on the locations involved in T[T[i]]
    if (T[T[i]] == 2)
      Frama_C_show_each_NO1();
    else
      Frama_C_show_each_OK1();
  }

  if (i >= 3 && i < 5) {
    T[T[i]] = 17;
    if (T[T[i]] == 17) // here the equality holds 
      Frama_C_show_each_OK2();
    else
      Frama_C_show_each_NO2();
  }

}

void main() {
  bts2166(sv, sv);
  main2();
  main3(sv);
}


