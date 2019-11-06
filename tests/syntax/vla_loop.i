void awhile(int x) {
  int *p;
  while(1) {
    int a1[x];
    p = &a1;
    if (x)
      break;
    else continue;
  }
}

int f(int i) {
  if (i > 1) return 1;
  int vla[i];
  return 0;
}

int g(int c) {
  int ret;
  if (c<=0) return 4;
  int a[c];
  a[c-1] = 3;
  ret = a[c-1];
  return ret;
}

int main(int argc, char **argv) {
  int *p;
  switch(argc) {
    default: {
      int a[argc];
      {
        while(1) {
          p = &a;
          break;
        }
      }
    }
  }
  {
    int b[argc];
    while(1) {
      p = &b;
      break;
    }
  }
  { int c[argc];
    { switch(argc) {
        case 1: { p = &c; break; }
      }
    }
  }
  { int d[argc];
    { switch(argc) {
        case 1: { p = &d; }
      }
    }
  }
  return argc;
}
