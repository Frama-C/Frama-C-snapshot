int case3(int arg) {
  switch(arg) {
    // Illegal according to 6.8.4.2§2
    int b[arg];
    case 1: {
      int *p;
      p = &b;
    }
    case 2: {
      int a[arg];
      if (arg)
      break;
    }
  }
}
