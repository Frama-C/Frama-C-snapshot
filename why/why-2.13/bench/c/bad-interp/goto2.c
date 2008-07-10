
int f(int x) {
  if (x==0) { 
    goto l2; 
    l1 : x = 1;  
  } else {
    goto l1;
    l2 : x = 2;
  }
  return x;
}
