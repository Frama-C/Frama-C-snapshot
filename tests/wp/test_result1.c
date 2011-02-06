int * r;

/*@ ensures *r == 1; */
int g2() { 
   r = (int*)malloc(sizeof(int));
  return *r; 
}
