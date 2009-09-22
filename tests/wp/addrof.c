int A;

/*@ ensures A == 5 ; */
int main() {
  int *p = &A;
  *p = 5; 
  return *p;
}
