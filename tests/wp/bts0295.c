
int i=0;

/*@ requires i==0;
 */
int f(void);

int main() 
{
  //@ assert  i==0;
  return f();
}

int X = 3;
int Y = X+1;

//@ ensures \result == 4;
int other_main (void) {
  return Y;
}

