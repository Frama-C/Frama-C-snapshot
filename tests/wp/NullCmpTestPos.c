

int *p; 
/*@ requires \valid (p);
 ensures \result == 0;
*/
int main(void)
{
  
  return (p == (char *) 0);
}
