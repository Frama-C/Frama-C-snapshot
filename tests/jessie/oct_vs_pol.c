
int main (void)
{
  int i = 0, j, k = 0;
  while ((0 <= j) && (j <= 9))
  {
    i = i * 10 + j;
    k++;
  }
  return 0;
}


/* 
Local Variables:
compile-command: "LC_ALL=C make oct_vs_pol"
End:
*/
