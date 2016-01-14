void funk(int rounds)
{
  int k[2*rounds]; 
  int kk[(unsigned) (2*rounds)];
  int i;
  long long j = rounds * rounds;
  int k_long_long_size[j*2];
  int k_positive_size[4-2];
  //  int k_negative_size[2-4]; -> [kernel] user error: Length of array is negative

  for (i = 0; i < 2*rounds; i++)
    {
      k[i] = i;
    }
}

int main ()
{

  funk(17);
  return 0;
}
