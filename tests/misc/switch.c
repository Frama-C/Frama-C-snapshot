int result1, result2;

int main (int c, int d) {

  switch (d)
    {
    case 1: 
      result1 = 1;
      break;
    case 2: 
      result1 = 2;
      break;
    case 3: 
      result1 = 3;
    case 4: 
      result1 = 4;
      break;
    }

  switch(c) 
    {
    case 0: CEA_F(c); return c;
    case 2: return c;
    }
  return 77;
}
