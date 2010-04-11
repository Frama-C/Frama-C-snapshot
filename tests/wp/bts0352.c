int G;

/*@ behavior default:
      ensures \separated(\result, &G);
*/
int *ptrX(int *p )
{
  p = & G;
  return (p);
}

int main (void) {return 0;}
