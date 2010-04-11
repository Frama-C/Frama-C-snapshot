int G; 
int X;



/*@ behavior default:
      ensures \separated(\result, &X);
*/
int *ptrX(int *p )
{
  p = & G;
  return (p);
}

int main (void) {return 0;}
