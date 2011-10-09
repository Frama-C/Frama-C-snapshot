/*@ requires \valid(b);
  @ requires \valid(c);
  @ requires \valid(&a);
  @ assigns *b;
  @ assigns *c;
  @*/
void main(int a, int *b, int *c)
{
    int i=0;
    
    if (a==1)
    {
        *b=1;
        *c=1;
    }
    else if (a==-1)
    {
        *b=-1;
        *c=-1;
    }
    else
    {
        while (i<a)
        {
            *b=0;
            i++;
        }
        *c=0;
    }

}
