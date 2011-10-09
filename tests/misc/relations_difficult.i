int x,y,*p;
int A,B,C,Z;

int main(int c)
{
  x = 0;
  y = 1;
  p = c ? &x : &y;
  *p = 2;
  x = 3;
  A = *p;  /* optimal : {2,3} ; sans relations : {1,2,3} */
  x = 4;
  B = (*p) + Z;  /* optimal : {2,4} ; sans relations : {1,2,4}; 
	      avec relations actuelles : {2,3,4} */
  C = *p; /* meme chose avec copy-paste */
  return 0;
}
