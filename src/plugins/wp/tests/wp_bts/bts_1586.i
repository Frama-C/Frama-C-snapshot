
/*@ behavior Bizarre:
      assumes x;
      ensures TRANS: x ==> \result==1 ;
 */
int compute_bizarre(int x)
{
  if (x)
    return 1;
  else
    return 2;
}

/*@ behavior Normal:
      assumes x;
      ensures TRANS: x <==> \result==1 ;
 */
int compute_normal(int x)
{
  if (x)
    return 1;
  else
    return 2;
}

int main_bizarre_KO(int x)
{
  int trans = compute_bizarre(x);

  switch(trans) {
  case 0:
    //@ assert FALSE: \false;
    return -1;
    break;
  default:
    return -1;
    break;
  }
}

int main_normal_KO(int x)
{
  int trans = compute_normal(x);

  switch(trans) {
  case 0:
    //@ assert FALSE: \false;
    return -1;
    break;
  default:
    return -1;
    break;
  }
}
