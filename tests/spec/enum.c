typedef enum{
  VRAI=1,
  FALSE=0
}T_BOOLEEN;

/*@logic T_BOOLEEN test (integer b)=
  @
  @   ((b==1)?
  @   (T_BOOLEEN)VRAI
  @   : (T_BOOLEEN)FALSE);
  @*/

/*@ensures \result == test(boo);
  @*/

T_BOOLEEN test(int boo)
{
  T_BOOLEEN b;

  if (boo==1)
    b = VRAI;
  else
    b= FALSE;

  return b;
}
