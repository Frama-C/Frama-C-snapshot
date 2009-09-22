int G;
int x,y;

int i_auSetEepromProgActive() {
  return G?0:(-51);
}

int main(int argc, char**argv)
{
  int r;
  int inRet = (0);
  char c = **argv;
  short s = argc;
  if(c < 0)
    x = c;
  if(s >= -10)
    y = s;
  r = i_auSetEepromProgActive() ;
  if (r != (0))
    {
      inRet = (-51);

    }
  return inRet;

}
