int G;

int i_auSetEepromProgActive() {
  return G?0:(-51);
}

int main (void)
{
  int inRet = (0);
  int r = i_auSetEepromProgActive() ;
  if (r != (0))
    {
      inRet = (-51);

    }
  return inRet;

}
