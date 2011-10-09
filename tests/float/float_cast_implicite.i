int C0 = 0, C2 = 2, CBP = 2000000000;
float fic0, fic1, fic2, fic4, fec0, fec2, fec4, ficbp, ficbn, fecbp, fecbn;

void main(void)
{
  fic0 = C0;
  fic1 = 1;
  fic2 = C2;
  fic4 = C2 + C2;
  fec0 = (float) C0;
  fec2 = (float) C2;
  fec4 = (float) (C2 + C2);

  ficbp = CBP;
  fecbp = (float) CBP;
  ficbn = -CBP;
  fecbn = (float) (-CBP);
}
