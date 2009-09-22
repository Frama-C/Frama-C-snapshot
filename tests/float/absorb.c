
float x = 1.0, y = 0.0, z;

void main() {
  z = y + 1e-286;
  while (y != x)
  {  
    y = x ; x+=1E-286;
  }
}
