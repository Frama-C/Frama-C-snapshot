int x,y,z;

int main(int c){
  x = 1;
  Frama_C_watch_value(&c, 
		      sizeof(c),
		      2000000000, 5);
  x = 2;
  c = !!c;
  x = 3;
  c = u();
  x = 4;
  x++;
  x++;
  x++;
  x++;
  x++;
  x++;
}
