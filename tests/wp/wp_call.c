int G;


/*@ 
  requires \true;
  ensures G == \old(G) + x ;*/
void add_G(int x) {
  G += x ;
  return;
}

/*@ 
  requires G == 0;
  ensures G == 5 ;*/
void main(void) {

  add_G(5);
  
  return;
}
