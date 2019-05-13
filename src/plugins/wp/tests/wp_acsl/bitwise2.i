
//@ ensures \result == ((unsigned)(A & (0x7FFFFE00 >> 9))) ;
unsigned job1(unsigned A) {
  return A & 0x7FFFFE00 >> 9 ;
}

//@ ensures \result == (A & 0x7FFFFE00 >> 9) ;
unsigned job1bis(unsigned A) {
  return A & 0x7FFFFE00 >> 9 ;
}

//@ ensures \result == (((unsigned)(A & 0x7FFFFE00)) >> 9) ;
unsigned job2(unsigned A) {
  return (A & 0x7FFFFE00) >> 9 ;
}

//@ ensures \result == ((unsigned)(A & (0x7FFFFE00 >> 9))) ;
unsigned job3(unsigned A) {
  return A & (0x7FFFFE00 >> 9) ;
}

//@ ensures \result == (A & (unsigned long)(0x7FFFFE00ul >> 9)) ;
unsigned job4(unsigned A) {
  return A & (0x7FFFFE00 >> 9) ;
}
