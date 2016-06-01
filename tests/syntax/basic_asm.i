int main(unsigned short bit) 
{
  // Extended asm, should have its assigns clause generated
  asm ("1: jmp %l[t_no]\n":);
  // Basic asm, should not have any assigns clause generated
  asm ("2: nop\n");
  return 1;
 t_no:
  return 0;
}
