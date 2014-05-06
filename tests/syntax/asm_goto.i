int main(unsigned short bit) 
{
  asm goto ("1: jmp %l[t_no]\n"
       /* skipping size check since replacement size = 0 */
	    : : "i" (bit) : : t_no);
  return 1;
 t_no:
  return 0;
}
