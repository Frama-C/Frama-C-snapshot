/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input  -journal-disable
   OPT: -memory-footprint 1 -val -deps -out -input -ulevel 15 -journal-disable
*/
int main(int argc, char *argv[])
{
  int test_value;
  int loop_counter;
  char buf[10];

  test_value = 17;

  loop_counter = 0;
  while(++loop_counter)
  {
    /*  BAD  */
    buf[loop_counter] = 'A';
    if (loop_counter >= test_value) break;
  }


  return 0;
}
