/* run.config
   COMMENT: bts #1478 about wrong detection of initializers in pre-analysis
*/

int global_i;
int* global_i_ptr = &global_i;
int global_i = 0;

/*@ requires global_i == 0;
    requires \valid(global_i_ptr);
    requires global_i_ptr == &global_i; */
void loop(void) { }

int main(void) {
  loop();
  return 0;
}
