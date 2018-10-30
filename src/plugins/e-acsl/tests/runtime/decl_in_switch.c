/* run.config
   DONTRUN:
   COMMENT: Variables declared in the body of switch statements so far cannot
   COMMENT: be tracked
*/

int main(int argc, char **argv) {
  switch(argc) {
    int *p;
    default: {
      p = &argc;
      /*! assert \valid(p); */
      break;
    }
  }
  return 0;
}
