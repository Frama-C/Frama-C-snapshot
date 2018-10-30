#include <stdlib.h>
#include <string.h>
#include "stdio.c"

int main() {
  FILE *stream;
  char *line = NULL;
  size_t len = 0;
  size_t total_len = 0;
  ssize_t read;
  stream = fopen("/etc/motd", "r");
  if (!stream) return 1;
  while ((read = getline(&line, &len, stream)) != -1) {
    //@ assert read_ok: line != \null;
    total_len += strlen(line);
    //@ assert read_bytes: strlen(line) == read;
    //@ assert allocated_enough: len >= strlen(line);
  }
  free(line);
  fclose(stream);
  return 0;
}
