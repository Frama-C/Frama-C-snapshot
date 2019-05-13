/* run.config*
   OPT: -eva @EVA_CONFIG@
*/

#include <stdlib.h>
#include <string.h>
#include <__fc_builtin.h>

int main(void)
{
    char *p;
    p = malloc(Frama_C_interval(4, 7));
    memcpy(p, "foo", 4);
    memcpy(p + 4, "x", 2); // must produce alarm
    memcpy(p, p + 5, 1); // \valid_read((p + 5) + (0 .. (1 - 1)))
    p[5] = 'x'; // \valid(p + 5)
    int r = p[5]; // \valid_read(p + 5)
    free(p);
    return 0;
}
