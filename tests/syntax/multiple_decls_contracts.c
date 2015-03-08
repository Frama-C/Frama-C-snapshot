/* run.config
OPT: share/libc/string.h @PTEST_FILE@ @PTEST_FILE@ -cpp-extra-args="-Ishare/libc" -check -print
OPT: @PTEST_FILE@ share/libc/string.h @PTEST_FILE@ -cpp-extra-args="-Ishare/libc" -check -print
OPT: @PTEST_FILE@ @PTEST_FILE@ share/libc/string.h -cpp-extra-args="-Ishare/libc" -check -print
*/

#include "string.h"
#include "stdlib.h"

char *
strdup(const char *str)
{
    if (str != NULL) {
        register char *copy = malloc(strlen(str) + 1);
        if (copy != NULL)
            return strcpy(copy, str);
    }
    return NULL;
}
