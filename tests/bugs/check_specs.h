// for SSIZE_MAX
//#include <limits.h>
#undef putchar
/* assigns \nothing;
 */
int putchar(int c);


#ifdef NO_FRAMA_C
/*@ assigns \nothing;
 */
void perror(const char *s);
#else
#define perror(s) 
#endif

/* To avoid issue with errno with is defined as "int *__errno_location (void);" 
   in system includes, we define our own errno.
 */
#undef errno
#define errno global_error_number
int global_error_number;

/*@ requires \valid(s);
    assigns \nothing;
 */
int puts(const char *s);

#ifdef NO_FRAMA_C
/*@ assigns \nothing;
 */
int fputs(const char *s, FILE *stream);
#else
#define fputs(...) 
#endif
