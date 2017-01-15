/* run.config
   COMMENT: Check warning for bad typedefs of standard integer types.
   OPT: -machdep x86_16
   OPT: -machdep ppc_32
   OPT: -machdep msvc_x86_64
 */

// These typedefs should cause warnings with every machdep supported by
// Frama-C.
typedef int int8_t;
typedef unsigned char uint_least64_t;
typedef short int_fast32_t;
typedef char intptr_t;
typedef unsigned short uintmax_t;
