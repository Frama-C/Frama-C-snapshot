/* run.config
   EXECNOW: make tests/aorai/Aorai_test.cmxs
   OPT: -aorai-ltl tests/aorai/test_factorial.ltl -aorai-test 1 -load-module tests/aorai/Aorai_test.cmxs -aorai-test-number @PTEST_NUMBER@
*/


/*


make -f test_factorial_annot.makefile goals



frama-c -jessie-analysis tests/aorai/test_factorial.c -jessie-int-model exact -jessie-gui 
frama-c tests/aorai/test_factorial.c  -ltl tests/aorai/test_factorial.ltl -ltl-dot 
frama-c -jessie-analysis tests/aorai/test_factorial_annot.c -jessie-int-model exact -jessie-gui 
 */




/* @ requires \string_len(s) >= 0;
 * @ ensures \result >= -1;
 */
int decode_int(char *s) {
    int intmax = ~ (1 << (sizeof(int) * 8 - 1));
    int cutlim  = intmax % 10;
    int cutoff  = intmax / 10;
    int value   = 0;
    char c;

    /* Decode number */
    while (c = *s++) {
	int v = 0;
	switch(c) {
	case '0': v = 0; break;	case '1': v = 1; break;
	case '2': v = 2; break;	case '3': v = 3; break;
	case '4': v = 4; break;	case '5': v = 5; break;
	case '6': v = 6; break;	case '7': v = 7; break;
	case '8': v = 8; break;	case '9': v = 9; break;
	default: return -1;
	}
	if ((value > cutoff) || (value == cutoff && v > cutlim))
	    return -1;
	value = value * 10 + v;
    }

    return value;
}



/*@ requires  0 <= value <= 12;
  @ decreases value;
  @ ensures   \result >= 1;
 */
int factorial(int value) {
    return value > 0 ? factorial(value - 1) * value : 1;
}

/*@ requires argc==2;
  @ ensures \result == 0 || \result == 1;
 */
int main(int argc, char** argv) {
    int value, err;

    if ((argc != 2)                         || /* bad argument number */
	((value = decode_int(argv[1])) < 0) || /* decoding error      */
	(value > 12))                          /* not in range        */
	return 1;

    factorial(value);
    //printf("%d! = %d\n", value, factorial(value));
    return 0;
}
