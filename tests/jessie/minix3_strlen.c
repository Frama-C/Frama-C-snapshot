/*
@PPC_OPTIONS: -cpp-extra-args '-I minix3_include'
 */
/*
 * (c) copyright 1987 by the Vrije Universiteit, Amsterdam, The Netherlands.
 * See the copyright notice in the ACK home directory, in the file "Copyright".
 */
/* $Header: /ppc/ppc/ppc/tests/jessie/minix3_strlen.c,v 1.3 2008/11/10 13:33:45 uid570 Exp $ */

#include	<string.h>

size_t
strlen(const char *FRAMA_C_STRING org)
{
	register const char *s = org;

	while (*s++)
		/* EMPTY */ ;

	return --s - org;
}

/* 
Local Variables:
compile-command: "./run_test.sh minix3_strlen.c"
End:
*/
