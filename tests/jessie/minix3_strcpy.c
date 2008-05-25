//#pragma AnnotationPolicy(StrongPre)
//#pragma AbstractDomain(Pol)

/*
 * (c) copyright 1987 by the Vrije Universiteit, Amsterdam, The Netherlands.
 * See the copyright notice in the ACK home directory, in the file "Copyright".
 */
/* $Header: /ppc/ppc/ppc/tests/jessie/minix3_strcpy.c,v 1.6 2008/04/14 12:15:23 uid570 Exp $ */

#include	<minix3_include/string.h>

char *
strcpy(char *ret, register const char *FRAMA_C_STRING s2)
{
	register char *s1 = ret;

	while (*s1++ = *s2++)
		/* EMPTY */ ;

	return ret;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make minix3_strcpy"
End:
*/
