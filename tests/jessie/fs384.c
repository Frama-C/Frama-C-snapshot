extern const char *_sys_errlist[];

char *
strerror(register int errnum)
{
	return (char *)_sys_errlist[errnum];
}

/*
Local Variables:
compile-command: "LC_ALL=C make fs384"
End:
*/
