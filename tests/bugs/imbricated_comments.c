#define MAX_NAME_LENGTH 30
	typedef char NAME_TYPE[MAX_NAME_LENGTH];

	extern void F (const NAME_TYPE /* in */ name);

	void G (void)
	{
	    F("toto") ;
	}
