/*$************* SCADE_KCG KCG Version 5.1.1 (build i10) **************
** Command :
** l2C        CruiseControl.lus -node CruiseControl
**     -noexp @ALL@
**     -keep_named_var
**     -const
**     -bitwise
**     -loc_ctx
**     -no_copy_mem
**     -debug
** date of generation (MM/DD/YYYY): 07/06/2007 13:30:09
** last modification date for CruiseControl.lus (MM/DD/YYYY): 07/06/2007 
********************************************************************$*/

#define _copy_mem(t,d,s) Copy_mem(t,d,s)
extern int _copy_mem(int, void *, const void *);

#define _comp_mem(t,x,y) Comp_mem(t,x,y)
extern int _comp_mem(int, const void *, const void *);

extern int printf(const char *, ...);
#define __assert(x) printf ("Violation of the assertion %s\n", x); return (false)


/*$************* SCADE_KCG KCG Version 5.1.1 (build i10) **************
** End of file definitions.h
** End of generation (MM/DD/YYYY) : 07/06/2007 13:30:09
********************************************************************$*/
