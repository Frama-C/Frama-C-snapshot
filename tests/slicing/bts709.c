/* run.config
   OPT: -check -slice-pragma func -no-unicode -journal-disable -then-on 'Slicing export' -print
 */

int inputsOf_testcase_func ();
int inp1, var1,var2;

void func(  void  )  
{ 
	if ( 1 == inp1 )  
	{ 
		// Block-1
		var1 =  1 ;
		var2 =  1 ;
	} 
	else  
 	{ 
		if (  2== inp1  )  
		{
			// Block-2
			 var1 = 2 ;
			 var2 = 2 ;
		} 
		else  
 		{ 
			// Block-3
			if (  3== inp1  )  
			{ 
				var1 =  3;
				var2 =  3  ;
			} 
		} 
	} 

	//@slice pragma stmt;
	 65 != var2 ? assert ( 5 != var1):1;
} 


int main( ) { 


	int _noOfIter_ = 0;
	for (_noOfIter_=0; _noOfIter_ < 1; _noOfIter_++ ) {
		inputsOf_testcase_func ( );
		func ();
	}
}

int inputsOf_testcase_func ()
{
	int nondet_int ( );
	inp1 = nondet_int ( );
	var1 = nondet_int ( );		// This required line is getting knocked off
	var2 = nondet_int ( );		// This required line is getting knocked off
        return 0;
}
