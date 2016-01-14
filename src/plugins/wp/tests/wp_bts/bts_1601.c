typedef unsigned char BOOL;
#define TRUE 1
#define FALSE 0

typedef unsigned char uint8;
typedef unsigned short int uint16;
typedef unsigned long  uint32;

uint16 F_MIN_R = 15;  

const uint8 RESP_STATE = 30; 


typedef enum
{
        RESP_MODE,
        SS_A_MODE
}tenumMode;

tenumMode tenumRMode;


BOOL gbCaMStatus;
BOOL gbCaaStatus;
uint8 mnPb;
BOOL mbApLYRange;
float gfApYLineSlope;
float gfApYLineConst;
float gfApRLineSlope;
float gfApRLineConst;
float mfAp;
uint16 almC;
uint16	nApLYL = 0;
uint16	nApLRL = 0;
uint16  Ap_Y_L_Ui = 0;
uint16  Ap_R_L_Ui = 0;
float	fCaValue=0.0;
float   fRrValue = 0.0;
uint16	nCaLYL=0;
uint16	nCaLRL=0;


/*@ 
  @ behavior basic:
  @  assumes fRrValue == 0;
  @  ensures tenumRMode == SS_A_MODE;
  @
*/

void foo()
{

	float	mfNewAp = 0;	
	BOOL	bYAp = FALSE;
	BOOL	bRAp = FALSE;
	BOOL	bApAlmC = FALSE;

	if (fRrValue != 0)
	{
            /* Some code here */ 
	}
	else
	{
		if (mnPb == 1)
		{
			mfAp = RESP_STATE; 
			mnPb = 2; 
		}
		
		tenumRMode = SS_A_MODE;
	}
	//@ assert fRrValue == 0.0 ==> tenumRMode == SS_A_MODE;

	if ( (mfAp >= F_MIN_R) &&
		 ((gbCaMStatus == TRUE) && (gbCaaStatus == FALSE)) )
	{
		bApAlmC = TRUE;
                almC = 1;
	}
        else
        {
                almC = 0;
        }
	//@ assert fRrValue == 0.0 ==> tenumRMode == SS_A_MODE;
 

	if ( (bApAlmC == TRUE)
		 && (mfAp < nApLYL)
		 && (fCaValue >= nCaLYL) )
	{
		float fmultval = 0;

		fmultval = gfApYLineSlope*fCaValue;

		mfNewAp = fmultval + gfApYLineConst;

		if (mfAp >= mfNewAp)
			bYAp = TRUE;
		else
			bYAp = FALSE;

		Ap_Y_L_Ui = (uint16)mfNewAp;
	}
	//@ assert fRrValue == 0.0 ==> tenumRMode == SS_A_MODE;


	if ((bApAlmC == TRUE) && (fCaValue > (float)nCaLYL))
	{
		mfNewAp = ((gfApYLineSlope*fCaValue) + gfApYLineConst);
		if (mfNewAp < (float)nApLYL);
                  Ap_Y_L_Ui = (uint16)mfNewAp;
	}

	else if ((bApAlmC == TRUE) && (fCaValue <= (float)nCaLYL))
		Ap_Y_L_Ui = F_MIN_R;	
  
	if ( (bApAlmC == TRUE) && (fCaValue >= nCaLRL) )
	{
		float fmultval = 0;

		fmultval = gfApRLineSlope*fCaValue;

		mfNewAp = fmultval + gfApRLineConst;

             
		if (mfAp >= mfNewAp)
			bRAp = TRUE;
		else
			bRAp = FALSE;

		Ap_R_L_Ui = (uint16)mfNewAp;
	}
        else if ( (bApAlmC == TRUE) && (fCaValue < nCaLRL) )
                Ap_R_L_Ui = F_MIN_R; 
	
	//@ assert fRrValue == 0.0 ==> tenumRMode == SS_A_MODE;
	if ( (mfAp >= nApLYL)
		 || ((bApAlmC == TRUE) && (fCaValue < nCaLYL))
		 || ((bYAp == TRUE)
			 && (gbCaMStatus == TRUE) && (gbCaaStatus == FALSE)  ) )
	{
		mbApLYRange = TRUE;
	}
	else
		mbApLYRange = FALSE;

	//@ assert fRrValue == 0.0 ==> tenumRMode == SS_A_MODE;
	if ( (mfAp >= nApLRL)
		 || ((bApAlmC == TRUE) && (fCaValue < nCaLRL))
		 || ((bRAp == TRUE)
			&& (gbCaMStatus == TRUE) && (gbCaaStatus == FALSE) )  )
	{
	  //@ assert fRrValue == 0.0 ==> tenumRMode == SS_A_MODE;

	}

	//@ assert fRrValue == 0.0 ==> tenumRMode == SS_A_MODE;
}

