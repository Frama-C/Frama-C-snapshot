/*
* $Workfile: Dispatcher.java $	$Revision: 1.1 $, $Date: 2007/09/26 14:32:59 $
*
* Copyright (c) 1999 Sun Microsystems, Inc. All Rights Reserved.
*
* This software is the confidential and proprietary information of Sun
* Microsystems, Inc. ("Confidential Information").  You shall not
* disclose such Confidential Information and shall use it only in
* accordance with the terms of the license agreement you entered into
* with Sun.
*
* SUN MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
* SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
* IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
* PURPOSE, OR NON-INFRINGEMENT. SUN SHALL NOT BE LIABLE FOR ANY DAMAGES
* SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
* THIS SOFTWARE OR ITS DERIVATIVES.
*/

// /*
// $Workfile: Dispatcher.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/Dispatcher.java $
// $Modtime: 5/02/00 8:48p $
// Original author:  Mitch Butler
// */
package javacard.framework;

import com.sun.javacard.impl.PrivAccess;
import com.sun.javacard.impl.AppTable;
import com.sun.javacard.impl.NativeMethods;
import javacard.security.CryptoException;

/**
 * This class is the Dispatcher, which contains the main entry point and
 * the main loop of the card. It dispatches APDUs to applets.
 */
class Dispatcher
{
	// Constants
    private static final byte INS_SELECT   = (byte)0xA4;
    private static final byte P1_SELECT_DFBYNAME = (byte) 0x04;

    private static final byte P2_SELECT_OPTIONS = (byte) 0xF3;
    private static final byte P2_SELECT_OPTIONS_ONLY = 0x00;

	// Static class fields, with initialization done in init()
	private static boolean initDone = false;
    private static APDU theAPDU;
    private static byte[] theAPDUBuffer;
    private static PrivAccess thePrivAccess;
    
    // other static fields
    static Dispatcher theDispatcher;

    /**
     * Main entry point invoked by VM when card is reset.
     */
	static void main()
	{
	    if (!initDone) cardInit();      // card initialization (first time only)	    
	    cardReset();                    // session initialization (each card reset)
	    short sw = 0;

        // main loop
    	while (true) {
            PrivAccess.resetSelectingAppletFlag();
    	    theAPDU.complete(sw); // respond to previous APDU and get next
    	    try {
    	        processDispatcherAPDU(); // Dispatcher handles the SELECT APDU
    	        // dispatch to the currently selected applet
    	        if (PrivAccess.getSelectedAppID()==PrivAccess.APP_NULL) {
                   // if no applet selected
                   ISOException.throwIt(ISO7816.SW_APPLET_SELECT_FAILED);
                   }
                PrivAccess.getSelectedApplet().process(theAPDU);
 	            // abort transaction if applet forgot to commit it
 	            if (JCSystem.getTransactionDepth() != 0) {
 	               TransactionException.throwIt(TransactionException.IN_PROGRESS);
 	               }
 	            sw = ISO7816.SW_NO_ERROR;
    	    } catch (ISOException ex) {
    	        // get SW from ISOException reason
    	        sw = ex.getReason();
    	    } catch (Throwable e) {
    	        // any other exception is unknown reason
    	        sw = ISO7816.SW_UNKNOWN;
    	    }

 	        // abort transaction if still in progress
 	        if (JCSystem.getTransactionDepth() != 0) {
	            JCSystem.abortTransaction();
	        }
   	    }
	}

    /**
     * Process APDUs handled by the Dispatcher.
     */
	private static void processDispatcherAPDU()
	{
		if (theAPDUBuffer[0] == (byte)(ISO7816.CLA_ISO7816)) {
    		switch (theAPDUBuffer[1]) {
    		    case (byte)INS_SELECT:
    		        theDispatcher.selectAPDU(theAPDU);
    		        break;
    		}
    	}
	}

    /**
     * Select commands are handled by the Dispatcher.
     * This method checks for the supported SELECT options. If it is a supported applet selection command,
     * and the applet is identified, deselect the currently selected applet and select the new one.
     * If applet selection fails, or the identified applet state is not selectable, deselect the
     * the currently selected applet and leave in "no applet selected" state.
     * This method rewinds the APDU if setIncomingAndReceive() is called.
     */
	void selectAPDU(APDU theAPDU)
	{
        // ensure that P1==4 and (P2 & 0xF3)==0
        if (theAPDUBuffer[2] == P1_SELECT_DFBYNAME) {

            // check P2 = 0x0, 0x04, 0x08 or 0x0C
            if ((theAPDUBuffer[3] & P2_SELECT_OPTIONS) != P2_SELECT_OPTIONS_ONLY) {
                return;
            }

            // read AID and check APDU data length
            byte len = (byte)(theAPDU.setIncomingAndReceive());

            if ( len == theAPDUBuffer[4]) {

                // search for applet with AID
                byte i = AppTable.findApplet(theAPDUBuffer, (short)5, len);

                // find AID in AppTable
                // if no match of AID found in AppTable. JCRE assumes that the command is
                // not being used to select an applet and will invoke process() method
                // of the currently select applet (if there is one)
                if ( i != AppTable.APP_NULL) {
                  // is applet selectable?
                  if ( PrivAccess.getAppState( thePrivAccess.getAID(i) ) >= PrivAccess.APP_STATE_SELECTABLE ){
                    PrivAccess.selectApplet(i);
                  } else {
                    // if applet is not selectable, no applet selected
                    PrivAccess.deselectOnly();
                    NativeMethods.setSelectedContext(PrivAccess.JCRE_CONTEXTID);
                  }
                }
            }
            // "undo" the receive so that the applet can do it
            undoReceive();
        }
    }
    
    void undoReceive() {
        // "undo" the receive so that the applet can do it
        theAPDU.undoIncomingAndReceive();
        }   
            

    /**
     * This method is executed once each time the card is reset, when
     * the Dispatcher is initialized. All reset-time-only Dispatcher,
     * PrivAccess, and "system" initializations are done here.
     */
	static void cardReset()
	{
	    // this implementation selects a default applet on card reset.
        PrivAccess.selectDefaultApplet();
        
        // ensure AppTable state is consistent
        AppTable.cleanup();
        // other card reset time initialization such as initializing
        // transient objects not shown here.
	}

    /**
     * This method is executed exactly once in the lifetime of the card, when
     * the Dispatcher is initialized for the first time. All first-time-only Dispatcher,
     * PrivAccess, and "system" initializations are done here.
     */
    static void cardInit()
    {
        NativeMethods.markHeap(); //allow the JCRE heap space recovery
        if (theDispatcher==null) theDispatcher = new Dispatcher();
        
        // create JCRE owned instances of exceptions
        // and designate as temporary Entry Point Objects
        // Exception classes init their own static systemInstance reference
        Exception ex;
        ex = new CardException( (short) 0 );
        NativeMethods.setJCREentry( ex, true );
        ex = new CardRuntimeException ( (short) 0 );
        NativeMethods.setJCREentry( ex, true );
        ex = new APDUException ( (short) 0 );
        NativeMethods.setJCREentry( ex, true );
        ex = new ISOException ( (short) 0 );
        NativeMethods.setJCREentry( ex, true );
        ex = new PINException ( (short) 0 );
        NativeMethods.setJCREentry( ex, true );
        ex = new SystemException ( (short) 0 );
        NativeMethods.setJCREentry( ex, true );
        ex = new TransactionException ( (short) 0 );
        NativeMethods.setJCREentry( ex, true );
        ex = new UserException ( (short) 0 );
        NativeMethods.setJCREentry( ex, true );
        ex = new CryptoException ( (short) 0 );
        NativeMethods.setJCREentry( ex, true );

        // create JCRE owned instance of the APDU and designate as temporary Entry Point Object
        theAPDU = new APDU();
        NativeMethods.setJCREentry( theAPDU, true );

        // create JCRE owned instance of PrivAccess and designate as permanent Entry Point Object
        thePrivAccess = JCSystem.thePrivAccess = new PrivAccess();
        NativeMethods.setJCREentry( thePrivAccess, false );

        // get APDU buffer and mark as (temporary) Global Array.
        theAPDUBuffer = theAPDU.getBuffer();
        NativeMethods.setJCREentry( theAPDUBuffer, true );

        PrivAccess.initialize( theAPDU );

        initDone = true;	        // card init is complete
        NativeMethods.commit();     // commit the heap space used by JCRE
    }

    /**
     * Only JCRE can use constructor.
     * No need to construct this class anyway. No instance methods/fields.
     */
    Dispatcher(){}

}
