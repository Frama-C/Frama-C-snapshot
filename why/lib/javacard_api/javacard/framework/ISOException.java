/*
* $Workfile: ISOException.java $	$Revision: 1.2 $, $Date: 2007/09/26 15:15:36 $
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
// $Workfile: ISOException.java $
// $Revision: 1.2 $
// $Date: 2007/09/26 15:15:36 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/ISOException.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Zhiqun
// */

package javacard.framework;

/**
 * <code>ISOException</code> class encapsulates an ISO 7816-4 response status word as
 * its <code>reason</code> code.
 * <p>The <code>APDU</code> class throws JCRE owned instances of <code>ISOException</code>.
 * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
 * and can be accessed from any applet context. References to these temporary objects
 * cannot be stored in class variables or instance variables or array components.
 * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
 */

public class ISOException extends CardRuntimeException {

  // initialized when created by Dispatcher
    // if non static : 
    //   Modified by Xavier to bypass pb with static non final variables
    private /* @ spec_public @*/ static ISOException systemInstance;
  
    private /* @ spec_public @*/short[] theSw;

  // @ public model short _reason; 

  /**
   * Constructs an ISOException instance with the specified status word.
   * To conserve on resources use <code>throwIt()</code>
   * to use the JCRE owned instance of this class.
   * @param sw the ISO 7816-4 defined status word
   */
  public ISOException(short sw){
    super(sw);
    if (systemInstance==null) // created by Dispatcher
        systemInstance = this;
    theSw = JCSystem.makeTransientShortArray( (short)1, (byte)JCSystem.CLEAR_ON_RESET );
    theSw[0] = sw; 
  }

  /**
   * Throws the JCRE owned instance of the ISOException class with the specified status word.
   * <p>JCRE owned instances of exception classes are temporary JCRE Entry Point Objects
   * and can be accessed from any applet context. References to these temporary objects
   * cannot be stored in class variables or instance variables or array components.
   * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.
   * @param sw ISO 7816-4 defined status word
   * @exception ISOException always.
   */
    
    /* @ public exceptional_behavior
      @   requires true;
      @   modifiable systemInstance.theSw[0];
      @   signals (ISOException e) e.getReason() == sw 
      @     && e == systemInstance && e != null; 
      @
      @*/
    // modified by Xavier, cf. systemInstance
    // suppressed from signals :  && e == systemInstance; 
    public static void throwIt(short sw){
	// begin
	//	ISOException tmp = new ISOException(sw);
	//throw tmp.systemInstance;
	// end
	// the following is the original body
	systemInstance.setReason(sw);
	throw systemInstance;
  }
  
  /** Get reason code
   *  @return the reason for the exception
   */

  /* @ public normal_behavior
    @    requires true;
    @  assignable \nothing;
    @     ensures \result == _reason ;
    @*/

  public /* @ pure @*/ short getReason() {
    return theSw[0];
  }

  /** Set reason code
   * @param reason the reason for the exception
   */

  /* @ public normal_behavior
    @    requires true;
    @  assignable _reason;
    @     ensures getReason() == sw;
    @*/
  public void setReason(short sw) {
    theSw[0] = sw;
  }
}






