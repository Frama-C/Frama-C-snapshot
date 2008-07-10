/*
* $Workfile: Applet.java $	$Revision: 1.2 $, $Date: 2008/01/31 18:27:25 $
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
// $Workfile: Applet.java $
// $Revision: 1.2 $
// $Date: 2008/01/31 18:27:25 $
// $Author: nrousset $
// $Archive: /Products/Europa/api21/javacard/framework/Applet.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

import javacard.framework.JCSystem;
import com.sun.javacard.impl.PrivAccess;
/**
 * This abstract class defines an applet in Java Card.
 * <p>
 * The <code>Applet</code> class should be extended by any applet that is intended to be
 * loaded onto, installed into and executed on a Java Card compliant
 * smart card.<p>
 *
 * <p>
 * Example usage of <code>Applet</code>
 * <pre><code>
 * public class MyApplet extends javacard.framework.Applet{
 * static byte someByteArray[];
 *
 * public static void install( byte[] bArray, short bOffset, byte bLength  ) throws ISOException {
 *   // make all my allocations here, so I do not run
 *   // out of memory later
 *   MyApplet theApplet = new MyApplet();
 *
 *   // check incoming parameter
 *   byte bLen = bArray[bOffset];
 *   if ( bLen!=0 ) { someByteArray = new byte[bLen]; theApplet.register(); return; }
 *   else ISOException.throwIt(ISO7816.SW_FUNC_NOT_SUPPORTED);
 *   }
 *
 * public boolean select(){
 *   // selection initialization
 *   someByteArray[17] = 42; // set selection state
 *   return true;
 *   }
 *
 * public void process(APDU apdu) throws ISOException{
 *  byte[] buffer = apdu.getBuffer();
 *  // .. process the incoming data and reply
 *  if ( buffer[ISO7816.OFFSET_CLA] == (byte)0 ) {
 *     switch ( buffer[ISO7816.OFFSET_INS] ) {
 *         case ISO.INS_SELECT:
 *             ...
 *             // send response data to select command
 *             short Le =  apdu.setOutgoing();
 *             // assume data containing response bytes in replyData[] array.
 *             if ( Le < ..) ISOException.throwIt( ISO7816.SW_WRONG_LENGTH);
 *             apdu.setOutgoingLength( (short)replyData.length );
 *             apdu.sendBytesLong(replyData, (short) 0, (short)replyData.length);
 *             break;
 *         case ...
 *         }
 *      }
 *   }
 *
 * }
 * </code></pre>
 *
 * @see SystemException
 * @see JCSystem
 */

abstract public class Applet
{
    private PrivAccess thePrivAccess;  
    
    /**
     * Only this class's <code>install()</code> method should create the applet object.
     */
    protected Applet(){
        thePrivAccess = PrivAccess.getPrivAccess();
    }

    /**
     * To create an instance of the <code>Applet</code> subclass, the JCRE
     * will call this static method first.
     * <p>The applet should
     * perform any necessary initializations and must call one of the <code>register()</code> methods.
     * Only one Applet instance can be successfully registered from within this install.
     * The installation is considered successful when the call to <code>register()</code>
     * completes without an exception. The installation is deemed unsuccessful if the
     * <code>install</code> method does not call a
     * <code>register()</code> method, or if an exception is thrown from within
     * the <code>install</code> method prior to the call to a <code>register()</code>
     * method, or if every call to the <code>register()</code> method results in an exception.
     * If the installation is unsuccessful, the JCRE must perform all the necessary clean up
     * when it receives control.
     * Successful installation makes the applet instance capable of being selected via a
     * SELECT APDU command.<p>
     * Installation parameters are supplied in the byte array parameter and
     * must be in a format defined by the applet. 
     * The <code>bArray</code> object is a global array. If the applet
     * desires to preserve any of this data, it should copy
     * the data into its own object.
     * <p><code>bArray</code> is zeroed by the JCRE after the return from the
     * <code>install()</code> method.<p>
     * References to the <code>bArray</code> object 
     * cannot be stored in class variables or instance variables or array components.
     * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.2 for details.<p>
     * The implementation of this method provided by
     * <code>Applet</code> class throws an <code>ISOException</code> with 
     * reason code = <code>ISO7816.SW_FUNC_NOT_SUPPORTED</code>.
     * <p>Note:<ul>
     * <li><em>Exceptions thrown by this method after successful installation are caught 
     * by the JCRE and processed by the Installer.</em>
     * </ul>
     * @param bArray the array containing installation parameters.
     * @param bOffset the starting offset in bArray.
     * @param bLength the length in bytes of the parameter data in bArray.
     * The maximum value of bLength is 32.
     */
    public static void install( byte[] bArray, short bOffset, byte bLength ) throws ISOException
    { ISOException.throwIt(ISO7816.SW_FUNC_NOT_SUPPORTED); }

    /**
     * Called by the JCRE to process an incoming APDU command.
     * An applet is expected to perform the action
     * requested and return response data if any to the terminal.<p>
     * Upon normal return from this
     * method the JCRE sends the ISO 7816-4 defined success status (90 00) in APDU response.
     * If this method throws an <code>ISOException</code> the JCRE sends the associated reason code as the
     * response status instead.<p>
     * The JCRE zeroes out the APDU buffer before receiving a new APDU command from the CAD.
     * The five header bytes of the APDU command are available in APDU buffer[0..4] at the time
     * this method is called.<p>
     * The <code>APDU</code> object parameter is a temporary JCRE Entry Point Object.
     * A temporary JCRE Entry Point Object can be accessed from any applet context. References
     * to these temporary objects cannot be stored in class variables or instance variables
     * or array components.<p>
     * Notes:<ul>
     * <li><em>APDU buffer[5..] is undefined and should not be read or written prior to invoking the
     * </em><code>APDU.setIncomingAndReceive()</code><em> method if incoming data is expected. Altering
     * the APDU buffer[5..] could corrupt incoming data.</em>
     * </ul>
     * @see APDU
     * @param apdu the incoming <code>APDU</code> object
     * @exception ISOException with the response bytes per ISO 7816-4
     */
    public abstract void process(APDU apdu) throws ISOException;
    
    /**
     * Called by the JCRE to inform this applet that it has been selected.
     * <p>It is called when a SELECT APDU command is received and
     * before the applet is selected. SELECT APDU commands use instance AID bytes for applet
     * selection.
     * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 4.2 for details.<p>
     * A subclass of <code>Applet</code> should override this method
     * if it should perform any initialization that may be required to
     * process APDU commands that may follow.
     * This method returns a boolean to indicate that it is ready to accept incoming APDU
     * commands via its <code>process()</code> method. If this method returns false, it indicates to
     * the JCRE that this Applet declines to be selected.
     * <p>
     * The implementation of this method provided by
     * <code>Applet</code> class returns <code>true</code>.<p>
     * @return <code>true</code> to indicate success, <code>false</code> otherwise.
     */
    public boolean select()
    {
        return true;
    }

    /**
     * Called by the JCRE to inform this currently selected applet that another (or the same) applet
     * will be selected. It is called when a SELECT APDU command is received by the JCRE. This method is invoked
     * prior to another applets or this very applets <code>select()</code> method
     * being invoked.
     * <p>
     * A subclass of <code>Applet</code> should override this method if
     * it has any cleanup or bookkeeping work to be performed before another
     * applet is selected.
     * <p>
     * The default implementation of this method provided by <code>Applet</code> class does nothing.<p>
     * Notes:<ul>
     * <li><em>Unchecked exceptions thrown by this method are caught by the JCRE but the
     * applet is deselected.</em>
     * <li><em>Transient objects of </em><code>JCSystem.CLEAR_ON_DESELECT</code><em> clear event type 
     * are cleared to their default value by the JCRE after this method.</em>
     * <li><em>This method is NOT called on reset or power loss.</em>
     * </ul>
     */
    public void deselect(){}

    /**
     * Called by the JCRE to obtain a shareable interface object from this server applet, on
     * behalf of a request from a client applet. This method executes in the applet context of
     * <code>this</code> applet instance.
     * The client applet initiated this request by calling the
     * <code>JCSystem.getAppletShareableInterfaceObject()</code> method.
     * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.4 for details.
     * <p>Note:<ul>
     * <li><em>The </em><code>clientAID</code><em> parameter is a JCRE owned </em><code>AID</code><em>
     * instance. JCRE owned instances of <code>AID</code> are permanent JCRE
     * Entry Point Objects and can be accessed from any applet context.
     * References to these permanent objects can be stored and re-used.</em>
     * </ul>
     * @param clientAID the <code>AID</code> object of the client applet.  
     * @param parameter optional parameter byte. The parameter byte may be used by the client to specify
     * which shareable interface object is being requested.
     * @return the shareable interface object or <code>null</code>.
     * @see javacard.framework.JCSystem#getAppletShareableInterfaceObject(AID, byte)
     */
    public Shareable getShareableInterfaceObject(AID clientAID, byte param /*CM parameter */) {
        return null;
    }

    /*
     * This method is used by the applet to register <code>this</code> applet instance with
     * the JCRE and to
     * assign the Java Card name of the applet as its instance AID bytes. 
     * One of the <code>register()</code> methods must be called from within <code>install()</code>
     * to be registered with the JCRE.
     * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 3.1 for details.
     * <p>Note:<ul>
     * <li><em>The phrase "Java card name of the applet" is a reference to the </em><code>AID[AID_length]</code><em>
     * item in the </em><code>applets[]</code><em> item of the </em><code>applet_component</code><em>, as documented in Section 6.5
     * Applet Component in the Java Card Virtual Machine Specification.</em>
     * </ul>
     * @exception SystemException with the following reason codes:<ul>
     * <li><code>SystemException.ILLEGAL_AID</code> if the <code>Applet</code> subclass AID bytes are in use or
     * if the applet instance has previously successfully registered with the JCRE via one of the
     * <code>register()</code> methods or if a JCRE initiated <code>install()</code> method execution is not in progress.
     * </ul>
     */
    protected final void register() throws SystemException
    {
	thePrivAccess.register(this);
    }

    /**
     * This method is used by the applet to register <code>this</code> applet instance with the JCRE and
     * assign the specified AID bytes as its instance AID bytes.
     * One of the <code>register()</code> methods must be called from within <code>install()</code>
     * to be registered with the JCRE.
     * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 3.1 for details.
     * @param bArray the byte array containing the AID bytes.
     * @param bOffset the start of AID bytes in bArray.
     * @param bLength the length of the AID bytes in bArray.
     * @exception SystemException with the following reason code:<ul>
     * <li><code>SystemException.ILLEGAL_VALUE</code> if the <code>bLength</code> parameter is
     * less than <code>5</code> or greater than <code>16</code>.
     * <li><code>SystemException.ILLEGAL_AID</code> if the specified instance AID bytes are in use or
     * if the RID portion of the AID bytes in the <code>bArray</code> parameter
     * does not match the RID portion of the Java Card name of the applet or
     * if the applet instance has previously successfully registered with the JCRE via one of the
     * <code>register()</code> methods or if a JCRE initiated <code>install()</code> method execution is not in progress.
     * </ul>
     * <p>Note:<ul>
     * <li><em>The phrase "Java card name of the applet" is a reference to the </em><code>AID[AID_length]</code><em>
     * item in the </em><code>applets[]</code><em> item of the </em><code>applet_component</code><em>, as documented in Section 6.5
     * Applet Component in the Java Card Virtual Machine Specification.</em>
     * </ul>
     */
    protected final void register(byte[] bArray, short bOffset, byte bLength ) throws SystemException
    {
        if (bLength<5 || bLength>16) SystemException.throwIt( SystemException.ILLEGAL_VALUE );
        thePrivAccess.register(this, bArray, bOffset, bLength );
    }

   /**
     * This method is used by the applet <code>process()</code> method to distinguish
     * the SELECT APDU command which selected <code>this</code> applet, from all other
     * other SELECT APDU commands which may relate to file or internal applet state selection.
     * @return <code>true</code> if <code>this</code> applet is being selected.
     */
    protected final boolean selectingApplet()
    {
        return thePrivAccess.selectingApplet();
    }


}
