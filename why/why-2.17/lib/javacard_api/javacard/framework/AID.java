/*
* $Workfile: AID.java $    $Revision: 1.1 $, $Date: 2007/09/26 14:32:59 $
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
// $Workfile: AID.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/AID.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Ravi
// */

package javacard.framework;

/**
 * This class encapsulates the Application Identifier(AID) associated with
 * an applet. An AID is defined in ISO 7816-5 to be a sequence of bytes between
 * 5 and 16 bytes in length.<p>
 *
 * The JCRE creates instances of <code>AID</code> class to identify and manage every applet on 
 * the card. Applets need not create instances of this class.
 * An applet may request and use the JCRE 
 * owned instances to identify itself and other applet instances.
 * <p>JCRE owned instances of <code>AID</code> are permanent JCRE Entry Point Objects
 * and can be accessed from any applet context. References to these permanent objects
 * can be stored and re-used.
 * <p>An applet instance can obtain a reference to JCRE owned instances of its own <code>AID</code>
 * object by using the <code>JCSystem.getAID()</code> method and another applet's <code>AID</code> object
 * via the <code>JCSystem.lookupAID()</code> method.
 * <p>An applet uses <code>AID</code> instances to request to share another applet's
 * object or to control access to its own shared object from another applet.
 * See <em>Java Card Runtime Environment (JCRE) Specification</em>, section 6.2.1 for details.</em>
 * @see JCSystem
 * @see SystemException
 */
public final class AID{

  byte[] theAID;

 /**
   * The JCRE uses this constructor to create a new <code>AID</code> instance
   * encapsulating the specified AID bytes.
   * @param bArray the byte array containing the AID bytes.
   * @param offset the start of AID bytes in bArray.
   * @param length the length of the AID bytes in bArray.
   * @exception SystemException with the following reason code:<ul>
   * <li><code>SystemException.ILLEGAL_VALUE</code> if the <code>length</code> parameter is
   * less than <code>5</code> or greater than <code>16</code>.
   * </ul>
   */
  public AID( byte[] bArray, short offset, byte length ) throws SystemException{
    if (length < 5 || length>16) SystemException.throwIt(SystemException.ILLEGAL_VALUE);
    theAID = new byte[length];
    Util.arrayCopy( bArray, offset, theAID, (short)0, length );
    }

  /**
   * Called to get the AID bytes encapsulated within <code>AID</code> object.
   * @param dest byte array to copy the AID bytes.
   * @param offset within dest where the AID bytes begin.
   * @return the length of the AID bytes.
   */
  public byte getBytes (byte[] dest, short offset) {
    Util.arrayCopy( theAID, (short)0, dest, offset, (short)theAID.length );
    return (byte) theAID.length;}

 /*
  * Compares the AID bytes in <code>this</code> <code>AID</code> instance to the AID bytes in the 
  * specified object.
  * The result is <code>true</code> if and only if the argument is not <code>null</code>
  * and is an <code>AID</code> object that encapsulates the same AID bytes as <code>this</code>
  * object.
  * <p>This method does not throw <code>NullPointerException</code>.
  * @param anObject the object to compare <code>this</code> <code>AID</code> against. 
  * @return <code>true</code> if the AID byte values are equal, <code>false</code> otherwise.
  */
    /*CM  public boolean equals( Object anObject )
  {
    if ( ! (anObject instanceof AID) || ((AID)anObject).theAID.length != theAID.length) return false; 
    return (Util.arrayCompare(((AID)anObject).theAID, (short)0, theAID, (short)0, (short)theAID.length) == 0);
  }
    CM*/
 /**
  * Checks if the specified AID bytes in <code>bArray</code> are the same as those encapsulated
  * in <code>this</code> <code>AID</code> object.
  * The result is <code>true</code> if and only if the <code>bArray</code> argument is not <code>null</code>
  * and the AID bytes encapsulated in <code>this</code> <code>AID</code> object are equal to 
  * the specified AID bytes in <code>bArray</code>.
  * <p>This method does not throw <code>NullPointerException</code>.
  * @param bArray containing the AID bytes
  * @param offset within bArray to begin
  * @param length of AID bytes in bArray
  * @return <code>true</code> if equal, <code>false</code> otherwise.
  */
  public boolean equals( byte[] bArray, short offset, byte length )
  {
    if (bArray==null) return false;
    // verify the array index
    byte testByte = bArray[(short)(offset + length - (short)1)];
    return ((length == theAID.length) &&
        (Util.arrayCompare(bArray, offset, theAID, (short)0, length) == 0));
  }

  /**
  * Checks if the specified partial AID byte sequence matches the first <code>length</code> bytes
  * of the encapsulated AID bytes within <code>this</code> <code>AID</code> object.
  * The result is <code>true</code> if and only if the <code>bArray</code> argument is not <code>null</code> 
  * and the input <code>length</code> is less than or equal to the length of the encapsulated AID
  * bytes within <code>this</code> <code>AID</code> object and the specified bytes match.
  * <p>This method does not throw <code>NullPointerException</code>.
  * @param bArray containing the partial AID byte sequence
  * @param offset within bArray to begin
  * @param length of partial AID bytes in bArray
  * @return <code>true</code> if equal, <code>false</code> otherwise.
  */
  public boolean partialEquals( byte[] bArray, short offset, byte length )
  {
    if (bArray==null || length > theAID.length) return false;
    return (Util.arrayCompare(bArray, offset, theAID, (short)0, length) == 0);
  }
   
  /**
  * Checks if the RID (National Registered Application provider identifier) portion of the encapsulated
  * AID bytes within the <code>otherAID</code> object matches
  * that of <code>this</code> <code>AID</code> object.
  * The first 5 bytes of an AID byte sequence is the RID. See ISO 7816-5 for details.
  * The result is <code>true</code> if and only if the argument is not <code>null</code>
  * and is an <code>AID</code> object that encapsulates the same RID bytes as <code>this</code>
  * object.
  * <p>This method does not throw <code>NullPointerException</code>.
  * @param otherAID the <code>AID</code> to compare against.
  * @return <code>true</code> if the RID bytes match, <code>false</code> otherwise.
  */
  public boolean RIDEquals ( AID otherAID )
  {
    if (otherAID==null) return false;
    if ( Util.arrayCompare( theAID, (short)0, otherAID.theAID, (short)0, (short)5 ) == 0 )
        return true;
    return false;
  }

}
