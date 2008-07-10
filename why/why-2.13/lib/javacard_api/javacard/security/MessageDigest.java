/*
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

/*
// $Workfile: MessageDigest.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/security/MessageDigest.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Andy
// */

package javacard.security;

/**
 * The <code>MessageDigest</code> class is the base class for hashing algorithms. Implementations of MessageDigest
 * algorithms must extend this class and implement all the abstract methods.
 * <p> A tear or card reset event resets a 
 * <code>MessageDigest</code> object to the initial state (state upon construction).
 */

abstract public class MessageDigest{


    // algorithm options

    /**
     * Message Digest algorithm SHA.
    */
    public static final byte ALG_SHA        = 1;

    /**
     * Message Digest algorithm MD5.
     */
     public static final byte ALG_MD5       = 2;

    /**
     * Message Digest algorithm RIPE MD-160.
     */
     public static final byte ALG_RIPEMD160 = 3;

	/**
	 * Creates a <code>MessageDigest</code> object instance of the selected algorithm.
	 * @param algorithm the desired message digest algorithm. Valid codes listed in ALG_.. constants. See above.
	 * @param externalAccess if <code>true</code> indicates that the instance will be shared among
	 * multiple applet instances and that the <code>MessageDigest</code> instance will also be accessed (via a <code>Shareable</code>
	 * interface) when the owner of the <code>MessageDigest</code> instance is not the currently selected applet.
	 * @return the <code>MessageDigest</code> object instance of the requested algorithm.
	 * @exception CryptoException with the following reason codes:<ul>
     * <li><code>CryptoException.NO_SUCH_ALGORITHM</code> if the requested algorithm is not supported.</ul>
	*/
	public static final MessageDigest getInstance(byte algorithm, boolean externalAccess) throws CryptoException{
	    CryptoException.throwIt( CryptoException.NO_SUCH_ALGORITHM);
	    return null;
	    }

	/**
	 *  Protected Constructor
	 */
	protected MessageDigest(){}

	/**
	 * Gets the Message digest algorithm.
	 * @return the algorithm code defined above.
	*/
	abstract public byte getAlgorithm();

      /**
	   * Returns the byte length of the hash.
	   * @return hash length
	   */
    abstract public byte getLength();

	  /**
	   * Generates a hash of all/last input data.
	   * Completes and returns the hash computation after performing final operations such as padding.
	   * The <code>MessageDigest</code> object is reset to the initial state after this call is made.
	   * <p>The input and output buffer data may overlap.
	   * @param inBuff the input buffer of data to be hashed
	   * @param inOffset the offset into the input buffer at which to begin hash generation
	   * @param inLength the byte length to hash
	   * @param outBuff the output buffer, may be the same as the input buffer
	   * @param outOffset the offset into the output buffer where the resulting hash value begins
	   * @return number of bytes of hash output in <code>outBuff</code>
	   */
    abstract public short doFinal(
        byte[] inBuff,
        short inOffset,
        short inLength,
        byte[] outBuff,
        short outOffset);

      /**
	   * Accumulates a hash of the input data. This method requires temporary storage of 
	   * intermediate results. In addition, if the input data length is not block aligned
	   * (multiple of block size)
	   * then additional internal storage may be allocated at this time to store a partial
	   * input data block.
	   * This may result in additional resource consumption and/or slow performance.
	   * This method should only be used if all the input data required for the hash
	   * is not available in one byte array. The doFinal() method
	   * is recommended whenever possible.
	   * @param inBuff the input buffer of data to be hashed
	   * @param inOffset the offset into the input buffer at which to begin hash generation
	   * @param inLength the byte length to hash
	   * @see #doFinal
	   */
    abstract public void update(
        byte[] inBuff,
        short inOffset,
        short inLength);

      /**
	   * Resets the <code>MessageDigest</code> object to the initial state for further use.
	   */
    abstract public void reset();
}
