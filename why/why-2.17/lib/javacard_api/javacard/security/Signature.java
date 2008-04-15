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
// $Workfile: Signature.java $
// $Revision: 1.4 $
// $Date: 2008/01/31 18:27:25 $
// $Author: nrousset $
// $Archive: /Products/Europa/api21/javacard/security/Signature.java $
// $Modtime: 5/02/00 8:48p $
// Original author:  Andy
// */

package javacard.security;

/**
 * The <code>Signature</code> class is the base class for Signature algorithms. Implementations of Signature
 * algorithms must extend this class and implement all the abstract methods.
 * <p>The term "pad" is used in the public key signature algorithms below to refer to all the
 * operations specified in the referenced scheme to transform the message digest into
 * the encryption block size.
 * <p> A tear or card reset event resets an initialized 
 * <code>Signature</code> object to the state it was in when previously initialized
 * via a call to <code>init()</code>.<br>
 * <p>Note:<ul>
 * <li><em>On a tear or card reset event, the DES and triple DES algorithms in outer CBC mode
 * reset the initial vector(IV) to 0. The initial vector(IV) can be re-initialized using the
 * </em><code>init(Key, byte, byte[], short, short)</code><em> method.</em>
 * </ul>
 */
abstract public class Signature{


    // algorithm options
    /**
     * Signature algorithm <code>ALG_DES_MAC4_NOPAD</code> generates a 4 byte MAC
     * (most significant 4 bytes of encrypted block) using DES or triple DES in CBC mode.&nbsp;
     * This algorithm uses outer CBC for triple DES.&nbsp;
     * This algorithm does not pad input data.
     * If the input data is not (8 byte) block aligned it throws <code>CryptoException</code>
     * with the reason code <code>ILLEGAL_USE</code>.
     *
     */
    public static final byte ALG_DES_MAC4_NOPAD           = 1;

    /**
     * Signature algorithm <code>ALG_DES_MAC_8_NOPAD</code> generates a 8 byte MAC
     * using DES or triple DES in CBC mode.&nbsp;This algorithm uses outer CBC for triple DES.&nbsp;
     * This algorithm does not pad input data.
     * If the input data is not (8 byte) block aligned it throws <code>CryptoException</code>
     * with the reason code <code>ILLEGAL_USE</code>.
     * <p>Note:
     * <ul><li><em>This algorithm must not be implemented if export restrictions apply.</em></ul>
     */
    public static final byte ALG_DES_MAC8_NOPAD           = 2;

    /**
     * Signature algorithm <code>ALG_DES_MAC4_ISO9797_M1</code> generates a 4 byte MAC
     * (most significant 4 bytes of encrypted block) using DES or triple DES in CBC mode.&nbsp;
     * This algorithm uses outer CBC for triple DES.&nbsp;
     * Input data is padded according to the ISO 9797 method 1 scheme.
     */
    public static final byte ALG_DES_MAC4_ISO9797_M1      = 3;

    /**
     * Signature algorithm <code>ALG_DES_MAC8_ISO9797_M1</code> generates a 8 byte MAC
     * using DES or triple DES in CBC mode.&nbsp;This algorithm uses outer CBC for triple DES.&nbsp;
     * Input data is padded according to the ISO 9797 method 1 scheme.
     * <p>Note:
     * <ul><li><em>This algorithm must not be implemented if export restrictions apply.</em></ul>
     */
    public static final byte ALG_DES_MAC8_ISO9797_M1      = 4;

    /**
     * Signature algorithm <code>ALG_DES_MAC4_ISO9797_M2</code> generates a 4 byte MAC
     * (most significant 4 bytes of encrypted block) using DES or triple DES in CBC mode.&nbsp;
     * This algorithm uses outer CBC for triple DES.&nbsp;
     * Input data is padded according to the ISO 9797 method 2 (ISO 7816-4, EMV'96) scheme.
     */
    public static final byte ALG_DES_MAC4_ISO9797_M2      = 5;

    /**
     * Signature algorithm <code>ALG_DES_MAC8_ISO9797_M2</code> generates a 8 byte MAC
     * using DES or triple DES in CBC mode.&nbsp;This algorithm uses outer CBC for triple DES.&nbsp;
     * Input data is padded according to the ISO 9797 method 2 (ISO 7816-4, EMV'96) scheme.
     * <p>Note:
     * <ul><li><em>This algorithm must not be implemented if export restrictions apply.</em></ul>
     */
    public static final byte ALG_DES_MAC8_ISO9797_M2      = 6;

    /**
     * Signature algorithm <code>ALG_DES_MAC4_PKCS5</code> generates a 4 byte MAC
     * (most significant 4 bytes of encrypted block) using DES or triple DES in CBC mode.&nbsp;
     * This algorithm uses outer CBC for triple DES.&nbsp;
     * Input data is padded according to the PKCS#5 scheme.
     */
    public static final byte ALG_DES_MAC4_PKCS5           = 7;

    /**
     * Signature algorithm </code>ALG_DES_MAC8_PKCS5</code> generates a 8 byte MAC
     * using DES or triple DES in CBC mode.&nbsp;This algorithm uses outer CBC for triple DES.&nbsp;
     * Input data is padded according to the PKCS#5 scheme.
     * <p>Note:
     * <ul><li><em>This algorithm must not be implemented if export restrictions apply.</em></ul>
     */
    public static final byte ALG_DES_MAC8_PKCS5           = 8;

    /**
     * Signature algorithm <code>ALG_RSA_SHA_ISO9796</code> encrypts the 20 byte SHA digest using RSA.&nbsp;
     * The digest is padded according to the ISO 9796 (EMV'96) scheme.
     */
    public static final byte ALG_RSA_SHA_ISO9796          = 9;

    /**
     * Signature algorithm <code>ALG_RSA_SHA_PKCS1</code> encrypts the 20 byte SHA digest using RSA.&nbsp;
     * The digest is padded according to the PKCS#1 (v1.5) scheme.
     * <p>Note:<ul>
     * <li><em> The encryption block(EB) during signing is built as follows:<br>
     * &nbsp; EB = 00 || 01 || PS || 00 || T<br>
     * &nbsp; &nbsp; &nbsp; :: where T is the DER encoding of :<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digestInfo ::= SEQUENCE {<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digestAlgorithm AlgorithmIdentifier of SHA-1,<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digest OCTET STRING<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; }<br>
     * &nbsp; &nbsp; &nbsp; :: PS is an octet string of length k-3-||T|| with value FF.
     * The length of PS must be at least 8 octets.<br>
     * &nbsp; &nbsp; &nbsp; :: k is the RSA modulus size. </em><br>
     * </ul>
     */
    public static final byte ALG_RSA_SHA_PKCS1            = 10;

    /**
     * Signature algorithm <code>ALG_RSA_MD5_PKCS1</code> encrypts the 16 byte MD5 digest using RSA.&nbsp;
     * The digest is padded according to the PKCS#1 (v1.5) scheme.
     * <p>Note:<ul>
     * <li><em> The encryption block(EB) during signing is built as follows:<br>
     * <&nbsp; EB = 00 || 01 || PS || 00 || T<br>
     * &nbsp; &nbsp; &nbsp; :: where T is the DER encoding of :<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digestInfo ::= SEQUENCE {<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digestAlgorithm AlgorithmIdentifier of MD5,<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digest OCTET STRING<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; }<br>
     * &nbsp; &nbsp; &nbsp; :: PS is an octet string of length k-3-||T|| with value FF.
     * The length of PS must be at least 8 octets.<br>
     * &nbsp; &nbsp; &nbsp; :: k is the RSA modulus size. </em><br>
     * </ul>
     */
    public static final byte ALG_RSA_MD5_PKCS1           = 11;

    /**
     * Signature algorithm <code>ALG_RSA_RIPEMD160_ISO9796</code> encrypts the 20 byte RIPE MD-160 digest
     * using RSA.&nbsp;The digest is padded according to the ISO 9796 scheme.
     */
    public static final byte ALG_RSA_RIPEMD160_ISO9796    = 12;

    /**
     * Signature algorithm <code>ALG_RSA_RIPEMD160_PKCS1</code> encrypts the 20 byte RIPE MD-160 digest
     * using RSA.&nbsp;The digest is padded according to the PKCS#1 (v1.5) scheme.
     * <p>Note:<ul>
     * <li><em> The encryption block(EB) during signing is built as follows:<br>
     * <&nbsp; EB = 00 || 01 || PS || 00 || T<br>
     * &nbsp; &nbsp; &nbsp; :: where T is the DER encoding of :<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digestInfo ::= SEQUENCE {<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digestAlgorithm AlgorithmIdentifier of RIPEMD160,<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; digest OCTET STRING<br>
     * &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; }<br>
     * &nbsp; &nbsp; &nbsp; :: PS is an octet string of length k-3-||T|| with value FF.
     * The length of PS must be at least 8 octets.<br>
     * &nbsp; &nbsp; &nbsp; :: k is the RSA modulus size. </em><br>
     * </ul>
     */
    public static final byte ALG_RSA_RIPEMD160_PKCS1      = 13;

    /**
     * Signature algorithm <code>ALG_DSA_SHA</code> signs/verifies the 20 byte SHA digest using DSA.
     */
    public static final byte ALG_DSA_SHA                  = 14;

    /**
     * Signature algorithm <code>ALG_RSA_SHA_RFC2409</code> encrypts the 20 byte SHA digest using RSA.&nbsp;
     * The digest is padded according to the RFC2409 scheme.
     */
    public static final byte ALG_RSA_SHA_RFC2409          = 15;

    /**
     * Signature algorithm <code>ALG_RSA_MD5_RFC2409</code> encrypts the 16 byte MD5 digest using RSA.&nbsp;
     * The digest is padded according to the RFC2409 scheme.
     */
    public static final byte ALG_RSA_MD5_RFC2409          = 16;

    // mode options
    /**
     * Used in <code>init()</code> methods to indicate signature sign mode.
     */
    public static final byte MODE_SIGN                    = 1;
    
    /**
     * Used in <code>init()</code> methods to indicate signature verify mode.
     */
    public static final byte MODE_VERIFY                  = 2;
    
    /**
     * Creates a <code>Signature</code> object instance of the selected algorithm.
     * @param algorithm the desired Signature algorithm. See above.
     * @param externalAccess if <code>true</code> indicates that the instance will be shared among
     * multiple applet instances and that the <code>Signature</code> instance will also be accessed (via a <code>Shareable</code>
     * interface) when the owner of the <code>Signature</code> instance is not the currently selected applet.
     * @return the <code>Signature</code> object instance of the requested algorithm.
     * @exception CryptoException with the following reason codes:<ul>
     * <li><code>CryptoException.NO_SUCH_ALGORITHM</code> if the requested algorithm is not supported.</ul>
     */

    /*@ behavior normal:
      @   ensures \result != null;
      @*/
    public static final Signature getInstance(byte algorithm, boolean externalAccess) 
	throws CryptoException{
	switch ( algorithm ){
	default :
	    CryptoException.throwIt( CryptoException.NO_SUCH_ALGORITHM );
	}
	return null;
    }
    
    /**
     * Initializes the <code>Signature</code> object with the appropriate <code>Key</code>. This method should be used
     * for algorithms which do not need initialization parameters or use default parameter
     * values.
     * <p>Note:<ul>
     * <li><em>DES and triple DES algorithms in CBC mode will use 0 for initial vector(IV) if this
     * method is used.</em>
     * </ul>
     * @param theKey the key object to use for signing or verifying
     * @param theMode one of <code>MODE_SIGN</code> or <code>MODE_VERIFY</code>
     * @exception CryptoException with the following reason codes:<ul>
     * <li><code>CryptoException.ILLEGAL_VALUE</code> if <code>theMode</code> option is an undefined value or
     * if the <code>Key</code> is inconsistent with <code>theMode</code>
     * or with the <code>Signature</code> implementation.
     * </ul>
     */
    abstract public void init ( Key theKey, byte theMode ) throws CryptoException;
    
    /*
     * Initializes the <code>Signature</code> object with the appropriate <code>Key</code> and algorithm specific
     * parameters.
     * <p>Note:<ul>
     * <li><em>DES and triple DES algorithms in outer CBC mode expect an 8 byte parameter value for
     * the initial vector(IV) in </em><code>bArray</code><em>.</em>
     * <li><em>RSA and DSA algorithms throw </em><code>CryptoException.ILLEGAL_VALUE</code><em>.</em>
     * </ul>
     * @param theKey the key object to use for signing
     * @param theMode one of <code>MODE_SIGN</code> or <code>MODE_VERIFY</code>
     * @param bArray byte array containing algorithm specific initialization info.
     * @param bOff offset within <code>bArray</code> where the algorithm specific data begins.
     * @param bLen byte length of algorithm specific parameter data
     * @exception CryptoException with the following reason codes:<ul>
     * <li><code>CryptoException.ILLEGAL_VALUE</code> if <code>theMode</code> option is an undefined value
     * or if a byte array parameter option is not supported by the algorithm or if
     * the <code>bLen</code> is an incorrect byte length for the algorithm specific data or
     * if the <code>Key</code> is inconsistent with <code>theMode</code>
     * or with the <code>Signature</code> implementation.
     * </ul>
     */
    abstract public void init ( Key theKey, byte theMode, byte[] bArray, short bOff, short bLen )
	throws CryptoException;
    
    /**
     * Protected Constructor
     *
     */
    protected Signature() {}

    /**
     * Gets the Signature algorithm.
     * @return the algorithm code defined above.
     */
    abstract public byte getAlgorithm();
    
    /**
     * Returns the byte length of the signature data.
     * @return the byte length of the signature data.
     */
    abstract public short getLength();

    /**
     * Accumulates a signature of the input data. This method requires temporary storage of 
     * intermediate results. In addition, if the input data length is not block aligned
     * (multiple of block size)
     * then additional internal storage may be allocated at this time to store a partial
     * input data block.
     * This may result in additional resource consumption and/or slow performance.
     * This method should only be used if all the input data required for signing/verifying
     * is not available in one byte array. The <code>sign()</code> or
     * <code>verify()</code> method is recommended whenever possible.
     * @param inBuff the input buffer of data to be signed
     * @param inOffset the offset into the input buffer at which to begin signature generation
     * @param inLength the byte length to sign
     * @exception CryptoException with the following reason codes:<ul>
     * <li><code>CryptoException.UNINITIALIZED_KEY</code> if key not initialized.</ul>
     * @see #sign(byte[], short, short, byte[], short)
     * @see #verify(byte[], short, short, byte[], short, short)
     */
    abstract public void update(
        byte[] inBuff,
        short inOffset,
        short inLength) throws CryptoException;

    /**
     * Generates the signature of all/last input data.
     * A call to this method also resets this <code>Signature</code> object to the state it was in
     * when previously initialized via a call to <code>init()</code>.
     * That is, the object is reset and available to sign another message.
     * <p>Note:<ul>
     * <li><em>DES and triple DES algorithms in outer CBC mode reset the initial vector(IV)
     * to 0. The initial vector(IV) can be re-initialized using the
     * </em><code>init(Key, byte, byte[], short, short)</code><em> method.</em>
     * </ul>
     * <p>The input and output buffer data may overlap.
     * @param inBuff the input buffer of data to be signed
     * @param inOffset the offset into the input buffer at which to begin signature generation
     * @param inLength the byte length to sign
     * @param sigBuff the output buffer to store signature data
     * @param sigOffset the offset into sigBuff at which to begin signature data
     * @return number of bytes of signature output in sigBuff
     * @exception CryptoException with the following reason codes:<ul>
     * <li><code>CryptoException.UNINITIALIZED_KEY</code> if key not initialized.
     * <li><code>CryptoException.INVALID_INIT</code> if this <code>Signature</code> object is
     * not initialized or initialized for signature verify mode.
     * <li><code>CryptoException.ILLEGAL_USE</code> if this <code>Signature</code> algorithm
     * does not pad the message and the message is not block aligned.
     * </ul>
     */
    abstract public short sign (
        byte[] inBuff,
        short inOffset,
        short inLength,
        byte[] sigBuff,
        short sigOffset) throws CryptoException;

    /**
     * Verifies the signature of all/last input data against the passed in signature.
     * A call to this method also resets this <code>Signature</code> object to the state it was in
     * when previously initialized via a call to <code>init()</code>.
     * That is, the object is reset and available to verify another message.
     * <p>Note:<ul>
     * <li><em>DES and triple DES algorithms in outer CBC mode reset the initial vector(IV)
     * to 0. The initial vector(IV) can be re-initialized using the
     * </em><code>init(Key, byte, byte[], short, short)</code><em> method.</em>
     * </ul>
     * @param inBuff the input buffer of data to be verified
     * @param inOffset the offset into the input buffer at which to begin signature generation
     * @param inLength the byte length to sign
     * @param sigBuff the input buffer containing signature data
     * @param sigOffset the offset into sigBuff where signature data begins.
     * @param sigLength the byte length of the signature data
     * @return <code>true</code> if signature verifies <code>false</code> otherwise.
     * @exception CryptoException with the following reason codes:<ul>
     * <li><code>CryptoException.UNINITIALIZED_KEY</code> if key not initialized.
     * <li><code>CryptoException.INVALID_INIT</code> if this <code>Signature</code> object is
     * not initialized or initialized for signature sign mode.
     * <li><code>CryptoException.ILLEGAL_USE</code> if this <code>Signature</code> algorithm
     * does not pad the message and the message is not block aligned.
     * </ul>
     */
    abstract public boolean verify (
        byte[] inBuff,
        short inOffset,
        short inLength,
        byte[] sigBuff,
        short sigOffset,
        short sigLength) throws CryptoException;

}









