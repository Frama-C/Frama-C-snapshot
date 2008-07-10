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
// $Workfile: KeyBuilder.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/security/KeyBuilder.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Andy
// */

package javacard.security;

/**
 * The <code>KeyBuilder</code> class is a key object factory.
 *
 */

public class KeyBuilder{

	// keyType parameter options

	/**
     * <code>Key</code> object which implements interface type <code>DESKey</code>
     * with CLEAR_ON_RESET transient key data.
     * <p>This <code>Key</code> object implicitly performs a <code>clearKey()</code> on
     * power on or card reset.
     */
	public static final byte TYPE_DES_TRANSIENT_RESET      = 1;

	/**
     * <code>Key</code> object which implements interface type <code>DESKey</code>
     * with CLEAR_ON_DESELECT transient key data.
     * <p>This <code>Key</code> object implicitly performs a <code>clearKey()</code> on
     * power on, card reset and applet deselection.
     */
	public static final byte TYPE_DES_TRANSIENT_DESELECT   = 2;

	/**
     * <code>Key</code> object which implements interface type <code>DESKey</code> with persistent key data.
     */
	public static final byte TYPE_DES                      = 3;

	/**
     * <code>Key</code> object which implements interface type <code>RSAPublicKey</code>.
     */
	public static final byte TYPE_RSA_PUBLIC               = 4;

	/**
     * <code>Key</code> object which implements interface type <code>RSAPrivateKey</code> which
     * uses modulus/exponent form.
     */
	public static final byte TYPE_RSA_PRIVATE              = 5;

	/**
     * <code>Key</code> object which implements interface type <code>RSAPrivateCrtKey</code> which
     * uses Chinese Remainder Theorem.
     */
	public static final byte TYPE_RSA_CRT_PRIVATE          = 6;

	/**
     * <code>Key</code> object which implements the interface type <code>DSAPublicKey</code>
     * for the DSA algorithm.
     */
	public static final byte TYPE_DSA_PUBLIC               = 7;

	/**
     * <code>Key</code> object which implements the interface type <code>DSAPrivateKey</code>
     * for the DSA algorithm.
     */
	public static final byte TYPE_DSA_PRIVATE              = 8;

	// keyLength parameter options
   /**
     * DES Key Length <code>LENGTH_DES</code> = 64.
     */
	public static final short LENGTH_DES  = 64;

	/**
     * DES Key Length <code>LENGTH_DES3_2KEY</code> = 128.
     */
	public static final short LENGTH_DES3_2KEY = 128;

	/**
     * DES Key Length <code>LENGTH_DES3_3KEY</code> = 192.
     */
	public static final short LENGTH_DES3_3KEY = 192;

	/**
     * RSA Key Length <code>LENGTH_RSA_512</code> = 512.
     */
	public static final short LENGTH_RSA_512 = (short)512;

	/**
     * RSA Key Length <code>LENGTH_RSA_768</code> = 768.
     */
	public static final short LENGTH_RSA_768 = (short)768;

	/**
     * RSA Key Length <code>LENGTH_RSA_1024</code> = 1024.
     */
	public static final short LENGTH_RSA_1024 = (short)1024;

	/**
     * RSA Key Length <code>LENGTH_RSA_2048</code> = 2048.
     */
	public static final short LENGTH_RSA_2048 = (short)2048;

	/**
     * DSA Key Length <code>LENGTH_DSA_512</code> = 512.
     */
	public static final short LENGTH_DSA_512 = (short)512;

	/**
     * DSA Key Length <code>LENGTH_DSA_768</code> = 768.
     */
	public static final short LENGTH_DSA_768 = (short)768;

	/**
     * DSA Key Length <code>LENGTH_DSA_1024</code> = 1024.
     */
	public static final short LENGTH_DSA_1024 = (short)1024;

	/**
	 * Creates uninitialized cryptographic keys for signature and cipher algorithms. Instances created
	 * by this method may be the only key objects used to initialize instances of
	 * <code>Signature</code> and <code>Cipher</code>.
	 * Note that the object returned must be cast to their appropriate key type interface.
	 * @param keyType the type of key to be generated. Valid codes listed in TYPE.. constants.
	 * @param keyLength the key size in bits. The valid key bit lengths are key type dependent. See above.
	 * @param keyEncryption if <code>true</code> this boolean requests a key implementation
	 * which implements the <code>javacardx.crypto.KeyEncryption</code> interface.
	 * The key implementation returned may implement the <code>javacardx.crypto.KeyEncryption</code>
	 * interface even when this parameter is <code>false</code>.
	 * @return the key object instance of the requested key type, length and encrypted access.
	 * @exception CryptoException with the following reason codes:<ul>
     * <li><code>CryptoException.NO_SUCH_ALGORITHM</code> if the requested algorithm
     * associated with the specified type, size of key and key encryption interface is not supported.</ul>
	*/
	public static Key buildKey( byte keyType, short keyLength, boolean keyEncryption ) throws CryptoException {

	    switch ( keyType ){
	        default :
	            CryptoException.throwIt( CryptoException.NO_SUCH_ALGORITHM );
	    }
	    return null;
	}

	/**
	 * No constructor
	 */
	KeyBuilder() {}

}
