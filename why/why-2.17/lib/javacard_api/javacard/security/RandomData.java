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
// $Workfile: RandomData.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/security/RandomData.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Andy
// */

package javacard.security;

//
/**
 * The <code>RandomData</code> abstract class is the base class for random number generation. Implementations of RandomData
 * algorithms must extend this class and implement all the abstract methods.
 */

abstract public class RandomData{

  // Random Number algorithm options

    /**
     * Utility pseudo random number generation algorithms.
    */
    public static final byte ALG_PSEUDO_RANDOM        = 1;

    /**
     * Cryptographically secure random number generation algorithms.
    */
    public static final byte ALG_SECURE_RANDOM        = 2;


  /**
   * Protected constructor for subclassing.
   */
    protected RandomData(){}

  /**
    * Creates a <code>RandomData</code> instance of the selected algorithm.
    * The pseudo random <code>RandomData</code> instance's seed is initialized to a internal default value.
	* @param algorithm the desired random number algorithm. Valid codes listed in ALG_.. constants. See above.
	* @return the <code>RandomData</code> object instance of the requested algorithm.
	* @exception CryptoException with the following reason codes:<ul>
    * <li><code>CryptoException.NO_SUCH_ALGORITHM</code> if the requested algorithm is not supported.</ul>
	*/
	public static final RandomData getInstance(byte algorithm) throws CryptoException{

	    switch ( algorithm ){
	        case ALG_PSEUDO_RANDOM :
	        case ALG_SECURE_RANDOM :
	            CryptoException.throwIt( CryptoException.NO_SUCH_ALGORITHM);
	            return null;
	    }
	    return null;
	 }


  /**
   * Generates random data.
   * @param buffer the output buffer
   * @param offset the offset into the output buffer
   * @param length the length of random data to generate
   */
   abstract public void generateData(
        byte[] buffer,
        short offset,
        short length);
 //   {
 //   	Randomness.generate(buffer,offset,length);
 //   }

  /**
   * Seeds the random data generator.
   * @param buffer the input buffer
   * @param offset the offset into the input buffer
   * @param length the length of the seed data
   */
    abstract public void setSeed(
        byte[] buffer,
        short offset,
        short length);
//    {
//    	Randomness.setSeed(buffer,offset,length);
//    }
}
