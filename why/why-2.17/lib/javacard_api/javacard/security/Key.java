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
// $Workfile: Key.java $
// $Revision: 1.2 $
// $Date: 2007/10/16 11:03:17 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/security/Key.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Andy
// */

package javacard.security;

/**
 * The <code>Key</code> interface is the base interface for all keys.
 *<p>
 * @see KeyBuilder
 */
public interface Key{
  	
  	/**
   	 * Reports the initialized state of the key. Keys must be initialized before
   	 * being used.
   	 * <p>A <code>Key</code> object sets its initialized state to true only when all the associated set
   	 * methods have been invoked at least once since the time the initialized state was set to false.
   	 * <p>A newly created <code>Key</code> object sets its initialized state to false. Invocation of the
   	 * <code>clearKey()</code> method sets the initialized state to false. A key with transient key data
   	 * sets its initialized state to false on the associated clear events.
	 * @return <code>true</code> if the key has been initialized.
   	 */

    boolean isInitialized();

  	/**
   	 * Clears the key and sets its initialized state to false.
   	 */
  	
    void clearKey();
	
	/**
	 * Returns the key interface type.
   	 * @return the key interface type.
   	 *<p>
     * @see KeyBuilder 
   	 */


    byte getType();
	
	/**
	 * Returns the key size in number of bits.
   	 * @return the key size in number of bits.
   	 */
	
    short getSize();
	
	
	
}
