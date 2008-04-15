/*
* $Workfile: Shareable.java $	$Revision: 1.1 $, $Date: 2007/09/26 14:32:59 $
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
// $Workfile: Shareable.java $
// $Revision: 1.1 $
// $Date: 2007/09/26 14:32:59 $
// $Author: marche $
// $Archive: /Products/Europa/api21/javacard/framework/Shareable.java $
// $Modtime: 5/02/00 7:13p $
// Original author:  Mitch
// */

package javacard.framework;

/**
 * The Shareable interface serves to identify all shared objects.
 * Any object that needs to be shared through the applet firewall
 * must directly or indirectly implement this interface. Only those
 * methods specified in a shareable interface are available through
 * the firewall.
 *
 * Implementation classes can implement any number of shareable
 * interfaces and can extend other shareable implementation classes.
 */

public interface Shareable {}
