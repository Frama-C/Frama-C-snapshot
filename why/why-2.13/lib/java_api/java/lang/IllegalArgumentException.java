/*
 * @(#)IllegalArgumentException.java	1.19 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package java.lang;

/**
 * Thrown to indicate that a method has been passed an illegal or 
 * inappropriate argument.
 *
 * @author  unascribed
 * @version 1.19, 01/23/03
 * @see	    java.lang.Thread#setPriority(int)
 * @since   JDK1.0
 */
public
class IllegalArgumentException extends RuntimeException {
    /**
     * Constructs an <code>IllegalArgumentException</code> with no 
     * detail message. 
     */
    public IllegalArgumentException() {
	super();
    }

    /**
     * Constructs an <code>IllegalArgumentException</code> with the 
     * specified detail message. 
     *
     * @param   s   the detail message.
     */
    public IllegalArgumentException(String s) {
	super(s);
    }
}
