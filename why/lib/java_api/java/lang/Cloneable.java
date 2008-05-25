/*
 * @(#)Cloneable.java	1.14 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package java.lang;

/**
 * A class implements the <code>Cloneable</code> interface to 
 * indicate to the {@link java.lang.Object#clone()} method that it 
 * is legal for that method to make a 
 * field-for-field copy of instances of that class. 
 * <p>
 * Invoking Object's clone method on an instance that does not implement the 
 * <code>Cloneable</code> interface results in the exception 
 * <code>CloneNotSupportedException</code> being thrown.
 * <p>
 * By convention, classes that implement this interface should override 
 * <tt>Object.clone</tt> (which is protected) with a public method.
 * See {@link java.lang.Object#clone()} for details on overriding this
 * method.
 * <p>
 * Note that this interface does <i>not</t> contain the <tt>clone</tt> method.
 * Therefore, it is not possible to clone an object merely by virtue of the
 * fact that it implements this interface.  Even if the clone method is invoked
 * reflectively, there is no guarantee that it will succeed.
 *
 * @author  unascribed
 * @version 1.14, 01/23/03
 * @see     java.lang.CloneNotSupportedException
 * @see     java.lang.Object#clone()
 * @since   JDK1.0
 */
public interface Cloneable { 
}
