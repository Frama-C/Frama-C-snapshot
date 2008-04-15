/*
 * @(#)PrintStream.java	1.25 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package java.io;


/**
 * A <code>PrintStream</code> adds functionality to another output stream,
 * namely the ability to print representations of various data values
 * conveniently.  Two other features are provided as well.  Unlike other output
 * streams, a <code>PrintStream</code> never throws an
 * <code>IOException</code>; instead, exceptional situations merely set an
 * internal flag that can be tested via the <code>checkError</code> method.
 * Optionally, a <code>PrintStream</code> can be created so as to flush
 * automatically; this means that the <code>flush</code> method is
 * automatically invoked after a byte array is written, one of the
 * <code>println</code> methods is invoked, or a newline character or byte
 * (<code>'\n'</code>) is written.
 *
 * <p> All characters printed by a <code>PrintStream</code> are converted into
 * bytes using the platform's default character encoding.  The <code>{@link
 * PrintWriter}</code> class should be used in situations that require writing
 * characters rather than bytes.
 *
 * @version    1.25, 03/01/23
 * @author     Frank Yellin
 * @author     Mark Reinhold
 * @since      JDK1.0
 */

public class PrintStream extends FilterOutputStream {

    private boolean autoFlush = false;
    private boolean trouble = false;

    /**
     * Track both the text- and character-output streams, so that their buffers
     * can be flushed without flushing the entire stream.
     */
    //KML private BufferedWriter textOut;
    //KML private OutputStreamWriter charOut;

    /**
     * Create a new print stream.  This stream will not flush automatically.
     *
     * @param  out        The output stream to which values and objects will be
     *                    printed
     *
     * @see java.io.PrintWriter#PrintWriter(java.io.OutputStream)
     */
    public PrintStream(OutputStream out) {
	this(out, false);
    }

    /* Initialization is factored into a private constructor (note the swapped
     * parameters so that this one isn't confused with the public one) and a
     * separate init method so that the following two public constructors can
     * share code.  We use a separate init method so that the constructor that
     * takes an encoding will throw an NPE for a null stream before it throws
     * an UnsupportedEncodingException for an unsupported encoding.
     */

    private PrintStream(boolean autoFlush, OutputStream out)
    {
	super(out);
	if (out == null)
	    throw new NullPointerException("Null output stream");
	this.autoFlush = autoFlush;
    }

    private void init(OutputStreamWriter osw) {
	this.charOut = osw;
	this.textOut = new BufferedWriter(osw);
    }

    /**
     * Create a new print stream.
     *
     * @param  out        The output stream to which values and objects will be
     *                    printed
     * @param  autoFlush  A boolean; if true, the output buffer will be flushed
     *                    whenever a byte array is written, one of the
     *                    <code>println</code> methods is invoked, or a newline
     *                    character or byte (<code>'\n'</code>) is written
     *
     * @see java.io.PrintWriter#PrintWriter(java.io.OutputStream, boolean)
     */
    public PrintStream(OutputStream out, boolean autoFlush) {
	this(autoFlush, out);
	init(new OutputStreamWriter(this));
    }

    /**
     * Create a new print stream.
     *
     * @param  out        The output stream to which values and objects will be
     *                    printed
     * @param  autoFlush  A boolean; if true, the output buffer will be flushed
     *                    whenever a byte array is written, one of the
     *                    <code>println</code> methods is invoked, or a newline
     *                    character or byte (<code>'\n'</code>) is written
     * @param  encoding   The name of a supported
     *                    <a href="../lang/package-summary.html#charenc">
     *                    character encoding</a>
     *
     * @exception  UnsupportedEncodingException
     *             If the named encoding is not supported
     */
    public PrintStream(OutputStream out, boolean autoFlush, String encoding)
        throws UnsupportedEncodingException
    {
	this(autoFlush, out);
	init(new OutputStreamWriter(this, encoding));
    }

    /** Check to make sure that the stream has not been closed */
    private void ensureOpen() throws IOException {
	if (out == null)
	    throw new IOException("Stream closed");
    }

    /**
     * Flush the stream.  This is done by writing any buffered output bytes to
     * the underlying output stream and then flushing that stream.
     *
     * @see        java.io.OutputStream#flush()
     */
    public void flush() {
	synchronized (this) {
	    try {
		ensureOpen();
		out.flush();
	    }
	    catch (IOException x) {
		trouble = true;
	    }
	}
    }

    private boolean closing = false; /* To avoid recursive closing */

    /**
     * Close the stream.  This is done by flushing the stream and then closing
     * the underlying output stream.
     *
     * @see        java.io.OutputStream#close()
     */
    public void close() {
	synchronized (this) {
	    if (! closing) {
		closing = true;
		try {
		    textOut.close();
		    out.close();
		}
		catch (IOException x) {
		    trouble = true;
		}
		textOut = null;
		charOut = null;
		out = null;
	    }
	}
    }

    /**
     * Flush the stream and check its error state.  The internal error state
     * is set to <code>true</code> when the underlying output stream throws an
     * <code>IOException</code> other than <code>InterruptedIOException</code>,
     * and when the <code>setError</code> method is invoked.  If an operation
     * on the underlying output stream throws an
     * <code>InterruptedIOException</code>, then the <code>PrintStream</code>
     * converts the exception back into an interrupt by doing:
     * <pre>
     *     Thread.currentThread().interrupt();
     * </pre>
     * or the equivalent.
     *
     * @return True if and only if this stream has encountered an
     *         <code>IOException</code> other than
     *         <code>InterruptedIOException</code>, or the
     *         <code>setError</code> method has been invoked
     */
    public boolean checkError() {
	if (out != null)
	    flush();
	return trouble;
    }

    /**
     * Set the error state of the stream to <code>true</code>.
     *
     * @since JDK1.1
     */
    protected void setError() {
	trouble = true;
    }


    /*
     * Exception-catching, synchronized output operations,
     * which also implement the write() methods of OutputStream
     */

    /**
     * Write the specified byte to this stream.  If the byte is a newline and
     * automatic flushing is enabled then the <code>flush</code> method will be
     * invoked.
     *
     * <p> Note that the byte is written as given; to write a character that
     * will be translated according to the platform's default character
     * encoding, use the <code>print(char)</code> or <code>println(char)</code>
     * methods.
     *
     * @param  b  The byte to be written
     * @see #print(char)
     * @see #println(char)
     */
    public void write(int b) {
	try {
	    synchronized (this) {
		ensureOpen();
		out.write(b);
		if ((b == '\n') && autoFlush)
		    out.flush();
	    }
	}
	catch (InterruptedIOException x) {
	    Thread.currentThread().interrupt();
	}
	catch (IOException x) {
	    trouble = true;
	}
    }

    /**
     * Write <code>len</code> bytes from the specified byte array starting at
     * offset <code>off</code> to this stream.  If automatic flushing is
     * enabled then the <code>flush</code> method will be invoked.
     *
     * <p> Note that the bytes will be written as given; to write characters
     * that will be translated according to the platform's default character
     * encoding, use the <code>print(char)</code> or <code>println(char)</code>
     * methods.
     *
     * @param  buf   A byte array
     * @param  off   Offset from which to start taking bytes
     * @param  len   Number of bytes to write
     */
    public void write(byte buf[], int off, int len) {
	try {
	    synchronized (this) {
		ensureOpen();
		out.write(buf, off, len);
		if (autoFlush)
		    out.flush();
	    }
	}
	catch (InterruptedIOException x) {
	    Thread.currentThread().interrupt();
	}
	catch (IOException x) {
	    trouble = true;
	}
    }

    /*
     * The following private methods on the text- and character-output streams
     * always flush the stream buffers, so that writes to the underlying byte
     * stream occur as promptly as with the original PrintStream.
     */

    private void write(char buf[]) {
	try {
	    synchronized (this) {
		ensureOpen();
		textOut.write(buf);
		textOut.flushBuffer();
		charOut.flushBuffer();
		if (autoFlush) {
		    for (int i = 0; i < buf.length; i++)
			if (buf[i] == '\n')
			    out.flush();
		}
	    }
	}
	catch (InterruptedIOException x) {
	    Thread.currentThread().interrupt();
	}
	catch (IOException x) {
	    trouble = true;
	}
    }

    private void write(String s) {
	try {
	    synchronized (this) {
		ensureOpen();
		textOut.write(s);
		textOut.flushBuffer();
		charOut.flushBuffer();
		if (autoFlush && (s.indexOf('\n') >= 0))
		    out.flush();
	    }
	}
	catch (InterruptedIOException x) {
	    Thread.currentThread().interrupt();
	}
	catch (IOException x) {
	    trouble = true;
	}
    }

    private void newLine() {
	try {
	    synchronized (this) {
		ensureOpen();
		textOut.newLine();
		textOut.flushBuffer();
		charOut.flushBuffer();
		if (autoFlush)
		    out.flush();
	    }
	}
	catch (InterruptedIOException x) {
	    Thread.currentThread().interrupt();
	}
	catch (IOException x) {
	    trouble = true;
	}
    }


    /* Methods that do not terminate lines */

    /**
     * Print a boolean value.  The string produced by <code>{@link
     * java.lang.String#valueOf(boolean)}</code> is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the
     * <code>{@link #write(int)}</code> method.
     *
     * @param      b   The <code>boolean</code> to be printed
     */
    public void print(boolean b) {
	write(b ? "true" : "false");
    }

    /**
     * Print a character.  The character is translated into one or more bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the
     * <code>{@link #write(int)}</code> method.
     *
     * @param      c   The <code>char</code> to be printed
     */
    public void print(char c) {
	write(String.valueOf(c));
    }

    /**
     * Print an integer.  The string produced by <code>{@link
     * java.lang.String#valueOf(int)}</code> is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the
     * <code>{@link #write(int)}</code> method.
     *
     * @param      i   The <code>int</code> to be printed
     * @see        java.lang.Integer#toString(int)
     */
    public void print(int i) {
	write(String.valueOf(i));
    }

    /**
     * Print a long integer.  The string produced by <code>{@link
     * java.lang.String#valueOf(long)}</code> is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the
     * <code>{@link #write(int)}</code> method.
     *
     * @param      l   The <code>long</code> to be printed
     * @see        java.lang.Long#toString(long)
     */
    public void print(long l) {
	write(String.valueOf(l));
    }

    /**
     * Print a floating-point number.  The string produced by <code>{@link
     * java.lang.String#valueOf(float)}</code> is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the
     * <code>{@link #write(int)}</code> method.
     *
     * @param      f   The <code>float</code> to be printed
     * @see        java.lang.Float#toString(float)
     */
    public void print(float f) {
	write(String.valueOf(f));
    }

    /**
     * Print a double-precision floating-point number.  The string produced by
     * <code>{@link java.lang.String#valueOf(double)}</code> is translated into
     * bytes according to the platform's default character encoding, and these
     * bytes are written in exactly the manner of the <code>{@link
     * #write(int)}</code> method.
     *
     * @param      d   The <code>double</code> to be printed
     * @see        java.lang.Double#toString(double)
     */
    public void print(double d) {
	write(String.valueOf(d));
    }

    /**
     * Print an array of characters.  The characters are converted into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the
     * <code>{@link #write(int)}</code> method.
     *
     * @param      s   The array of chars to be printed
     * 
     * @throws  NullPointerException  If <code>s</code> is <code>null</code>
     */
    public void print(char s[]) {
	write(s);
    }

    /**
     * Print a string.  If the argument is <code>null</code> then the string
     * <code>"null"</code> is printed.  Otherwise, the string's characters are
     * converted into bytes according to the platform's default character
     * encoding, and these bytes are written in exactly the manner of the
     * <code>{@link #write(int)}</code> method.
     *
     * @param      s   The <code>String</code> to be printed
     */
    public void print(String s) {
	if (s == null) {
	    s = "null";
	}
	write(s);
    }

    /**
     * Print an object.  The string produced by the <code>{@link
     * java.lang.String#valueOf(Object)}</code> method is translated into bytes
     * according to the platform's default character encoding, and these bytes
     * are written in exactly the manner of the
     * <code>{@link #write(int)}</code> method.
     *
     * @param      obj   The <code>Object</code> to be printed
     * @see        java.lang.Object#toString()
     */
    public void print(Object obj) {
	write(String.valueOf(obj));
    }


    /* Methods that do terminate lines */

    /**
     * Terminate the current line by writing the line separator string.  The
     * line separator string is defined by the system property
     * <code>line.separator</code>, and is not necessarily a single newline
     * character (<code>'\n'</code>).
     */
    public void println() {
	newLine();
    }

    /**
     * Print a boolean and then terminate the line.  This method behaves as
     * though it invokes <code>{@link #print(boolean)}</code> and then
     * <code>{@link #println()}</code>.
     *
     * @param x  The <code>boolean</code> to be printed
     */
    public void println(boolean x) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

    /**
     * Print a character and then terminate the line.  This method behaves as
     * though it invokes <code>{@link #print(char)}</code> and then
     * <code>{@link #println()}</code>.
     *
     * @param x  The <code>char</code> to be printed.
     */
    public void println(char x) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

    /**
     * Print an integer and then terminate the line.  This method behaves as
     * though it invokes <code>{@link #print(int)}</code> and then
     * <code>{@link #println()}</code>.
     *
     * @param x  The <code>int</code> to be printed.
     */
    public void println(int x) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

    /**
     * Print a long and then terminate the line.  This method behaves as
     * though it invokes <code>{@link #print(long)}</code> and then
     * <code>{@link #println()}</code>.
     *
     * @param x  a The <code>long</code> to be printed.
     */
    public void println(long x) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

    /**
     * Print a float and then terminate the line.  This method behaves as
     * though it invokes <code>{@link #print(float)}</code> and then
     * <code>{@link #println()}</code>.
     *
     * @param x  The <code>float</code> to be printed.
     */
    public void println(float x) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

    /**
     * Print a double and then terminate the line.  This method behaves as
     * though it invokes <code>{@link #print(double)}</code> and then
     * <code>{@link #println()}</code>.
     *
     * @param x  The <code>double</code> to be printed.
     */
    public void println(double x) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

    /**
     * Print an array of characters and then terminate the line.  This method
     * behaves as though it invokes <code>{@link #print(char[])}</code> and
     * then <code>{@link #println()}</code>.
     *
     * @param x  an array of chars to print.
     */
    public void println(char x[]) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

    /**
     * Print a String and then terminate the line.  This method behaves as
     * though it invokes <code>{@link #print(String)}</code> and then
     * <code>{@link #println()}</code>.
     *
     * @param x  The <code>String</code> to be printed.
     */
    public void println(String x) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

    /**
     * Print an Object and then terminate the line.  This method behaves as
     * though it invokes <code>{@link #print(Object)}</code> and then
     * <code>{@link #println()}</code>.
     *
     * @param x  The <code>Object</code> to be printed.
     */
    public void println(Object x) {
	synchronized (this) {
	    print(x);
	    newLine();
	}
    }

}
