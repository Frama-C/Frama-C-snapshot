
package javacardx.crypto;
import javacard.security.*;

public class Cipher extends Object {

    /*
    static int MODE_DECRYPT;
    static int MODE_ENCRYPT;
    static int ENCRYPT_MODE;
    static int PRIVATE_KEY;
    static int PUBLIC_KEY;
    static int SECRET_KEY;
    static int UNWRAP_MODE;
    static int WRAP_MODE;
    static int ALG_DES_ECB_NOPAD;

    byte[] doFinal();
    byte[] doFinal(byte[] input);
    int doFinal(byte[] output, int outputOffset);
    byte[] doFinal(byte[] input, int inputOffset, int inputLen);
    int doFinal(byte[] input, int inputOffset, int inputLen, byte[] output);
    int doFinal(byte[] input, int inputOffset, int inputLen, byte[] output, int outputOffset);
    int getBlockSize();
    byte[] getIV();
    int getOutputSize(int inputLen);
    void init(DESKey key, int opmode);
    byte[] update(byte[] input);
    byte[] update(byte[] input, int inputOffset, int inputLen);
    int update(byte[] input, int inputOffset, int inputLen, byte[] output);
    int update(byte[] input, int inputOffset, int inputLen, byte[] output, int outputOffset);
    Cipher getInstance(int x, boolean y); 
*/

    public static final byte ALG_AES_BLOCK_128_CBC_NOPAD = 13;
    public static final byte ALG_AES_BLOCK_128_ECB_NOPAD = 14;
    public static final byte ALG_DES_CBC_ISO9797_M1 = 2;
    public static final byte ALG_DES_CBC_ISO9797_M2 = 3;
    public static final byte ALG_DES_CBC_NOPAD = 1;
    public static final byte ALG_DES_CBC_PKCS5 = 4;
    public static final byte ALG_DES_ECB_ISO9797_M1 = 6;
    public static final byte ALG_DES_ECB_ISO9797_M2 = 7;
    public static final byte ALG_DES_ECB_NOPAD = 5;
    public static final byte ALG_DES_ECB_PKCS5 = 8;
    public static final byte ALG_RSA_ISO14888 = 9;
    public static final byte ALG_RSA_ISO9796 = 11;
    public static final byte ALG_RSA_NOPAD = 12;
    public static final byte ALG_RSA_PKCS1 = 10;
    public static final byte ALG_RSA_PKCS1_OAEP = 15;
    public static final byte MODE_DECRYPT = 1;
    public static final byte MODE_ENCRYPT = 2;


    protected Cipher();
    // Protected constructor.

    abstract short doFinal(byte[] inBuff, short inOffset, short inLength, byte[] outBuff, short outOffset);
    // Generates encrypted/decrypted output from all/last input data.

    abstract byte getAlgorithm();
    // Gets the Cipher algorithm.

    static Cipher getInstance(byte algorithm, boolean externalAccess);
    // Creates a Cipher object instance of the selected algorithm.

    abstract void init(Key theKey, byte theMode);
    // Initializes the Cipher object with the appropriate Key.

    abstract void init(Key theKey, byte theMode, byte[] bArray, short bOff, short bLen);
    // Initializes the Cipher object with the appropriate Key and algorithm specific parameters.

    abstract  short 	update(byte[] inBuff, short inOffset, short inLength, byte[] outBuff, short outOffset);
    // Generates encrypted/decrypted output from input data.

}


   
