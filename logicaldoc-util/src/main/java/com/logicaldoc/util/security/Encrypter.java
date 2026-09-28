package com.logicaldoc.util.security;

import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;

import javax.crypto.Cipher;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.SecretKeySpec;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.util.spring.Context;

/**
 * Utility class to encrypt/decrypt strings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class Encrypter {

    private Cipher cipher;

    private Cipher deCipher;

    private static Encrypter instance;

    public static Encrypter get() throws EncryptionException {
        if (instance == null)
            synchronized (Encrypter.class) {
                instance = new Encrypter();
            }
        return instance;
    }

    /**
     * A private constructor that takes the encryption key from the context
     * settings
     * 
     * @throws EncryptionException Error setting up the encryption machinery
     */
    private Encrypter() throws EncryptionException {
        this(Context.get().getConfig().getString("encryption.key"));
    }

    public Encrypter(String key) throws EncryptionException {
        try {
            SecretKeySpec keySpec = new SecretKeySpec(key.getBytes(StandardCharsets.UTF_8), "AES");

            cipher = Cipher.getInstance("AES");
            cipher.init(Cipher.ENCRYPT_MODE, keySpec);

            deCipher = Cipher.getInstance("AES");
            deCipher.init(Cipher.DECRYPT_MODE, keySpec);
        } catch (InvalidKeyException | NoSuchAlgorithmException | NoSuchPaddingException e) {
            throw new EncryptionException(e);
        }
    }

    public synchronized String encrypt(String unencryptedString) throws EncryptionException {
        if (StringUtils.isEmpty(unencryptedString))
            throw new IllegalArgumentException("unencrypted string was null or empty");
        try {
            byte[] cleartext = unencryptedString.getBytes(StandardCharsets.UTF_8);
            byte[] ciphertext = cipher.doFinal(cleartext);

            // Encode the output string in Base64
            return new String(java.util.Base64.getMimeEncoder().encode(ciphertext), StandardCharsets.UTF_8);
        } catch (Exception e) {
            throw new EncryptionException(e);
        }
    }

    public synchronized String decrypt(String encryptedString) throws EncryptionException {
        if (StringUtils.isEmpty(encryptedString))
            throw new IllegalArgumentException("encrypted string was null or empty");
        try {
            // Encode the inputed string in Base64
            byte[] encryptedtext = java.util.Base64.getMimeDecoder().decode(encryptedString);
            byte[] cleartext = deCipher.doFinal(encryptedtext);
            return bytes2String(cleartext);
        } catch (Exception e) {
            throw new EncryptionException(e);
        }
    }

    private static String bytes2String(byte[] bytes) {
        StringBuilder stringBuffer = new StringBuilder();
        for (int i = 0; i < bytes.length; i++)
            stringBuffer.append((char) bytes[i]);
        return stringBuffer.toString();
    }

    public static class EncryptionException extends Exception {
        private static final long serialVersionUID = 1L;

        public EncryptionException(Throwable t) {
            super(t);
        }
    }
}
