package com.logicaldoc.util.crypt;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.util.Base64;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

import com.logicaldoc.util.config.ContextProperties;
import com.logicaldoc.util.spring.Context;

/**
 * Utility class to encrypt/decrypt strings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class Encrypter {

    private static final int PBKDF2_ITERATIONS = 310_000;

    private static final int KEY_LENGTH = 256; // AES-256

    private static final int SALT_LENGTH = 16;

    private static final int IV_LENGTH = 12;

    private static volatile Encrypter instance;

    private final String encryptionKey;

    private static final SecureRandom RANDOM = new SecureRandom();

    /**
     * Singleton factory method
     * 
     * @return the instance
     * 
     * @throws EncryptionException error retrieving the encryption key
     */
    public static Encrypter get() throws EncryptionException {
        if (instance == null) {
            synchronized (Encrypter.class) {
                if (instance == null)
                    try {
                        instance = new Encrypter();
                    } catch (Exception e) {
                        throw new EncryptionException(e);
                    }
            }
        }
        return instance;
    }

    /**
     * A private constructor that takes the encryption key from the context
     * settings
     * 
     * @throws IOException error retrieving the encryption key
     */
    private Encrypter() throws IOException {
        this(Context.get() != null ? Context.get().getConfig().getString("encryption.key")
                : new ContextProperties().getString("encryption.key"));
    }

    /**
     * A public constructor that instantiates this class with a given encryption
     * key
     * 
     * @param encryptionKey The encryption key to use
     */
    public Encrypter(String encryptionKey) {
        this.encryptionKey = encryptionKey;
    }

    /**
     * Encrypts a text
     * 
     * @param plaintext The text in clear form
     * 
     * @return The encrypted content
     * 
     * @throws EncryptionException Error in the encryption machinery
     */
    public String encrypt(String plaintext) throws EncryptionException {
        PBEKeySpec spec = null;
        try {
            // Generate salt
            byte[] salt = new byte[SALT_LENGTH];
            RANDOM.nextBytes(salt);

            // Derive AES key from LogicalDOC encryption key
            SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
            spec = new PBEKeySpec(encryptionKey.toCharArray(), salt, PBKDF2_ITERATIONS, KEY_LENGTH);
            SecretKey tmp = factory.generateSecret(spec);
            SecretKeySpec keySpec = new SecretKeySpec(tmp.getEncoded(), "AES");

            // Generate IV
            byte[] iv = new byte[IV_LENGTH];
            RANDOM.nextBytes(iv);

            // AES-GCM encrypt
            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
            GCMParameterSpec gcmSpec = new GCMParameterSpec(128, iv);
            cipher.init(Cipher.ENCRYPT_MODE, keySpec, gcmSpec);

            byte[] ciphertext = cipher.doFinal(plaintext.getBytes(StandardCharsets.UTF_8));

            // Output format: Base64(salt) | Base64(iv) | Base64(ciphertext)
            return "%s:%s:%s".formatted(Base64.getEncoder().encodeToString(salt),
                    Base64.getEncoder().encodeToString(iv), Base64.getEncoder().encodeToString(ciphertext));
        } catch (InvalidKeyException | NoSuchAlgorithmException | InvalidKeySpecException | NoSuchPaddingException
                | InvalidAlgorithmParameterException | IllegalBlockSizeException | BadPaddingException e) {
            throw new EncryptionException(e);
        } finally {
            // Clear password from memory
            if (spec != null)
                spec.clearPassword();
        }
    }

    /**
     * Decrypts an encoded content
     * 
     * @param encoded The encoded string
     * 
     * @return The original clear text
     * 
     * @throws EncryptionException Error in the decryption machinery
     */
    public String decrypt(String encoded) throws EncryptionException {
        PBEKeySpec spec = null;
        try {
            // Split salt | iv | ciphertext
            String[] parts = encoded.split(":");
            if (parts.length != 3)
                throw new IllegalArgumentException("Invalid encrypted format");

            byte[] salt = Base64.getDecoder().decode(parts[0]);
            byte[] iv = Base64.getDecoder().decode(parts[1]);
            byte[] ciphertext = Base64.getDecoder().decode(parts[2]);

            // Derive AES key from LogicalDOC password (same salt!)
            SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
            spec = new PBEKeySpec(encryptionKey.toCharArray(), salt, PBKDF2_ITERATIONS, KEY_LENGTH);
            SecretKey tmp = factory.generateSecret(spec);
            SecretKeySpec keySpec = new SecretKeySpec(tmp.getEncoded(), "AES");

            // AES-GCM decrypt
            Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
            GCMParameterSpec gcmSpec = new GCMParameterSpec(128, iv);
            cipher.init(Cipher.DECRYPT_MODE, keySpec, gcmSpec);

            byte[] plaintext = cipher.doFinal(ciphertext);

            return new String(plaintext, StandardCharsets.UTF_8);
        } catch (InvalidKeyException | NoSuchAlgorithmException | InvalidKeySpecException | NoSuchPaddingException
                | InvalidAlgorithmParameterException | IllegalBlockSizeException | BadPaddingException
                | IllegalArgumentException e) {
            throw new EncryptionException(e);
        } finally {
            // Clear password from memory
            if (spec != null)
                spec.clearPassword();
        }
    }

    /**
     * This method encodes a given string using the SHA-256 algorithm
     * 
     * @param original String to encode
     * 
     * @return Encoded string
     * 
     * @throws NoSuchAlgorithmException encrypting exception
     */
    public static String encodeSHA256(String original) throws NoSuchAlgorithmException {
        StringBuilder copy = new StringBuilder();

        MessageDigest md = MessageDigest.getInstance("SHA-256");
        byte[] digest = md.digest(original.getBytes(StandardCharsets.UTF_8));

        for (int i = 0; i < digest.length; i++) {
            copy.append(String.format("%02X", digest[i]));
        }

        return copy.toString();
    }

    public static class EncryptionException extends Exception {
        private static final long serialVersionUID = 1L;

        public EncryptionException(Throwable t) {
            super(t);
        }
    }
}
