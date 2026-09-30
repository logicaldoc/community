package com.logicaldoc.util.crypt;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.DESKeySpec;
import javax.crypto.spec.DESedeKeySpec;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.StringUtils;

import com.logicaldoc.util.io.FileUtil;

public class CryptUtil {

    public static final String DESEDE_ENCRYPTION_SCHEME = "DESede";

    public static final String DES_ENCRYPTION_SCHEME = "DES";

    public static final String DEFAULT_ENCRYPTION_KEY = "This is a fairly long phrase used to encrypt";

    private KeySpec keySpec;

    private SecretKeyFactory keyFactory;

    private Cipher cipher;

    public CryptUtil(String encryptionKey) throws EncryptionException {
        this(DES_ENCRYPTION_SCHEME, encryptionKey);
    }

    public CryptUtil(String encryptionScheme, String encryptionKey) throws EncryptionException {
        if (encryptionKey == null)
            throw new IllegalArgumentException("encryption key was null");

        try {
            String key = encryptionKey;
            if (encryptionKey.length() < 32)
                key = StringUtils.rightPad(encryptionKey, 32, '*');
            byte[] keyAsBytes = key.getBytes(StandardCharsets.UTF_8);
            if (encryptionScheme.equals(DESEDE_ENCRYPTION_SCHEME)) {
                keySpec = new DESedeKeySpec(keyAsBytes);
            } else if (encryptionScheme.equals(DES_ENCRYPTION_SCHEME)) {
                keySpec = new DESKeySpec(keyAsBytes);
            } else {
                throw new IllegalArgumentException("Encryption scheme not supported: %s".formatted(encryptionScheme));
            }
            keyFactory = SecretKeyFactory.getInstance(encryptionScheme);
            cipher = Cipher.getInstance(encryptionScheme);
        } catch (InvalidKeyException | NoSuchAlgorithmException | NoSuchPaddingException e) {
            throw new EncryptionException(e);
        }
    }

    public void encrypt(File inputFile, File outputFile) throws EncryptionException {
        if (inputFile == null || !inputFile.exists())
            throw new IllegalArgumentException("Unencrypted file not found in inpout file");

        try {
            SecretKey key = keyFactory.generateSecret(keySpec);
            cipher.init(Cipher.ENCRYPT_MODE, key);
            byte[] clearContent = FileUtils.readFileToByteArray(inputFile);
            byte[] encryptedContent = cipher.doFinal(clearContent);
            outputFile.mkdirs();
            FileUtil.delete(outputFile);
            boolean created = outputFile.createNewFile();
            if (!created)
                throw new IOException("Cannot create file %s".formatted(outputFile.getAbsolutePath()));
            FileUtils.writeByteArrayToFile(outputFile, encryptedContent);
        } catch (InvalidKeyException | InvalidKeySpecException | IllegalBlockSizeException | BadPaddingException
                | IOException e) {
            throw new EncryptionException(e);
        }
    }

    public void decrypt(File inputFile, File outputFile) throws EncryptionException {

        try {
            if (inputFile == null || !inputFile.exists())
                throw new IllegalArgumentException("Encrypted file not found in input file");
            SecretKey key = keyFactory.generateSecret(keySpec);
            cipher.init(Cipher.DECRYPT_MODE, key);
            byte[] encryptedContent = FileUtils.readFileToByteArray(inputFile);
            byte[] clearContent = cipher.doFinal(encryptedContent);
            outputFile.mkdirs();
            FileUtil.delete(outputFile);
            boolean created = outputFile.createNewFile();
            if (!created)
                throw new IOException("Cannot create file %s".formatted(outputFile.getAbsolutePath()));
            FileUtils.writeByteArrayToFile(outputFile, clearContent);
        } catch (InvalidKeyException | InvalidKeySpecException | IllegalBlockSizeException | BadPaddingException
                | IOException e) {
            throw new EncryptionException(e);
        }
    }

    public String encrypt(String unencryptedString) throws EncryptionException {
        if (StringUtils.isEmpty(unencryptedString))
            throw new IllegalArgumentException("unencrypted string was null or empty");

        try {
            SecretKey key = keyFactory.generateSecret(keySpec);
            cipher.init(Cipher.ENCRYPT_MODE, key);
            byte[] cleartext = unencryptedString.getBytes(StandardCharsets.UTF_8);
            byte[] ciphertext = cipher.doFinal(cleartext);
            return new String(java.util.Base64.getMimeEncoder().encode(ciphertext), StandardCharsets.UTF_8);
        } catch (InvalidKeyException | InvalidKeySpecException | IllegalBlockSizeException | BadPaddingException e) {
            throw new EncryptionException(e);
        }
    }

    public String decrypt(String encryptedString) throws EncryptionException {
        if (StringUtils.isEmpty(encryptedString))
            throw new IllegalArgumentException("encrypted string was null or empty");

        try {
            SecretKey key = keyFactory.generateSecret(keySpec);
            cipher.init(Cipher.DECRYPT_MODE, key);
            byte[] cleartext = java.util.Base64.getMimeDecoder().decode(encryptedString);
            byte[] ciphertext = cipher.doFinal(cleartext);
            return bytes2String(ciphertext);
        } catch (InvalidKeyException | InvalidKeySpecException | IllegalBlockSizeException | BadPaddingException e) {
            throw new EncryptionException(e);
        }
    }

    private static String bytes2String(byte[] bytes) {
        StringBuilder stringBuffer = new StringBuilder();
        for (int i = 0; i < bytes.length; i++) {
            stringBuffer.append((char) bytes[i]);
        }
        return stringBuffer.toString();
    }

    public static class EncryptionException extends Exception {
        private static final long serialVersionUID = 1L;

        public EncryptionException(Throwable t) {
            super(t);
        }
    }
}