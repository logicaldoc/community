package com.logicaldoc.util.crypt;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.MessageDigest;
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
import org.bouncycastle.crypto.digests.MD4Digest;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;

public class CryptUtil {
	private static Logger log = LoggerFactory.getLogger(CryptUtil.class);

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
				throw new IllegalArgumentException("Encryption scheme not supported: " + encryptionScheme);
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
			FileUtil.strongDelete(outputFile);
			boolean created = outputFile.createNewFile();
			if (!created)
				throw new IOException("Cannot create file " + outputFile.getAbsolutePath());
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
			FileUtil.strongDelete(outputFile);
			boolean created = outputFile.createNewFile();
			if (!created)
				throw new IOException("Cannot create file " + outputFile.getAbsolutePath());
			FileUtils.writeByteArrayToFile(outputFile, clearContent);
		} catch (InvalidKeyException | InvalidKeySpecException | IllegalBlockSizeException | BadPaddingException
				| IOException e) {
			throw new EncryptionException(e);
		}
	}

	public String encrypt(String unencryptedString) throws EncryptionException {
		if (unencryptedString == null || unencryptedString.trim().length() == 0)
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
		if (encryptedString == null || encryptedString.trim().length() <= 0)
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

	/**
	 * This method encodes a given string using the SHA algorithm. This method
	 * will be dismissed in the future.
	 * 
	 * @param original String to encode
	 * 
	 * @return Encoded string
	 * 
	 * @throws NoSuchAlgorithmException Cripting exception
	 */
	public static String cryptStringLegacy(String original) throws NoSuchAlgorithmException {
		StringBuilder copy = new StringBuilder();

		MessageDigest md = MessageDigest.getInstance("SHA");
		byte[] digest = md.digest(original.getBytes(StandardCharsets.UTF_8));

		for (int i = 0; i < digest.length; i++) {
			copy.append(Integer.toHexString(digest[i] & 0xFF));
		}

		return copy.toString();
	}

	/**
	 * This method encodes a given string using the SHA-256 algorithm
	 * 
	 * @param original String to encode
	 * 
	 * @return Encoded string
	 * 
	 * @throws NoSuchAlgorithmException Cripting exception
	 */
	public static String cryptString(String original) throws NoSuchAlgorithmException {
		StringBuilder copy = new StringBuilder();

		MessageDigest md = MessageDigest.getInstance("SHA-256");
		byte[] digest = md.digest(original.getBytes(StandardCharsets.UTF_8));

		for (int i = 0; i < digest.length; i++) {
			copy.append(String.format("%02X", digest[i]));
		}

		return copy.toString();
	}

	/**
	 * Converts a string into a MD4 hash.
	 * 
	 * @param original the original string to be encrypted.
	 * @return the returned hash as bytes.
	 */
	public static String hashMD4(String original) {
		if (original == null)
			original = "";

		String copy = "";
		try {
			MD4Digest md4 = new MD4Digest();
			byte[] pwdBytes = original.getBytes();
			md4.update(pwdBytes, 0, pwdBytes.length);
			byte[] encPwd = new byte[md4.getDigestSize()];
			md4.doFinal(encPwd, 0);
			copy = getHex(encPwd).toLowerCase();
		} catch (Exception nsae) {
			log.error(nsae.getMessage());
		}
		return copy;
	}

	/**
	 * Converts a string into a MD4 hash suitable for the NTLM v1 authentication
	 * 
	 * @param original the original string
	 * 
	 * @return the MD4 hash
	 */
	public static String hashNTLM1(String original) {

		try {
			if (original == null) {
				original = "";
			}
			MD4Digest md4 = new MD4Digest();
			int len = original.length();
			byte[] pwdBytes = new byte[len * 2];

			for (int i = 0; i < len; i++) {
				char ch = original.charAt(i);
				pwdBytes[i * 2] = (byte) ch;
				pwdBytes[i * 2 + 1] = (byte) ((ch >> 8) & 0xFF);
			}

			md4.update(pwdBytes, 0, pwdBytes.length);
			byte[] encPwd = new byte[16];
			md4.doFinal(encPwd, 0);

			return CryptUtil.getHex(encPwd).substring(0, 32);
		} catch (Exception e) {
			log.error(e.getMessage());
			return null;
		}

	}

	public static String getHex(byte[] raw) {
		String hexes = "0123456789ABCDEF";
		if (raw == null) {
			return null;
		}
		final StringBuilder hex = new StringBuilder(2 * raw.length);
		for (final byte b : raw) {
			hex.append(hexes.charAt((b & 0xF0) >> 4)).append(hexes.charAt((b & 0x0F)));
		}
		return hex.toString();
	}

	public static class EncryptionException extends Exception {
		private static final long serialVersionUID = 1L;

		public EncryptionException(Throwable t) {
			super(t);
		}
	}
}