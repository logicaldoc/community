package com.logicaldoc.util.crypt;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import java.io.File;
import java.security.NoSuchAlgorithmException;

import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.crypt.CryptUtil.EncryptionException;
import com.logicaldoc.util.io.FileUtil;

import junit.framework.Assert;

public class CryptUtilTest {

	private CryptUtil testSubject;

	@Before
	public void setUp() throws EncryptionException {
		testSubject = new CryptUtil("awkljo4tir0qjug429mxiog9xtbjwre0hj3g6otbe6575454sg5rewg5rtegte78hgteh");
	}

	@Test
	public void testEncryptFile() throws EncryptionException {
		File clearFile = new File("src/main/resources/mimetypes.properties");
		File cryptedFile = new File("target/ctypt.crypted");
		File decryptedFile = new File("target/ctypt.decrypted");
		try {

			testSubject.encrypt(clearFile, cryptedFile);
			Assert.assertTrue(cryptedFile.exists());
			Assert.assertTrue(cryptedFile.length() > 0);
			Assert.assertNotSame(cryptedFile.length(), clearFile.length());

			testSubject.decrypt(cryptedFile, decryptedFile);
			Assert.assertTrue(decryptedFile.exists());
			Assert.assertEquals(clearFile.length(), decryptedFile.length());

		} finally {
			FileUtil.delete(cryptedFile);
			FileUtil.delete(decryptedFile);
		}
	}

	@Test
	public void testEncryptString() throws EncryptionException, NoSuchAlgorithmException {
		String encryptedString = testSubject.encrypt("pippo");
		assertNotSame("pippo", encryptedString);
		assertEquals("pippo", testSubject.decrypt(encryptedString));

		assertEquals("A2242EAD55C94C3DEB7CF2340BFEF9D5BCACA22DFE66E646745EE4371C633FC8",
				CryptUtil.encryptSHA256("pippo"));
	}

	@Test
	public void testHash() {
		assertEquals("1f544b64a50bdb31b2bd9d9ad96ad09d", CryptUtil.hashMD4("pippo"));
		assertEquals("0EBD3FEBDB972B9D9A164B72F321E341", CryptUtil.hashNTLM1("pippo"));
	}
}