package com.logicaldoc.util.crypt;

import java.io.File;
import java.io.IOException;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.crypt.CryptUtil.EncryptionException;
import com.logicaldoc.util.io.FileUtil;

public class CryptUtilTest {

	@Before
	public void setUp() throws Exception {

	}

	@After
	public void tearDown() throws Exception {

	}

	@Test
	public void testCrypt() throws IOException, EncryptionException {
		File clearFile = new File("src/main/resources/mimetypes.properties");
		File cryptedFile = new File("target/ctypt.crypted");
		CryptUtil crypt=new CryptUtil("awkljo4tir0qjug429mxiog9xtbjwre0hj3g6otbe6575454sg5rewg5rtegte78hgteh");
		crypt.encrypt(clearFile, cryptedFile);
		Assert.assertTrue(cryptedFile.exists());
		Assert.assertTrue(cryptedFile.length()>0);
		Assert.assertTrue(cryptedFile.length()!=clearFile.length());
		
		File decryptedFile = new File("target/ctypt.decrypted");
		crypt.decrypt(cryptedFile, decryptedFile);
		Assert.assertTrue(decryptedFile.exists());
		Assert.assertEquals(clearFile.length(), decryptedFile.length());
		
		FileUtil.strongDelete(cryptedFile);
		FileUtil.strongDelete(decryptedFile);
	}
}