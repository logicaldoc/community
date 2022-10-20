package com.logicaldoc.util.io;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import junit.framework.Assert;

public class JarUtilTest {
	@Before
	public void setUp() throws Exception {
		File dir = new File("target/test");
		dir.mkdirs();
		dir.mkdir();
	}

	@After
	public void tearDown() throws Exception {
		File dir = new File("target/test");
		if (dir.exists())
			try {
				FileUtils.forceDelete(dir);
			} catch (IOException e) {
				// Nothing to do
			}
	}

	@Test
	public void testUnjar() throws IOException {
		File file = new File("target/test.zip");
		FileUtil.copyResource("/test.zip", file);

		Assert.assertFalse(new File("target/test/index.xml").exists());

		Assert.assertTrue(new JarUtil().unjar(file.getPath(), "target/test"));

		Assert.assertTrue(new File("target/test/index.xml").exists());
	}
	
	@Test
	public void testSaveEntry() throws IOException {
		File file = new File("target/test.zip");
		FileUtil.copyResource("/test.zip", file);

		Assert.assertFalse(new File("target/test/index.xml").exists());

		Assert.assertTrue(new JarUtil().unjar(file.getPath(), "index.xml", "target/test/index.xml"));

		Assert.assertTrue(new File("target/test/index.xml").exists());
	}
}
