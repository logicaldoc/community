package com.logicaldoc.util.io;

import java.io.File;
import java.io.IOException;
import java.util.List;

import junit.framework.Assert;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ZipUtilTest {

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
			}
	}

	@Test
	public void testListEntries() throws IOException {
		File file = new File("target/test.zip");
		FileUtil.copyResource("/test.zip", file);

		ZipUtil zipUtil = new ZipUtil();
		List<String> entries = zipUtil.listEntries(file);

		Assert.assertEquals(3, entries.size());
		Assert.assertTrue(entries.contains("impex.xsd"));
	}

	@Test
	public void testGetEntryBytes() throws IOException {
		File file = new File("target/test.zip");
		FileUtil.copyResource("/test.zip", file);
		ZipUtil zipUtil = new ZipUtil();
		zipUtil.setFileNameCharset("UTF-8");
		byte[] in = zipUtil.getEntryBytes(file, "index.xml");
		Assert.assertEquals(132997526, in.length);
	}

	@Test
	public void testUmlauts() throws IOException {
		File file = new File("target/NeuesZip.zip");
		FileUtil.copyResource("/NeuesZip.zip", file);
		ZipUtil zipUtil = new ZipUtil();
		zipUtil.setFileNameCharset("utf-8");
		List<String> entries = zipUtil.listEntries(file);
		
		System.out.println(entries);
		
	}
}
