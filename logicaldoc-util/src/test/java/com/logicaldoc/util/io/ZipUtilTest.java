package com.logicaldoc.util.io;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.zip.ZipEntry;

import org.apache.commons.io.FileUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import junit.framework.Assert;

public class ZipUtilTest {
	private static Logger log = LoggerFactory.getLogger(ZipUtilTest.class);

	@Before
	public void setUp() throws FileNotFoundException, IOException, SQLException {
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
	public void testListZipEntries() throws IOException {
		File file = new File("target/test.zip");
		FileUtil.copyResource("/test.zip", file);

		ZipUtil zipUtil = new ZipUtil();
		List<ZipEntry> entries = zipUtil.listZipEntries(file);

		for (ZipEntry zipEntry : entries) {
			System.out.println(zipEntry.getName() + " " + zipEntry.getSize() + " " + zipEntry.getLastModifiedTime());
		}

		Assert.assertEquals(5, entries.size());
	}

	@Test
	public void testUnzip() throws IOException {
		File file = new File("target/test.zip");
		FileUtil.copyResource("/test.zip", file);

		Assert.assertFalse(new File("target/test/index.xml").exists());

		ZipUtil zipUtil = new ZipUtil();
		zipUtil.unzip(file.getPath(), "target/test");

		Assert.assertTrue(new File("target/test/index.xml").exists());
		Assert.assertTrue(new File("target/test/abc/test.txt").exists());
	}

	@Test
	public void testListEntries() throws IOException {
		File file = new File("target/test.zip");
		FileUtil.copyResource("/test.zip", file);

		ZipUtil zipUtil = new ZipUtil();
		List<String> entries = zipUtil.listEntries(file);

		Assert.assertEquals(5, entries.size());
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
	public void testUmlauts() {
		String notThrownTest = null;
		try {
			File file = new File("target/NeuesZip.zip");
			FileUtil.copyResource("/NeuesZip.zip", file);
			ZipUtil zipUtil = new ZipUtil();
			zipUtil.setFileNameCharset("utf-8");
			List<String> entries = zipUtil.listEntries(file);
			log.debug("Found {} entries", entries.size());
			notThrownTest = "ok";
		} catch (Exception t) {
			// Nothing to do

		}
		Assert.assertNotNull(notThrownTest);

	}
}
