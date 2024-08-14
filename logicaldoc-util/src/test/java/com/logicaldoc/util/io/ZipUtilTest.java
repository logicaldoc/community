package com.logicaldoc.util.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
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

import com.logicaldoc.util.plugin.PluginException;

public class ZipUtilTest {
	private static Logger log = LoggerFactory.getLogger(ZipUtilTest.class);

	private File folder = new File("target/test");

	private File file = new File("target/test.zip");

	private ZipUtil testSubject = new ZipUtil();

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		folder.mkdir();
		FileUtil.copyResource("/test.zip", file);
	}

	@After
	public void tearDown() {
		try {
			FileUtils.forceDelete(file);
			FileUtils.forceDelete(folder);
		} catch (IOException e) {
			// Ignore
		}
	}

	@Test
	public void testListZipEntries() throws IOException {
		List<ZipEntry> entries = testSubject.listZipEntries(file);
		assertEquals(5, entries.size());
	}

	@Test
	public void testUnzip() throws IOException {
		File indexXml = new File("target/test/index.xml");
		File test = new File("target/text.txt");

		try {
			assertFalse(indexXml.exists());

			testSubject.unzip(file, folder);

			assertTrue(new File(folder, "index.xml").exists());
			assertTrue(new File(folder.getPath() + "/abc/test.txt").exists());

			testSubject.unzip(this.getClass().getResourceAsStream("/test.zip"), "abc/test.txt", test);
			assertTrue(test.exists());
			FileUtil.delete(test);

			testSubject.unzip(new File("src/test/resources/test.zip"), "/abc/test.txt", test);
			assertTrue(test.exists());
		} finally {
			FileUtil.delete(test);
			FileUtil.delete(indexXml);
		}
	}

	@Test
	public void testListEntries() throws IOException {
		List<String> entries = testSubject.listEntries(file);
		assertEquals(5, entries.size());
		assertTrue(entries.contains("impex.xsd"));
	}

	@Test
	public void testGetEntryBytes() throws IOException {
		testSubject.setFileNameCharset("UTF-8");
		byte[] in = testSubject.getEntryBytes(file, "/index.xml");
		assertEquals(132997526, in.length);
	}

	@Test
	public void testUmlauts() {
		String notThrownTest = null;
		try {
			File file = new File("target/NeuesZip.zip");
			FileUtil.copyResource("/NeuesZip.zip", file);
			testSubject.setFileNameCharset("utf-8");
			List<String> entries = testSubject.listEntries(file);
			log.debug("Found {} entries", entries.size());
			notThrownTest = "ok";
		} catch (Exception t) {
			// Nothing to do
		}
		assertNotNull(notThrownTest);
	}

	@Test
	public void testGetEntryContent() throws IOException {
		String content = testSubject.getEntryContent(file, "abc/test.txt");
		assertEquals("abc", content);
	}

	@Test
	public void testAddEntry() throws IOException {
		try {
			testSubject.getEntryContent(file, "abc/pippo.txt");
			fail("No error getting content of unexisting entry?");
		} catch (NullPointerException npe) {
			// All ok
		}
		ZipUtil.addEntry(file, "abc/pippo.txt", this.getClass().getResourceAsStream("/context.properties"));
		assertNotNull(testSubject.getEntryContent(file, "abc/pippo.txt"));
	}

	@Test
	public void testZipFolder() throws IOException {
		testSubject.zipFolder(new File("src/test/resources"), file);
		assertNotNull(testSubject.getEntryContent(file, "/log.xml"));
	}

	@Test
	public void testZipFile() throws IOException {
		File context = new File("target/context.zip");
		try {
			testSubject.zipFile(new File("src/test/resources/context.properties"), context);
			assertNotNull(testSubject.getEntryContent(context, "/context.properties"));
		} finally {
			FileUtil.delete(context);
		}
	}

	@Test
	public void testUnGZipUnTar() throws IOException {
		testSubject.unGZipUnTar(new File("src/test/resources/kofax2.tgz"), folder);
		assertTrue(new File("target/test/kofax/scans/invoice-001.PDF").exists());
	}
}