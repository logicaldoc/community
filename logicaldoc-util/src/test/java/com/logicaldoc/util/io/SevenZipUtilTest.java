package com.logicaldoc.util.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.logicaldoc.util.plugin.PluginException;

public class SevenZipUtilTest {
	private File dir = new File("target/test");

	private File file = new File("target/test.7z");

	private SevenZipUtil testSubject = new SevenZipUtil();

	@Before
	public void setUp() throws IOException, SQLException, PluginException {
		File dir = new File("target/test");
		dir.mkdirs();
		dir.mkdir();

		FileUtil.copyResource("/kofax.7z", file);
	}

	@After
	public void tearDown() throws Exception {
		FileUtil.delete(dir);
	}

	@Test
	public void testlistEntries() throws IOException {
		assertEquals(22, testSubject.listEntries(file).size());
	}

	@Test
	public void testExtractEntry() throws IOException {
		final File test = new File(dir, "invoice.pdf");
		assertFalse(test.exists());
		testSubject.extractEntry(file, "kofax/scans/invoice-001.PDF", test);
		assertTrue(test.exists());
	}
}