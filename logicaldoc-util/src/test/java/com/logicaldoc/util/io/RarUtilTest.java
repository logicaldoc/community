package com.logicaldoc.util.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class RarUtilTest {
	private File dir = new File("target/test");

	private File file = new File("target/test.rar");

	private RarUtil testSubject = new RarUtil();

	@Before
	public void setUp() throws IOException {
		dir.mkdirs();
		dir.mkdir();

		FileUtil.copyResource("kofax.rar", file);
	}

	@After
	public void tearDown() {
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
		testSubject.extractEntry(file, "kofax\\export\\invoice-001.PDF", test);
		assertTrue(test.exists());
	}
}