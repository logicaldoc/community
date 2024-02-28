package com.logicaldoc.util.io;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.junit.Test;

public class TarUtilTest {

	private TarUtil testSubject = new TarUtil();

	@Test
	public void testListEntries() throws IOException {
		List<String> entries = testSubject.listEntries(new File("src/test/resources/kofax.tar"));
		assertEquals(22, entries.size());
	}

	@Test
	public void testExtractEntry() throws IOException {
		File out = new File("target/tarentry.xml");
		try {
			testSubject.extractEntry(new File("src/test/resources/kofax.tar"), out);
			assertTrue(out.exists());
			assertTrue(FileUtil.readFile(out).contains("<BatchHeader>"));
		} finally {
			FileUtil.strongDelete(out);
		}
	}
}