package com.logicaldoc.util.io;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class JarUtilTest {
	private File dir = new File("target/test");

	private File file = new File("target/test.zip");

	private JarUtil testSubject = new JarUtil();

	@Before
	public void setUp() throws IOException {
		dir.mkdirs();
		dir.mkdir();

		FileUtil.copyResource("test.zip", file);
	}

	@After
	public void tearDown() {
		FileUtil.delete(dir);
	}

	@Test
	public void testUnjar() throws IOException {
		File test = new File(dir, "index.xml");
		try {
			assertFalse(test.exists());
			testSubject.unjar(file.getPath(), "target/test");
			assertTrue(test.exists());
		} finally {
			FileUtil.delete(test);
		}
	}

	@Test
	public void testUnjarEntry() throws IOException {
		final File test = new File(dir, "test.txt");
		assertFalse(test.exists());
		testSubject.unjar(file.getPath(), "abc/test.txt", test.getPath());
		assertTrue(test.exists());

	}
}