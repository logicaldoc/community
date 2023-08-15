package com.logicaldoc.util.io;

import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

public class FileUtilTest {

	@Test
	public void testMatch() throws IOException {
		Assert.assertTrue(FileUtil.matches("ReleaseNotes.txt", "*.doc,*.txt", ""));
	}

	@Test
	public void testGetName() throws IOException {
		String file = "/abc/def/pollo:test.pdf";
		Assert.assertEquals("pollo:test.pdf", FileUtil.getName(file));
	}

	@Test
	public void testGetBaseName() throws IOException {
		String file = "/abc/def/pollo:test.pdf";
		Assert.assertEquals("pollo:test", FileUtil.getBaseName(file));
	}

	@Test
	public void testGetExtension() throws IOException {
		String file = "/abc/def/pollo:test.pdf";
		Assert.assertEquals("pdf", FileUtil.getExtension(file));
	}

	@Test
	public void testGetPath() throws IOException {
		String file = "/abc/def/pollo:test.pdf";
		Assert.assertEquals("abc/def/", FileUtil.getPath(file));
	}

	@Test
	public void testReplaceInFile() throws IOException {
		File file = new File("target/test.txt");
		FileUtil.copyResource("/allowed-commands.txt", file);
		FileUtil.replaceInFile(file.getPath(), "test-classes", "XYZ");
		String content=FileUtil.readFile(file);
		Assert.assertTrue(content.contains("XYZ"));
		Assert.assertFalse(content.contains("test-classes"));
	}
}