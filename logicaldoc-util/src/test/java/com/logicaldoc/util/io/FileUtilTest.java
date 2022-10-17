package com.logicaldoc.util.io;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

public class FileUtilTest {

	@Test
	public void testMatch() throws IOException {
		Assert.assertTrue(FileUtil.matches("ReleaseNotes.txt", "*.doc,*.txt", ""));
	}
}