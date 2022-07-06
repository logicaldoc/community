package com.logicaldoc.util.io;

import java.io.IOException;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class FileUtilTest {

	@Before
	public void setUp() throws Exception {

	}

	@After
	public void tearDown() throws Exception {

	}

	@Test
	public void testMatch() throws IOException {
		Assert.assertTrue(FileUtil.matches("ReleaseNotes.txt", "*.doc,*.txt", ""));
	}
}