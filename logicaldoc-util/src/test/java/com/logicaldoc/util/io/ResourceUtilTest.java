package com.logicaldoc.util.io;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

public class ResourceUtilTest {

	@Test
	public void testExistsResource() throws IOException {
		Assert.assertTrue(ResourceUtil.existsResource("/context.xml"));
		Assert.assertFalse(ResourceUtil.existsResource("/test.txt"));
	}

	@Test
	public void testCopyResource() throws IOException {
		File out = new File("target/out.txt");
		try {
			assertFalse(out.exists());
			ResourceUtil.copyResource("/context.xml", out);
			assertTrue(out.length() > 0);
		} finally {
			FileUtil.strongDelete(out);
		}
	}

	@Test
	public void testReadAsString() throws IOException {
		assertTrue(ResourceUtil.readAsString("/context.xml").contains("<beans default-lazy-init"));
	}

	@Test
	public void testReadAsBytes() throws IOException {
		File out = new File("target/out.txt");
		try {
			assertFalse(out.exists());
			IOUtil.write(new ByteArrayInputStream(ResourceUtil.readAsBytes("/context.xml")), out);
			assertTrue(out.length() > 0);
		} finally {
			FileUtil.strongDelete(out);
		}
	}
}