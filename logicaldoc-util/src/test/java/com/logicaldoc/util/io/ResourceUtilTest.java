package com.logicaldoc.util.io;

import java.io.IOException;

import org.junit.Assert;
import org.junit.Test;

public class ResourceUtilTest {

	@Test
	public void testExistsResource() throws IOException {
		Assert.assertTrue(ResourceUtil.existsResource("/context.xml"));
		Assert.assertFalse(ResourceUtil.existsResource("/test.txt"));
	}
}