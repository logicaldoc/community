package com.logicaldoc.util.io;

import java.io.IOException;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class ResourceUtilTest {

	@Before
	public void setUp() throws Exception {

	}

	@After
	public void tearDown() throws Exception {

	}

	@Test
	public void testExistsResource() throws IOException {
		Assert.assertTrue(ResourceUtil.existsResource("/context.xml"));
		Assert.assertFalse(ResourceUtil.existsResource("/test.txt"));
	}
}