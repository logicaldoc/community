package com.logicaldoc.util;

import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

/**
 * Test cases for Encodings class
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class EncodingsTest {

	@Test
	public void testExtractTags() {
		List<String> encodings = Encodings.enlistSupportedEncodings();
		assertTrue(encodings.size() > 1);
	}
}