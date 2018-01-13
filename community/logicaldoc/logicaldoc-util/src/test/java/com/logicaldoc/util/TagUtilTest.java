package com.logicaldoc.util;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;

import org.junit.Test;

import com.logicaldoc.util.TagUtil;

/**
 * Test case for TagUtil class
 * 
 * @author Marco Meschieri - Logical Objects
 * @since 4.5
 */
public class TagUtilTest {

	@Test
	public void testExtractTags() {
		Collection<String> coll = TagUtil.extractTags("default","my name is tom");
		assertNotNull(coll);
		assertEquals(1, coll.size());

		coll = TagUtil.extractTags("default","il mio,nome, e' tom");
		assertNotNull(coll);
		assertEquals(3, coll.size());

		coll = TagUtil.extractTags("default","il mio, nome e' ,123456789123456789123456789");
		assertNotNull(coll);
		assertEquals(3, coll.size());
		assertFalse(coll.contains("123456789123456789123456789"));
		assertTrue(coll.contains("12345678912345678912"));
	}
}