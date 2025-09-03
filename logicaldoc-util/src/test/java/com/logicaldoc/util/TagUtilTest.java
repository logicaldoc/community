package com.logicaldoc.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Set;

import org.junit.Test;

/**
 * Test case for TagUtil class
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 4.5
 */
public class TagUtilTest {

	@Test
	public void testExtractTags() {
		Collection<String> coll = TagUtil.extractTags("default", "my name is tom");
		assertNotNull(coll);
		assertEquals(1, coll.size());

		coll = TagUtil.extractTags("default", "il mio,nome, e' tom");
		assertNotNull(coll);
		assertEquals(3, coll.size());

		coll = TagUtil.extractTags("default", "il mio, nome e' ,123456789123456789123456789");
		assertNotNull(coll);
		assertEquals(3, coll.size());
		assertFalse(coll.contains("123456789123456789123456789"));
		assertTrue(coll.contains("12345678912345678912"));
	}

	@Test
	public void testNormalizeTags() {
		String tenant = "default";

		// basic input
		String input1 = "il mio,nome, e' tom";
		Set<String> expected1 = TagUtil.extractTags(tenant, input1);
		String normalized1 = TagUtil.normalizeTags(tenant, input1);
		Set<String> actual1 = TagUtil.extractTags(tenant, normalized1);

		assertEquals(expected1.size(), actual1.size());
		assertTrue(actual1.containsAll(expected1));
		assertTrue(expected1.containsAll(actual1));

		// escaped comma
		String input2 = "alpha\\,beta,gamma";
		Set<String> expected2 = TagUtil.extractTags(tenant, input2);
		String normalized2 = TagUtil.normalizeTags(tenant, input2);
		Set<String> actual2 = TagUtil.extractTags(tenant, normalized2);

		assertEquals(expected2.size(), actual2.size());
		assertTrue(actual2.containsAll(expected2));
		assertTrue(expected2.containsAll(actual2));

		assertTrue(normalized2.contains("alpha\\,beta"));
	}
}