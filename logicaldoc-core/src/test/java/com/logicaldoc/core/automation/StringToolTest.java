package com.logicaldoc.core.automation;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class StringToolTest {

	// instance under test
	private StringTool testSubject = new StringTool();

	@Test
	public void testDefaultString() {
		assertEquals("test", testSubject.defaultString("test"));
		assertEquals("", testSubject.defaultString(null));

		assertEquals("test", testSubject.defaultString("test", "abc"));
		assertEquals("abc", testSubject.defaultString("", "abc"));
		assertEquals("abc", testSubject.defaultString(null, "abc"));
	}
}