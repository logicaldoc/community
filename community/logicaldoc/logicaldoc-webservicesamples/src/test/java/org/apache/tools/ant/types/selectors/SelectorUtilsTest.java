package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.*;

import org.apache.tools.ant.types.selectors.SelectorUtils;
import org.junit.Test;

public class SelectorUtilsTest {

	@Test
	public void testMatchStringString() {
		boolean xxx = SelectorUtils.match("FA*", "FA234156215");
		assertTrue(xxx);
		xxx = SelectorUtils.match("????????", "FT412574");
		assertTrue(xxx);
		xxx = SelectorUtils.match("fa*", "FT412574"); // it is case sensitive
		assertFalse(xxx);		
	}

}
