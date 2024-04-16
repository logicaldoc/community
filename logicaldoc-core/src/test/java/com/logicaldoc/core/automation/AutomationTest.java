package com.logicaldoc.core.automation;

import static org.junit.Assert.assertEquals;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTestCase;

/**
 * Test case for the <code>MailUtil</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class AutomationTest extends AbstractCoreTestCase {

	@Test
	public void testAutomation() throws Exception {
		Map<String, Object> dict = new HashMap<>();
		dict.put("testval", "abc");
		
		Automation automation = new Automation();
		String output = automation.evaluate("pippo $testval", dict);

		assertEquals("pippo abc", output);
	}
}