package com.logicaldoc.core.automation;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.logicaldoc.core.AbstractCoreTCase;

import junit.framework.Assert;

/**
 * Test case for the <code>MailUtil</code>
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class AutomationTest extends AbstractCoreTCase {

	@Test
	public void testAutomation() throws Exception {
		{
			Map<String, Object> dict=new HashMap<String, Object>();
			dict.put("testval","abc");
			
			Automation automation = new Automation();
			String output = automation.evaluate("pippo $testval", dict);
			
			Assert.assertEquals("pippo abc", output);
		}
	}
}